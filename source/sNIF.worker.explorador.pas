unit sNIF.worker.explorador;

{ *******************************************************

  sNIF

  Utilidad para buscar ficheros ofimáticos con
  cadenas de caracteres coincidentes con NIF/NIE.

  *******************************************************

  2012-2018 Ángel Fernández Pineda. Madrid. Spain.

  This work is licensed under the Creative Commons
  Attribution-ShareAlike 3.0 Unported License. To
  view a copy of this license,
  visit http://creativecommons.org/licenses/by-sa/3.0/
  or send a letter to Creative Commons,
  444 Castro Street, Suite 900,
  Mountain View, California, 94041, USA.

  ******************************************************* }

interface

uses
  Classes,
  CSVUtils,
  sNIF.data,
  sNIF.params,
  sNIF.worker,
  sNIF.control;

type
  TWorkerExplorador = class(TsNIFWorker)
    {
      PROPOSITO:

      Toma un fichero de la cola de trabajo y busca cadenas coincidentes de
      NIF/NIE. Luego escribe el resultado de la exploración.

      NOTA:

      No se procesan los últimos 17 bytes de un fichero porque no cabe un
      NIF en formato UTF-16, aunque podrían contener un NIF en formato
      UTF-8.
    }
  private
    Buffer: PByte;
    csvFields: TCSVFields;
  protected
    procedure Execute; override;
    procedure procesarCandidato(const candidato: TsNIFdata);
  public
    constructor Create(const AControl: TsNIFControl;
      const Parametros: TsNIFParametros);
    destructor Destroy; override;
  end;

implementation

uses
  IOUtils,
  IOUtilsFix,
  System.StrUtils,
  System.SysUtils,
  System.DateUtils,
  windows,
  NIFUtils,
  SyncObjs,
  ThreadUtils.SafeDataTypes;

const
  BUFFER_SIZE = 64 * 1024; // tamaño testado para mejor rendimiento
  BYTES_PER_NIF_UTF16 = 18;
  BYTES_PER_NIF_UTF8 = 9;

  // --------------------------------------------------------------------------
  // Hilo de trabajo
  // --------------------------------------------------------------------------

procedure TWorkerExplorador.Execute;
var
  candidato: TsNIFdata;
  handle: THANDLE;
  accessAttr: FILETIME;
  preservarAttr: boolean;
  fichero: string;
begin
  while (not Terminated) do
    try
      // Sacar un fichero de la cola de trabajo
      candidato := FControl.ColaCandidatos.Dequeue;
      fichero := TPath.Combine(candidato.Ruta, candidato.fichero);
      fichero := TPath.SetExtendedPrefix(fichero);

      // Leer tiempo de ultimo acceso para intentar preservarlo
      handle := CreateFile(PWideChar(fichero), GENERIC_READ,
        (FILE_SHARE_READ or FILE_SHARE_WRITE), nil, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL, 0);
      if (handle <> INVALID_HANDLE_VALUE) then
        preservarAttr := GetFileTime(handle, nil, @accessAttr, nil)
      else
        preservarAttr := false;
      CloseHandle(handle);

      // procesar fichero
      // (altera el atributo de fecha de ultimo acceso)
      procesarCandidato(candidato);

      // Restaurar el atributo de fecha de ultimo acceso, si se puede
      if (preservarAttr) then
      begin
        handle := CreateFile(PWideChar(fichero), FILE_WRITE_ATTRIBUTES,
          (FILE_SHARE_READ or FILE_SHARE_WRITE), nil, OPEN_EXISTING,
          FILE_ATTRIBUTE_NORMAL, 0);
        if (handle <> INVALID_HANDLE_VALUE) then
          SetFileTime(handle, nil, @accessAttr, nil);
        CloseHandle(handle);
      end;

      // Escribir resultados
      csvFields.Field[0] := candidato.Ruta;
      csvFields.Field[1] := candidato.fichero;
      if (candidato.error) and (FParametros.IncluirErrores) then
      begin
        csvFields.Field[2] := 'error';
        csvFields.Field[3] := candidato.mensajeError;
      end
      else
      begin
        csvFields.Field[2] := candidato.CuentaNIF;
        csvFields.Field[3] := candidato.FechaUltimoAcceso;
      end;
      if (not candidato.error) or (FParametros.IncluirErrores) then
        FControl.CSVWriter.WriteLine(csvFields);
      candidato.Free;

    except
      // cola cancelada
      on E: ESyncObjectException do
        Terminate;
    end;
end;

procedure TWorkerExplorador.procesarCandidato(const candidato: TsNIFdata);
var
  // Posicion actual en el buffer donde se están buscando cadenas NIF/NIE
  pbuffer: PByte;
  // Ultima posicion del buffer donde es posible buscar sin causar
  // desbordamiento
  maxbuffer: PByte;
  // Stream asociado al fichero de entrada
  input: TFileStream;
  // Total de bytes leidos desde el fichero y explorados
  bytesLeidos: int64;
  // Número de bytes que se han cargado en el buffer a partir del fichero.
  // Siempre igual o inferior a BUFFER_SIZE.
  bytesEnBuffer: integer;
  // Máximo de bytes donde debe aparecer alguna cadena NIF/NIE antes de
  // abandonar la exploración. Depende de "FParametros".
  maxBytesSinNIF: int64;
  // Nombre completo del fichero a explorar
  fichero: string;
begin
  try
    // Inicializar
    bytesLeidos := 0;
    ZeroMemory(Buffer + BUFFER_SIZE - BYTES_PER_NIF_UTF16, BYTES_PER_NIF_UTF16);
    fichero := TPath.Combine(candidato.Ruta, candidato.fichero);
    // Abrir fichero con permisos de lectura
    input := TFileStream.Create(TPath.SetExtendedPrefix(fichero), fmOpenRead,
      fmShareDenyWrite);
    if (FParametros.MaxSinNIF < 100) and (FParametros.MaxSinNIF > 0) then
      maxBytesSinNIF := Trunc(input.Size * (FParametros.MaxSinNIF / 100))
    else
      maxBytesSinNIF := input.Size;
    try
      repeat
        // Los ultimos bytes de la anterior iteración son los primeros bytes
        // ahora. No se vuelven a leer.
        CopyMemory(Buffer, Buffer + BUFFER_SIZE - BYTES_PER_NIF_UTF16,
          BYTES_PER_NIF_UTF16);
        // Cargar el resto del buffer desde el fichero
        bytesEnBuffer := input.Read((Buffer + BYTES_PER_NIF_UTF16)^,
          BUFFER_SIZE - BYTES_PER_NIF_UTF16) + BYTES_PER_NIF_UTF16;

        // Buscar cadenas coincidentes con NIF/NIE
        pbuffer := Buffer;
        maxbuffer := Buffer + bytesEnBuffer - BYTES_PER_NIF_UTF16;
        while (not Terminated) and (candidato.CuentaNIF < FParametros.MaxNIF)
          and (pbuffer < maxbuffer) do
        begin
          if (isNIF_UTF16(pbuffer)) then
          begin
            inc(candidato.CuentaNIF);
            inc(pbuffer, BYTES_PER_NIF_UTF16);
          end
          else if (isNIF_UTF8(pbuffer)) then
          begin
            inc(candidato.CuentaNIF);
            inc(pbuffer, BYTES_PER_NIF_UTF8);
          end
          else
            inc(pbuffer);
        end;
        bytesLeidos := bytesLeidos + bytesEnBuffer - BYTES_PER_NIF_UTF16;

      until
      // test: exploracion cancelada o terminada
        (Terminated) or
      // test: fin de fichero
        (bytesEnBuffer < BUFFER_SIZE) or
      // test: se alcanzó el limite de búsqueda
        (candidato.CuentaNIF >= FParametros.MaxNIF) or
      // test: no se han encontrado cadenas NIF/NIE en
      // la primera parte del fichero
        ((candidato.CuentaNIF = 0) and (bytesLeidos >= maxBytesSinNIF));
    finally
      input.Free;
    end;
  except
    // Error al intentar abrir o leer el fichero
    on E: Exception do
    begin
      FControl.incCuentaErroresFichero;
      candidato.error := true;
      candidato.mensajeError := E.Message;
    end;
  end;
  FControl.incCuentaFicherosExplorados;
end;

// --------------------------------------------------------------------------
// Otros
// --------------------------------------------------------------------------

constructor TWorkerExplorador.Create(const AControl: TsNIFControl;
  const Parametros: TsNIFParametros);
begin
  inherited Create(AControl, Parametros);
  GetMem(Buffer, BUFFER_SIZE);
  csvFields := TCSVFields.Create;
  csvFields.FieldCount := 4;
end;

destructor TWorkerExplorador.Destroy;
begin
  FreeMem(Buffer);
  csvFields.Free;
  inherited;
end;

end.
