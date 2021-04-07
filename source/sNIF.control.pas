unit sNIF.control;
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
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  CSVUtils.ThreadSafe,
  sNIF.params,
  sNIF.data;

type
  TsNIFControl = class
    {
      PROPOSITO:

      Controla la concurrencia durante la exploración. Mantiene contadores
      de progreso y su estado.

      ALGORITMO:

      El trabajo se ejecuta en hilos concurrentes llamados "trabajadores".
      El trabajador "candidatos" genera los ficheros a explorar a partir de un
      listado recursivo de directorios. El trabajo se almacena en una cola
      concurrente denominada "ColaCandidatos".
      Varios hilos trabajadores de tipo "explorador" extraen de dicha cola
      cada fichero candidato y cuentan el número de ocurrencias de NIF/NIE.
      El resultado de cada exploración pasa a otra cola concurrente
      denominada "ColaResultados".
      El trabajador "escritor" toma los resultados de dicha cola y los escribe
      en el fichero de salida.

      ESTADOS:

      (incial y final): preparado
      Método "iniciar": preparado --> trabajando.
      Método "cancelar": trabajando --> cancelando.
      Evento de fin de trabajo: trabajando --> preparado
      Evento de fin de trabajo: cancelando --> preparado

      EVENTO DE FIN DE TRABAJO:

      El trabajador "candidatos" ha finalizado
      Y
      "ColaCandidatos" está vacía

      CONTADORES DE PROGRESO:

      Algunos contadores no están sincronizados porque se supone que solamente
      un hilo trabajador efectuará escrituras.
      Los restantes están sincronizados. La lectura no necesita sincronización.

    }
  public type
    TEstado = (preparado, cancelando, trabajando);
    EWorkerException = class(Exception);
  private
    FCuentaTotalFicheros: int64;
    FCuentaFicherosExplorados: int64;
    FCuentaCarpetas: int64;
    FCuentaErroresFichero: int64;
    FCuentaErroresCarpeta: int64;
    FTimestampInicio: TDateTime;
    FTimestampFin: TDateTime;
    FEstado: TEstado;
    FParametros: TsNIFParametros;
    FColaCandidatos: TColaCandidatos;
    FWorkers: TList<TThread>;
    FCSVWriter: TSafeCSVWriter;
    NumExploradores: int64;
    procedure FinDelTrabajo;
    function getProgreso: integer;
    function getMinutosTrabajo: int64;
  public
    // Propiedades y métodos públicos para uso exclusivo de los hilos
    // exploradores. No invocar directamente.
    property ColaCandidatos: TColaCandidatos read FColaCandidatos;
    property CSVWriter: TSafeCSVWriter read FCSVWriter;
  public
    // -- Inicialización

    constructor Create;
    destructor Destroy; override;

    // -- Contadores de progreso

    // Nota: Solo escritura para el hilo que busca ficheros candidatos
    procedure incCuentaTotalFicheros;
    property CuentaTotalFicheros: int64 read FCuentaTotalFicheros;
    procedure incCuentaCarpetas;
    property CuentaCarpetas: int64 read FCuentaCarpetas;
    property Progreso: integer read getProgreso;
    property MinutosDeTrabajo: int64 read getMinutosTrabajo;
    procedure incCuentaErroresCarpeta;
    property CuentaErroresCarpeta: int64 read FCuentaErroresCarpeta;

    // Nota: propiedades/procedimientos sincronizados
    procedure incCuentaFicherosExplorados;
    property CuentaFicherosExplorados: int64 read FCuentaFicherosExplorados;
    procedure incCuentaErroresFichero;
    property CuentaErroresFichero: int64 read FCuentaErroresFichero;

    // -- Control y estado
    procedure Iniciar(const Parametros: TsNIFParametros);
    procedure Cancelar;
    property Estado: TEstado read FEstado;
    // En aplicaciones CLI no funcionan los eventos, por lo que se necesita
    // esta primitiva de espera en bucle. No es espera activa.
    function haTerminado(const timeout: cardinal = 1000): boolean;

  end;

implementation

uses
  SyncObjs,
  // System.SysUtils,
  System.DateUtils,
  ThreadUtils.Sync,
  CSVUtils,
  IOUtils,
  IOUtilsFix,
  sNIF.worker.candidatos,
  sNIF.worker.explorador,
  windows;

// --------------------------------------------------------------------------
// TsNIFControl
// --------------------------------------------------------------------------

// -- Inicialización

constructor TsNIFControl.Create;
begin
  inherited;
  FWorkers := TList<TThread>.Create;
  FEstado := preparado;
  FColaCandidatos := nil;
  FCSVWriter := nil;
  FTimestampInicio := Now;
  FTimestampFin := FTimestampInicio;
end;

destructor TsNIFControl.Destroy;
begin
  if (FEstado <> preparado) then
    // no debería ocurrir
    raise EAbort.Create
      ('Se ha ejecutado TsNIFControl.Destroy con la exploración en marcha');
  FWorkers.Free;
  inherited;
end;

// -- Contadores de progreso

procedure TsNIFControl.incCuentaTotalFicheros;
begin
  inc(FCuentaTotalFicheros);
end;

procedure TsNIFControl.incCuentaCarpetas;
begin
  inc(FCuentaCarpetas);
end;

procedure TsNIFControl.incCuentaErroresCarpeta;
begin
  inc(FCuentaErroresCarpeta);
end;

procedure TsNIFControl.incCuentaFicherosExplorados;
begin
  InterlockedIncrement64(FCuentaFicherosExplorados);
end;

procedure TsNIFControl.incCuentaErroresFichero;
begin
  InterlockedIncrement64(FCuentaErroresFichero);
end;

function TsNIFControl.getProgreso: integer;
begin
  if (FCuentaTotalFicheros > 0) then
    Result := Trunc((FCuentaFicherosExplorados * 1.0 /
      FCuentaTotalFicheros) * 100)
  else
    Result := 0;
end;

function TsNIFControl.getMinutosTrabajo: int64;
begin
  if (FEstado = preparado) then
    Result := MinutesBetween(FTimestampFin, FTimestampInicio)
  else
    Result := MinutesBetween(Now, FTimestampInicio);
end;

// -- Control

procedure TsNIFControl.Iniciar(const Parametros: TsNIFParametros);
var
  worker: TThread;
  c: integer;
  csvFields: TCSVFields;
begin
  if (FEstado = preparado) then
  begin
    FTimestampInicio := Now;
    FParametros := Parametros; // Se necesita en "FinDelTrabajo"
    FWorkers.Clear;
    FCuentaTotalFicheros := 0;
    FCuentaFicherosExplorados := 0;
    FCuentaCarpetas := 0;
    FCuentaErroresFichero := 0;
    FCuentaErroresCarpeta := 0;

    // Preparar fichero CSV de resultados
    FCSVWriter := TSafeCSVWriter.Create
      (TPath.SetExtendedPrefix(Parametros.FicheroSalida));
    csvFields := TCSVFields.Create;
    // Escribir cabecera de fichero CSV
    try
      csvFields.StrictSyntax := true;
      csvFields.FieldCount := 4;
      csvFields.Field[0] := 'Carpeta';
      csvFields.Field[1] := 'Fichero';
      csvFields.Field[2] := 'CuentaNIF';
      csvFields.Field[3] := 'Ultimo acceso';
      FCSVWriter.WriteLine(csvFields);
    finally
      csvFields.Free;
    end;

    try
      FColaCandidatos := TColaCandidatos.Create;
      // Crear hilo buscador de ficheros candidatos
      worker := TWorkerCandidatos.Create(self, Parametros);
      FWorkers.Add(worker);
      // Crear hilos exploradores de cadenas NIF/NIE
      NumExploradores := Parametros.NumHilos;
      for c := 0 to Parametros.NumHilos - 1 do
      begin
        worker := TWorkerExplorador.Create(self, Parametros);
        FWorkers.Add(worker);
      end;
    except
      // Si algo falla, finalizar los hilos que se hayan podido crear
      // y liberar recursos.
      FColaCandidatos.Free;
      TThread.TerminateAll(FWorkers);
      // Todos los hilos se crean en estado suspendido. Hay que despertarlos
      // para que puedan terminar.
      TThread.StartAll(FWorkers);
      TThread.FreeAll(FWorkers);
      // Relanzar la excepción
      raise;
    end;
    // iniciar el trabajo
    FEstado := trabajando;
    TThread.StartAll(FWorkers);
  end;
end;

procedure TsNIFControl.Cancelar;
begin
  if (FEstado = trabajando) then
  begin
    FEstado := cancelando;
    // Solicitar la parada de todos los hilos
    TThread.TerminateAll(FWorkers);
    // Cancelar la cola. De lo contrario, algunos hilos quedarían
    // bloqueados y no terminarían nunca.
    FreeAndNil(FColaCandidatos);
  end;
end;

procedure TsNIFControl.FinDelTrabajo;
var
  estadoPrevio: TEstado;
  csv: TCSVWriter;
  I: integer;
  ExcepcionEnHilo: boolean;
  MensajeExcepcion: string;
begin
  // Liberar recursos y cambiar estado
  estadoPrevio := Estado;
  FEstado := preparado;
  FTimestampFin := Now;
  FreeAndNil(FCSVWriter);
  FreeAndNil(FColaCandidatos);
  csv := nil;

  // Determinar si algún hilo terminó en excepción
  ExcepcionEnHilo := false;
  I := 0;
  while (I < FWorkers.Count) and (not ExcepcionEnHilo) do
  begin
    ExcepcionEnHilo := (FWorkers[I].FatalException <> nil);
    inc(I);
  end;
  if (ExcepcionEnHilo) then
    MensajeExcepcion := Exception(FWorkers[I-1].FatalException).Message;
  TThread.FreeAll(FWorkers);

  // Generar fichero resumen, si procede
  if (FParametros.GenerarResumen) then
    try
      csv := TCSVWriter.Create(FParametros.FicheroResumen);
      csv.FieldCount := 2;
      csv.Field[0] := 'Clave';
      csv.Field[1] := 'Valor';
      csv.WriteLine;
      csv.Field[0] := 'MARCA_TIEMPO_INICIO';
      csv.Field[1] := FormatDateTime('yyyy/mm/dd hh:nn:ss', FTimestampInicio);
      csv.WriteLine;
      csv.Field[0] := 'MARCA_TIEMPO_FIN';
      csv.Field[1] := FormatDateTime('yyyy/mm/dd hh:nn:ss', FTimestampFin);
      csv.WriteLine;
      csv.Field[0] := 'TIEMPO_EJECUCION';
      csv.Field[1] := FormatDateTime('hh:nn:ss',
        FTimestampFin - FTimestampInicio);
      csv.WriteLine;
      csv.Field[0] := 'CUENTA_CARPETAS_EXPLORADAS';
      csv.Field[1] := FCuentaCarpetas;
      csv.WriteLine;
      csv.Field[0] := 'CUENTA_CARPETAS_ERROR';
      csv.Field[1] := FCuentaErroresCarpeta;
      csv.WriteLine;
      csv.Field[0] := 'CUENTA_FICHEROS_EXPLORADOS';
      csv.Field[1] := FCuentaFicherosExplorados;
      csv.WriteLine;
      csv.Field[0] := 'CUENTA_FICHEROS_ERROR_LECTURA';
      csv.Field[1] := FCuentaErroresFichero;
      csv.WriteLine;
      csv.Field[0] := 'ESTADO_FINALIZACION';
      if (ExcepcionEnHilo) then
        csv.Field[1] := 'ABORTADO'
      else if (estadoPrevio = cancelando) then
        csv.Field[1] := 'CANCELADO'
      else
        csv.Field[1] := 'correcto';
      csv.WriteLine;
      if (ExcepcionEnHilo) then
      begin
        csv.Field[0] := 'MENSAJE_ERROR';
        csv.Field[1] := MensajeExcepcion;
        csv.WriteLine;
      end;
      for I := 0 to FParametros.Carpetas.Count - 1 do
      begin
        csv.Field[0] := 'CARPETA_EXPLORADA';
        csv.Field[1] := FParametros.Carpetas.Strings[I];
        csv.WriteLine;
      end;
      csv.Field[0] := 'EXTENSIONES';
      csv.Field[1] := FParametros.Extensiones.DelimitedText;
      csv.WriteLine;
      csv.Field[0] := 'MAX_CUENTA_NIF';
      csv.Field[1] := FParametros.MaxNIF;
      csv.WriteLine;
      csv.Field[0] := 'MAX_PORCENTAJE_SIN_NIF';
      csv.Field[1] := FParametros.MaxSinNIF;
      csv.WriteLine;
      csv.Field[0] := 'MIN_BYTES';
      csv.Field[1] := FParametros.MinBytes;
      csv.WriteLine;
      csv.Field[0] := 'MIN_DIAS_CREADO';
      csv.Field[1] := FParametros.MinDiasCreado;
      csv.WriteLine;
      csv.Free;
    except
      // ignorar errores
      csv.Free;
    end;

  // Si algún trabajador abortó su ejecución, lanzar la excepción
  // en el hilo principal
  if (ExcepcionEnHilo) then
    raise EWorkerException.Create(MensajeExcepcion);
end;

function TsNIFControl.haTerminado(const timeout: cardinal = 1000): boolean;
var
  wr: TWaitResult;
begin
  Result := false;
  if (FEstado <> preparado) then
  begin
    wr := TThread.WaitForAll(FWorkers, timeout);
    if (wr = wrSignaled) then
    begin
      Result := true;
      FinDelTrabajo;
    end
    else if (wr <> wrTimeout) then
      RaiseLastOSError
  end
  else
    Result := true;
end;

end.
