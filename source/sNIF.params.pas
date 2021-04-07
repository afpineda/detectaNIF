unit sNIF.params;
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
  System.Classes;

type
  {
    PROPOSITO:

    Contiene los parámetros de búsqueda y exploración.
    No deben modificarse una vez se ha iniciado la exploración porque
    no hay sincronización.
  }
  TsNIFParametros = class
  private
    FCarpetas: TStrings;
    FExts: TStrings;
    FMaxNIF: cardinal;
    FMaxSinNIF: integer;
    FMinBytes: int64;
    FMinDiasCreado: integer;
    FNumHilos: integer;
    FFicheroSalida: string;
    FIncluirErrores: boolean;
    FGenerarResumen: boolean;
    procedure setMaxSinNif(const porcentaje: integer);
    procedure setNumHilos(const valor: integer);
    procedure setMinBytes(const valor: int64);
    procedure setMaxNIF(const valor: cardinal);
    procedure setMinDiasCreado(const valor: integer);
    function getFicheroResumen: string;
  public
    // Constructor/Destructor
    constructor Create;
    destructor Destroy; override;
    procedure Limpiar;
    // Indica si falta algun parámetro imprescindible
    function esCompleto: boolean;
    // Comprobar que los parametros son coherentes y corregirlos
    procedure comprobarCoherencia;
    // Carpetas a explorar
    property Carpetas: TStrings read FCarpetas;
    // Extensiones de los ficheros a explorar (sin punto)
    property Extensiones: TStrings read FExts;
    // No seguir buscando si se alcanza MaxNIF
    property MaxNIF: cardinal read FMaxNIF write setMaxNIF;
    // No seguir buscando si no se encuentran NIF tras buscar
    // en el % inicial del fichero establecido en MaxSinNIF
    property MaxSinNIF: integer read FMaxSinNIF write setMaxSinNif;
    // Tamaño mínimo de los ficheros a explorar en bytes
    property MinBytes: int64 read FMinBytes write setMinBytes;
    // Número mínimo de días transcurridos desde que el fichero se
    // creó para que sea elegido para exploración
    property MinDiasCreado: integer read FMinDiasCreado write setMinDiasCreado;
    // Número de hilos para explorar el contenido de los ficheros
    property NumHilos: integer read FNumHilos write setNumHilos;
    // Fichero con los resultados
    property FicheroSalida: string read FFicheroSalida write FFicheroSalida;
    // Fichero con el resumen de los resultados
    property FicheroResumen: string read getFicheroResumen;
    // true para incluir los errores de lectura en el fichero de salida
    property IncluirErrores: boolean read FIncluirErrores write FIncluirErrores;
    // true para generar el fichero con el resumen de los resultados. false
    // para no generarlo
    property GenerarResumen: boolean read FGenerarResumen write FGenerarResumen;
  end;

const
  EXTENSIONES_POR_DEFECTO = 'xls xlsx accdb mdb csv txt xlsxm xlsxb mdx ' +
    'accda mde accde ade db dbf prn dif tab adp';
  MAXIMO_HILOS = 10;

implementation

uses
  sNIF.params.DEF,
  IOUtils,
  IOUtilsFix,
  System.SysUtils;

// ----------------------------------------------------------------------------
// TsNIFParametros
// ----------------------------------------------------------------------------

constructor TsNIFParametros.Create;
begin
  inherited Create;
  FCarpetas := TStringList.Create;
  FExts := TStringList.Create;
  FExts.Delimiter := ' ';
  FExts.StrictDelimiter := true;
  Limpiar;
end;

procedure TsNIFParametros.Limpiar;
begin
  FCarpetas.Clear;
  FExts.Clear;
  FMaxNIF := 1000;
  FMaxSinNIF := 50;
  FFicheroSalida := '';
  FIncluirErrores := true;
  FGenerarResumen := true;
  FNumHilos := 6;
{$IFDEF DEBUG}
  FMinBytes := 0;
  FMinDiasCreado := 0;
  FExts.DelimitedText := '*';
{$ELSE}
  FMinBytes := 3145728; // = 3 MB
  FMinDiasCreado := 365;
  FExts.DelimitedText := 'xls accd? md? csv txt xlsx* db* prn ' +
    'dif tab adp gdb ib jet odb sbf sqlite* xml';
{$ENDIF}
end;

destructor TsNIFParametros.Destroy;
begin
  FCarpetas.Free;
  FExts.Free;
  inherited Destroy;
end;

procedure TsNIFParametros.comprobarCoherencia;
var
  i, j: integer;
begin
  // Comprobar que solo hay rutas de carpeta bien formadas
  i := 0;
  while i < FCarpetas.Count do
    if (not TPath.isWellFormed(FCarpetas.Strings[i], true, false)) then
      FCarpetas.Delete(i)
    else
      inc(i);

  // Excluir subcarpetas si ya está incluida la raiz y
  // quitar carpetas duplicadas. Comprobar que es una ruta válida
  // (hacia adelante)
  for i := 0 to FCarpetas.Count - 1 do
  begin
    j := i + 1;
    while j < FCarpetas.Count do
      if (TPath.isContainerOf(FCarpetas.Strings[i], FCarpetas.Strings[j])) or
        (CompareText(FCarpetas.Strings[i], FCarpetas.Strings[j]) = 0) then
        FCarpetas.Delete(j)
      else
        inc(j);
  end;
  // (hacia atrás)
  i := FCarpetas.Count - 1;
  while (i >= 0) do
  begin
    j := i - 1;
    while (j >= 0) do
      if (TPath.isContainerOf(FCarpetas.Strings[i], FCarpetas.Strings[j])) or
        (CompareText(FCarpetas.Strings[i], FCarpetas.Strings[j]) = 0) then
      begin
        FCarpetas.Delete(j);
        dec(j);
        dec(i);
      end
      else
        dec(j);
    dec(i);
  end;

  // Si no hay extensiones, buscar todas
  if (FExts.Count = 0) then
    FExts.Add('*');
end;

function TsNIFParametros.getFicheroResumen: string;
begin
  if (Length(FFicheroSalida) > 0) then
    Result := TPath.ReplaceFileName(FFicheroSalida, FFicheroSalida + '.log')
  else
    Result := '';
end;

function TsNIFParametros.esCompleto: boolean;
begin
  Result := (Length(FFicheroSalida) > 0) and (FExts.Count > 0) and
    (FCarpetas.Count > 0);
end;

procedure TsNIFParametros.setMaxNIF(const valor: cardinal);
begin
  if (valor >= 20) then
    FMaxNIF := valor
  else
    raise EArgumentOutOfRangeException.Create
      ('Valor fuera de rango en parámetro ' + PARAM_MAXNIF);
end;

procedure TsNIFParametros.setMinBytes(const valor: int64);
begin
  if (valor >= 0) then
    FMinBytes := valor
  else
    raise EArgumentOutOfRangeException.Create
      ('Valor fuera de rango en parámetro ' + PARAM_MINBYTES);
end;

procedure TsNIFParametros.setMinDiasCreado(const valor: integer);
begin
  if (valor >= 0) then
    FMinDiasCreado := valor
  else
    raise EArgumentOutOfRangeException.Create
      ('Valor fuera de rango en parámetro ' + PARAM_MINDIAS);
end;

procedure TsNIFParametros.setMaxSinNif(const porcentaje: integer);
begin
  if (porcentaje >= 40) and (porcentaje <= 100) then
    FMaxSinNIF := porcentaje
  else
    raise EArgumentOutOfRangeException.Create
      ('Valor fuera de rango en parámetro ' + PARAM_MAXSINNIF);
end;

procedure TsNIFParametros.setNumHilos(const valor: integer);
begin
  if (valor > 0) and (valor <= MAXIMO_HILOS) then
    FNumHilos := valor
  else
    raise EArgumentOutOfRangeException.Create
      ('Valor fuera de rango en parámetro ' + PARAM_HILOS);
end;

// ----------------------------------------------------------------------------

end.
