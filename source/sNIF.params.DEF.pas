unit sNIF.params.DEF;
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
  KV.Definition;

var
  GlobalParamDef: TKeyValueValidator;

const
  PARAM_CARPETA = 'Carpeta';
  PARAM_EXT = 'Extensiones';
  PARAM_MAXNIF = 'MaxNIF';
  PARAM_MAXSINNIF = 'MaxSinNIF';
  PARAM_MINBYTES = 'MinBytesTamaño';
  PARAM_MINDIAS = 'MinDiasCreado';
  PARAM_HILOS = 'NumeroHilos';
  PARAM_ERRORES = 'IncluirErrores';
  PARAM_RESUMEN = 'GenerarResumen';
  PARAM_SALIDA = 'Salida';

implementation

uses
  IOUtils,
  IOUtilsFix;

var
  d: TKeyDefinition;
  validadorCardinal: TValueValidator;
  validadorFichero: TValueValidator;

  {
    En GlobalParamDef se definen los los parámetros del fichero de
    configuración y sus valores admitidos. También sus descripciones.
    Los valores leídos del fichero de configuración se constrastarán contra
    esta definición en "sNIF.cmdline.pas",
  }

initialization

GlobalParamDef := TKeyValueValidator.Create;

validadorCardinal := function(const valor: variant): boolean
  begin
    Result := (valor >= 0);
  end;

validadorFichero := function(const valor: variant): boolean
  begin
    Result := (Length(valor) > 0) and (TPath.isWellFormed(valor, false, false));
  end;

d := GlobalParamDef.Add(PARAM_CARPETA, varString, 0, 65535);
d.Description := 'Carpeta a explorar (recursivamente)';
d.ValidValueDescription :=
  'Ruta a una carpeta existente. No se admiten comodines (* o ?).';
d.OnValidate := function(const valor: variant): boolean
  begin
    Result := (Length(valor) > 0) and (TPath.isWellFormed(valor, true, false));
  end;

d := GlobalParamDef.Add(PARAM_EXT, varString);
d.Description :=
  'Indica las extensiones que puede tener un fichero para que sea elegido para su exploración.';
d.ValidValueDescription :=
  'Extensiones de fichero sin punto y separadas por un espacio en blanco. Se admiten comodines. Por omisión, extensiones de ficheros ofimáticos.';
d.OnValidate := function(const valor: variant): boolean
  begin
    Result := (Length(valor) > 0);
  end;

d := GlobalParamDef.Add(PARAM_MAXNIF, varInt64);
d.Description :=
  'Indica el número máximo de cadenas coincidentes con el formato de NIF/NIE que serán contabilizadas. Superado este límite, se finalizará la exploración del fichero.';
d.ValidValueDescription := 'Entero mayor que cero. Por omisión, 1000.';
d.OnValidate := validadorCardinal;

d := GlobalParamDef.Add(PARAM_MAXSINNIF, varInt64);
d.Description :=
  'Indica, en porcentaje, la porción inicial del fichero que debe contener alguna cadena NIF/NIE para continuar con la exploración.';
d.ValidValueDescription :=
  'Entero entre 40 y 100, ambos inclusive. Por omisión, 50.';
d.OnValidate := function(const valor: variant): boolean
  begin
    Result := (valor >= 40) and (valor <= 100);
  end;

d := GlobalParamDef.Add(PARAM_MINBYTES, varInt64);
d.Description :=
  'Indica el tamaño mínimo, en bytes, que ha de tener un fichero para que sea elegible para su exploración.';
d.ValidValueDescription := 'Entero mayor que cero. Por omisión, 3 MB.';
d.OnValidate := validadorCardinal;

d := GlobalParamDef.Add(PARAM_MINDIAS, varInt64);
d.Description :=
  'Indica el número mínimo de días que han de transcurrir desde la fecha de creación de un fichero para que éste sea elegible para su exploración.';
d.ValidValueDescription := 'Entero mayor que cero. Por omisión, 365.';
d.OnValidate := validadorCardinal;

d := GlobalParamDef.Add(PARAM_HILOS, varInt64);
d.Description := 'Indica el número de exploraciones concurrentes.';
d.ValidValueDescription :=
  'Entero entre uno y diez, ambos inclusive. Por omisión, 6.';
d.OnValidate := function(const valor: variant): boolean
  begin
    Result := (valor > 0) and (valor < 11);
  end;

d := GlobalParamDef.Add(PARAM_ERRORES, varBoolean);
d.Description :=
  'Indica si se desea dejar constancia de los ficheros que no pudieron ser explorados.';
d.ValidValueDescription := 'true (si, valor por omisión) o false (no).';

d := GlobalParamDef.Add(PARAM_RESUMEN, varBoolean);
d.Description :=
  'Indica si se desea generar un fichero adicional a modo de resumen.';
d.ValidValueDescription := 'true (si) o false (no, valor por omisión).';

d := GlobalParamDef.Add(PARAM_SALIDA, varString);
d.Description := 'Indica el fichero a generar con los resultados.';
d.ValidValueDescription := 'Ruta a un nombre de fichero.';
d.OnValidate := validadorFichero;

finalization

GlobalParamDef.Free;

end.
