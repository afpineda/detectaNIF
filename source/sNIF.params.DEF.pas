unit sNIF.params.DEF;
{ *******************************************************

  sNIF

  Utilidad para buscar ficheros ofim�ticos con
  cadenas de caracteres coincidentes con NIF/NIE.

  *******************************************************

  2012-2018 �ngel Fern�ndez Pineda. Madrid. Spain.

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
  PARAM_MINBYTES = 'MinBytesTama�o';
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
    En GlobalParamDef se definen los los par�metros del fichero de
    configuraci�n y sus valores admitidos. Tambi�n sus descripciones.
    Los valores le�dos del fichero de configuraci�n se constrastar�n contra
    esta definici�n en "sNIF.cmdline.pas",
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
  'Indica las extensiones que puede tener un fichero para que sea elegido para su exploraci�n.';
d.ValidValueDescription :=
  'Extensiones de fichero sin punto y separadas por un espacio en blanco. Se admiten comodines. Por omisi�n, extensiones de ficheros ofim�ticos.';
d.OnValidate := function(const valor: variant): boolean
  begin
    Result := (Length(valor) > 0);
  end;

d := GlobalParamDef.Add(PARAM_MAXNIF, varInt64);
d.Description :=
  'Indica el n�mero m�ximo de cadenas coincidentes con el formato de NIF/NIE que ser�n contabilizadas. Superado este l�mite, se finalizar� la exploraci�n del fichero.';
d.ValidValueDescription := 'Entero mayor que cero. Por omisi�n, 1000.';
d.OnValidate := validadorCardinal;

d := GlobalParamDef.Add(PARAM_MAXSINNIF, varInt64);
d.Description :=
  'Indica, en porcentaje, la porci�n inicial del fichero que debe contener alguna cadena NIF/NIE para continuar con la exploraci�n.';
d.ValidValueDescription :=
  'Entero entre 40 y 100, ambos inclusive. Por omisi�n, 50.';
d.OnValidate := function(const valor: variant): boolean
  begin
    Result := (valor >= 40) and (valor <= 100);
  end;

d := GlobalParamDef.Add(PARAM_MINBYTES, varInt64);
d.Description :=
  'Indica el tama�o m�nimo, en bytes, que ha de tener un fichero para que sea elegible para su exploraci�n.';
d.ValidValueDescription := 'Entero mayor que cero. Por omisi�n, 3 MB.';
d.OnValidate := validadorCardinal;

d := GlobalParamDef.Add(PARAM_MINDIAS, varInt64);
d.Description :=
  'Indica el n�mero m�nimo de d�as que han de transcurrir desde la fecha de creaci�n de un fichero para que �ste sea elegible para su exploraci�n.';
d.ValidValueDescription := 'Entero mayor que cero. Por omisi�n, 365.';
d.OnValidate := validadorCardinal;

d := GlobalParamDef.Add(PARAM_HILOS, varInt64);
d.Description := 'Indica el n�mero de exploraciones concurrentes.';
d.ValidValueDescription :=
  'Entero entre uno y diez, ambos inclusive. Por omisi�n, 6.';
d.OnValidate := function(const valor: variant): boolean
  begin
    Result := (valor > 0) and (valor < 11);
  end;

d := GlobalParamDef.Add(PARAM_ERRORES, varBoolean);
d.Description :=
  'Indica si se desea dejar constancia de los ficheros que no pudieron ser explorados.';
d.ValidValueDescription := 'true (si, valor por omisi�n) o false (no).';

d := GlobalParamDef.Add(PARAM_RESUMEN, varBoolean);
d.Description :=
  'Indica si se desea generar un fichero adicional a modo de resumen.';
d.ValidValueDescription := 'true (si) o false (no, valor por omisi�n).';

d := GlobalParamDef.Add(PARAM_SALIDA, varString);
d.Description := 'Indica el fichero a generar con los resultados.';
d.ValidValueDescription := 'Ruta a un nombre de fichero.';
d.OnValidate := validadorFichero;

finalization

GlobalParamDef.Free;

end.
