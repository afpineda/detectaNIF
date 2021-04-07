unit sNIF.cmdline.DEF;

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
  CmdLineParams: TKeyValueValidator;

const
  CMDLINE_CFG = 'Cfg';
  CMDLINE_PLANTILLA = 'Plantilla';
  CMDLINE_SALIDA = 'Salida';
  CMDLINE_ENTRADA_FICHERO = 'FicheroCarpetas';
  CMDLINE_ENTRADA_CARPETA = 'Carpeta';

  {
    En CmdLineParams se definen los parámetros de la línea de comandos
    y sus valores admitidos. También sus descripciones.
    Los valores leídos del fichero de configuración se constrastarán contra
    esta definición en "sNIF.cmdline.pas".
  }

implementation

uses
  IOUtils,
  IOUtilsFix;

var
  df: TKeyDefinition;
  validadorFichero: TValueValidator;

initialization

validadorFichero := function(const valor: variant): boolean
  begin
    Result := (Length(valor) > 0) and (valor <> 'true') and
      (TPath.isWellFormed(valor, false, false));
  end;

CmdLineParams := TKeyValueValidator.Create;
df := CmdLineParams.Add(CMDLINE_CFG, varString);
df.Description :=
  '-Cfg <fichero>'#10#13'Toma los parámetros de exploración del fichero indicado.'#10#13'Por omisión, se utilizarán los parámetros por defecto.';
df.ValidValueDescription := 'Ruta a fichero de configuración (véase manual).';
df.OnValidate := validadorFichero;

df := CmdLineParams.Add(CMDLINE_PLANTILLA, varString);
df.Description :=
  '-Plantilla <fichero>'#10#13'Genera un fichero de configuración a modo de plantilla para su edición.'#10#13'Incompatible con los demás parámetros.';
df.ValidValueDescription := 'Ruta a fichero';
df.OnValidate := validadorFichero;

df := CmdLineParams.Add(CMDLINE_SALIDA, varString);
df.Description :=
  '-Salida <fichero>'#10#13'Fichero resultado de la exploración.';
df.ValidValueDescription := 'Ruta a fichero (formato CSV, codificación UTF-8)';
df.OnValidate := validadorFichero;

df := CmdLineParams.Add(CMDLINE_ENTRADA_FICHERO, varString);
df.Description :=
  '-FicheroCarpetas <fichero>'#10#13'Fichero de texto plano que contiene las carpetas a explorar (una por línea).';
df.ValidValueDescription := 'Ruta a fichero de texto plano';
df.OnValidate := validadorFichero;

df := CmdLineParams.Add(CMDLINE_ENTRADA_CARPETA, varString, 0, 65535);
df.Description :=
  '-Carpeta "<carpeta>" ["<carpeta>" ... "<Carpeta>"]'#10#13'Carpeta(s) a explorar recursivamente.';
df.ValidValueDescription := 'Ruta a carpeta(s)';
df.OnValidate := function(const valor: variant): boolean
  begin
    Result := (Length(valor) > 0) and (TPath.isWellFormed(valor, true, false));
  end;

finalization

CmdLineParams.Free;

end.
