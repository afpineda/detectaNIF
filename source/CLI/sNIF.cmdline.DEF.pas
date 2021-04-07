unit sNIF.cmdline.DEF;

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
  CmdLineParams: TKeyValueValidator;

const
  CMDLINE_CFG = 'Cfg';
  CMDLINE_PLANTILLA = 'Plantilla';
  CMDLINE_SALIDA = 'Salida';
  CMDLINE_ENTRADA_FICHERO = 'FicheroCarpetas';
  CMDLINE_ENTRADA_CARPETA = 'Carpeta';

  {
    En CmdLineParams se definen los par�metros de la l�nea de comandos
    y sus valores admitidos. Tambi�n sus descripciones.
    Los valores le�dos del fichero de configuraci�n se constrastar�n contra
    esta definici�n en "sNIF.cmdline.pas".
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
  '-Cfg <fichero>'#10#13'Toma los par�metros de exploraci�n del fichero indicado.'#10#13'Por omisi�n, se utilizar�n los par�metros por defecto.';
df.ValidValueDescription := 'Ruta a fichero de configuraci�n (v�ase manual).';
df.OnValidate := validadorFichero;

df := CmdLineParams.Add(CMDLINE_PLANTILLA, varString);
df.Description :=
  '-Plantilla <fichero>'#10#13'Genera un fichero de configuraci�n a modo de plantilla para su edici�n.'#10#13'Incompatible con los dem�s par�metros.';
df.ValidValueDescription := 'Ruta a fichero';
df.OnValidate := validadorFichero;

df := CmdLineParams.Add(CMDLINE_SALIDA, varString);
df.Description :=
  '-Salida <fichero>'#10#13'Fichero resultado de la exploraci�n.';
df.ValidValueDescription := 'Ruta a fichero (formato CSV, codificaci�n UTF-8)';
df.OnValidate := validadorFichero;

df := CmdLineParams.Add(CMDLINE_ENTRADA_FICHERO, varString);
df.Description :=
  '-FicheroCarpetas <fichero>'#10#13'Fichero de texto plano que contiene las carpetas a explorar (una por l�nea).';
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
