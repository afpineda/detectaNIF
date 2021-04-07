unit sNIF.cmdline;
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
  sNIF.params;

function parsearLineaComandos: TsNIFParametros;

implementation

uses
  System.variants,
  System.Classes,
  CLIVersion,
  IOUtils,
  IOUtilsFix,
  sNIF.params.DEF,
  KV.Definition,
  KV.Validator,
  sNIF.cmdline.DEF,
  System.SysUtils;

{ Mensaje por pantalla cuando se ejecuta sin parámetros }
procedure noHayParametros;
var
  Major, Minor, Release, Build: integer;
  I: integer;
begin
  WriteLn('Efectúa una exploración binaria de ficheros en busca');
  WriteLn('de cadenas coincidentes con el NIF/NIE de persona física.');
  WriteLn('Preserva las fechas de último acceso, si es posible.');
  WriteLn;
  WriteLn('PARÁMETROS');
  WriteLn;
  for I := 0 to CmdLineParams.Count - 1 do
  begin
    WriteLn(CmdLineParams.keyDefByIndex[I].Description);
    WriteLn(CmdLineParams.keyDefByIndex[I].ValidValueDescription);
    WriteLn;
  end;
  WriteLn('Software en el Dominio Público. PROHIBIDA SU VENTA.');
  if CLIExeVersion(Major, Minor, Release, Build) then
    WriteLn(Format('  Versión %d.%d', [Major, Minor]));
  Halt(10);
end;

{ detectaNIF -FicheroCarpetas <fichero> }
procedure leerFicheroCarpetas(const fichero: string; cfg: TsNIFParametros);
var
  txt: TStringList;
  I: integer;
begin
  txt := TStringList.Create;
  try
    txt.LoadFromFile(fichero);
    for I := 0 to txt.Count - 1 do
      cfg.Carpetas.Add(txt.Strings[I]);
  except
    WriteLn('ERROR: imposible leer fichero de carpetas a explorar');
    WriteLn('(' + fichero + ')');
    Halt(1000);
  end;
end;

{ detectaNIF -plantilla <fichero> }
procedure crearPlantilla(const fichero: string);
var
  txt: TStringList;
  paramDef: TKeyDefinition;
  I: integer;
begin
  txt := TStringList.Create;
  for I := 0 to GlobalParamDef.Count - 1 do
  begin
    paramDef := GlobalParamDef.keyDefByIndex[I];
    txt.Add('# ' + paramDef.Description);
    txt.AddPair(paramDef.tag, paramDef.ValidValueDescription);
  end;
  try
    txt.SaveToFile(fichero, TEncoding.UTF8);
  except
    WriteLn('ERROR: imposible crear plantilla');
    Halt(104);
  end;
  WriteLn('Plantilla generada');
  Halt(0);
end;

{ detectaNIF -cfg <fichero> }
procedure cargarCfg(const fichero: string; cfg: TsNIFParametros);
var
  valores: TStringList;
begin
  valores := TStringList.Create;
  valores.Duplicates := dupAccept;
  valores.CaseSensitive := false;
  valores.Sorted := false;
  try
    valores.LoadFromFile(fichero);
    KV.Validator.Validate(valores, GlobalParamDef,
      procedure(const tag: string; const value: variant)
      begin
        with cfg do
          if (CompareText(tag, PARAM_CARPETA) = 0) then
            Carpetas.Add(value)
          else if (CompareText(tag, PARAM_EXT) = 0) then
            Extensiones.DelimitedText := value
          else if (CompareText(tag, PARAM_MAXNIF) = 0) then
            MaxNIF := value
          else if (CompareText(tag, PARAM_MAXSINNIF) = 0) then
            MaxSinNIF := value
          else if (CompareText(tag, PARAM_MINBYTES) = 0) then
            MinBytes := value
          else if (CompareText(tag, PARAM_MINDIAS) = 0) then
            MinDiasCreado := value
          else if (CompareText(tag, PARAM_HILOS) = 0) then
            NumHilos := value
          else if (CompareText(tag, PARAM_ERRORES) = 0) then
            IncluirErrores := value
          else if (CompareText(tag, PARAM_RESUMEN) = 0) then
            GenerarResumen := value
          else if (CompareText(tag, PARAM_SALIDA) = 0) and (FicheroSalida = '')
          then
            FicheroSalida := value
      end);
    valores.Free;
  except
    on E: EInvalidValueException do
    begin
      WriteLn('ERROR DE CONFIGURACIÓN: valor inválido en parámetro "' +
        E.tag + '"');
      WriteLn(GlobalParamDef.KeyDefAtLastException.ValidValueDescription);
      Halt(900);
    end;
    on E: EVariantTypeCastError do
    begin
      WriteLn('ERROR DE CONFIGURACIÓN: valor de tipo incorrecto en parámetro "'
        + GlobalParamDef.KeyDefAtLastException.tag + '"');
      WriteLn(GlobalParamDef.KeyDefAtLastException.ValidValueDescription);
      Halt(1000);
    end;
    // on E: EMinCardinalityException do
    // begin
    // WriteLn('ERROR DE CONFIGURACIÓN: parámetro "' + E.tag + '" obligatorio');
    // Halt(900);
    // end;
    on E: EMaxCardinalityException do
    begin
      WriteLn('ERROR DE CONFIGURACIÓN: parámetro "' + E.tag + '" duplicado');
      Halt(900);
    end;
    on E: EUnknownKeyException do
    begin
      WriteLn('ERROR DE CONFIGURACIÓN: parámetro "' + E.tag +
        '" no reconocido');
      Halt(900);
    end;
    on E: EStreamError do
    begin
      WriteLn('ERROR: el fichero de configuración no existe o no es accesible');
      Halt(900);
    end
    else
      raise;
  end;
end;

procedure validarCfg(cfg: TsNIFParametros);
begin
  if (cfg.esCompleto) then
  begin
    cfg.comprobarCoherencia;
    if (not TFile.CanCreate(cfg.FicheroSalida)) then
    begin
      WriteLn('ERROR: imposible crear el fichero de salida');
      WriteLn(cfg.FicheroSalida);
      WriteLn;
      Halt(100);
    end;
  end
  else
  begin
    WriteLn('ERROR: faltan parametros imprescindibles para la exploración');
    WriteLn('(carpeta a explorar, fichero de salida o extensiones de fichero)');
    WriteLn;
    Halt(100);
  end;
end;

function parsearLineaComandos: TsNIFParametros;
var
  cfg: TsNIFParametros;
begin
  Result := nil;
  if (ParamCount = 0) then
    noHayParametros; // detiene la ejecución
  cfg := TsNIFParametros.Create;
  try
    KV.Validator.ValidateCmdLine(CmdLineParams,
      procedure(const tag: string; const value: variant)
      begin
        if (CompareText(tag, CMDLINE_CFG) = 0) then
          cargarCfg(value, cfg)
        else if (CompareText(tag, CMDLINE_PLANTILLA) = 0) then
          crearPlantilla(value)
        else if (CompareText(tag, CMDLINE_SALIDA) = 0) then
          cfg.FicheroSalida := value
        else if (CompareText(tag, CMDLINE_ENTRADA_FICHERO) = 0) then
          leerFicheroCarpetas(value, cfg)
        else if (CompareText(tag, CMDLINE_ENTRADA_CARPETA) = 0) then
          cfg.Carpetas.Add(value);
      end);
    validarCfg(cfg);
    Result := cfg;
  except
    on E: EVariantTypeCastError do
    begin
      WriteLn('ERROR: El parámetro "' + CmdLineParams.KeyDefAtLastException.tag
        + '" no admite el valor dado');
      WriteLn(CmdLineParams.KeyDefAtLastException.ValidValueDescription);
      Halt(1000);
    end;
    on E: EInvalidValueException do
    begin
      WriteLn('ERROR: Ruta mal formada en parámetro "' + E.tag + '"');
      WriteLn('(' + CmdLineParams.KeyDefAtLastException.
        ValueAtLastException + ')');
      Halt(1000);
    end;
    on E: EMaxCardinalityException do
    begin
      WriteLn('ERROR: parámetro "' + E.tag + '" duplicado');
      Halt(1000);
    end;
    on E: EUnknownKeyException do
    begin
      WriteLn('ERROR: parámetro "' + E.tag + '" no reconocido');
      Halt(1000);
    end
    else
      raise;
  end;
end;


// function parsearLineaComandos: TsNIFParametros;
// var
// fichero, salida: string;
// begin
// Result := nil;
// if (ParamCount = 0) then
// noHayParametros
// else if (FindCmdLineSwitch(CMDLINE_PLANTILLA, fichero)) then
// crearPlantilla(fichero)
// else if (FindCmdLineSwitch(CMDLINE_CFG, fichero)) then
// try
// Result := cargarCfg(fichero);
// if (FindCmdLineSwitch(PARAM_SALIDA, salida)) then
// Result.FicheroSalida := salida;
// validarCfg(Result);
// except
// on E: EInvalidValueException do
// begin
// WriteLn('ERROR: valor inválido en parámetro "' + E.tag + '"');
// WriteLn(GlobalParamDef.KeyDefAtLastException.ValidValueDescription);
// Halt(1000);
// end;
// on E: EVariantTypeCastError do
// begin
// WriteLn('ERROR: valor de tipo incorrecto en parámetro "' +
// GlobalParamDef.KeyDefAtLastException.tag + '"');
// WriteLn(GlobalParamDef.KeyDefAtLastException.ValidValueDescription);
// Halt(1000);
// end;
// on E: EMinCardinalityException do
// begin
// WriteLn('ERROR: parámetro "' + E.tag + '" obligatorio');
// Halt(1000);
// end;
// on E: EMaxCardinalityException do
// begin
// WriteLn('ERROR: parámetro "' + E.tag + '" duplicado');
// Halt(1000);
// end;
// on E: EUnknownKeyException do
// begin
// WriteLn('ERROR: parámetro "' + E.tag + '" no reconocido');
// Halt(1000);
// end;
// on E: EStreamError do
// begin
// WriteLn('ERROR: el fichero de configuración no existe o no es accesible');
// Halt(1000);
// end
// else
// raise;
// end;
// if (Result = nil) then
// begin
// WriteLn('ERROR: se necesita un fichero de configuración');
// Halt(103);
// end;
// end;

end.
