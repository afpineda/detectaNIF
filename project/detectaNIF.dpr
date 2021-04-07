program detectaNIF;

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

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ThreadUtils.SafeDataTypes in '..\..\LIB\ThreadUtils\ThreadUtils.SafeDataTypes.pas',
  ThreadUtils.Sync in '..\..\LIB\ThreadUtils\ThreadUtils.Sync.pas',
  IOUtilsFix in '..\..\LIB\IOUtilsFix.pas',
  NIFUtils in '..\source\NIFUtils.pas',
  sNIF.control in '..\source\sNIF.control.pas',
  sNIF.data in '..\source\sNIF.data.pas',
  sNIF.params in '..\source\sNIF.params.pas',
  sNIF.worker.candidatos in '..\source\sNIF.worker.candidatos.pas',
  sNIF.worker.explorador in '..\source\sNIF.worker.explorador.pas',
  sNIF.worker in '..\source\sNIF.worker.pas',
  sNIF.cmdline in '..\source\CLI\sNIF.cmdline.pas',
  CLIVersion in '..\..\LIB\CLIVersion.pas',
  CSVUtils in '..\..\LIB\CSVUtils\CSVUtils.pas',
  CSVUtils.ThreadSafe in '..\..\LIB\CSVUtils\CSVUtils.ThreadSafe.pas',
  sNIF.params.DEF in '..\source\sNIF.params.DEF.pas',
  KV.Definition in '..\..\LIB\KeyValue\KV.Definition.pas',
  KV.Validator in '..\..\LIB\KeyValue\KV.Validator.pas',
  sNIF.cmdline.DEF in '..\source\CLI\sNIF.cmdline.DEF.pas';

const
  MsgStatus = #13'Se han procesado %d ficheros de %d (%d%%) en %d minutos... ';

  // --------------------------------------------------------------------------
  // Hilo principal (main)
  // --------------------------------------------------------------------------
var
  params: TsNIFparametros;
  control: TsNIFControl;

begin
  ExitCode := 0;
  try
    params := parsearLineaComandos;
    if (Params=nil) or (not Params.esCompleto) then
      // no debería ocurrir
      raise Exception.Create('No hay parámetros suficientes');
    control := TsNIFControl.Create;
    control.Iniciar(params);
    repeat
      Write(Format(MsgStatus, [control.CuentaFicherosExplorados,
        control.CuentaTotalFicheros, control.Progreso,
        control.MinutosDeTrabajo]));
    until control.haTerminado;
    Write(Format(MsgStatus, [control.CuentaFicherosExplorados,
      control.CuentaTotalFicheros, control.Progreso,
      control.MinutosDeTrabajo]));
    WriteLn('Finalizado');
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      ExitCode := 2000;
    end;
  end;
{$IFDEF DEBUG}readln{$ENDIF};

  // --------------------------------------------------------------------------

end.
