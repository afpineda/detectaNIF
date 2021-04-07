program sNIFdebug;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  IOUtils,
  ThreadUtils.SafeDataTypes in '..\..\LIB\ThreadUtils\ThreadUtils.SafeDataTypes.pas',
  ThreadUtils.Sync in '..\..\LIB\ThreadUtils\ThreadUtils.Sync.pas',
  IOUtilsFix in '..\..\LIB\IOUtilsFix.pas',
  CSVUtils in '..\..\LIB\CSVUtils.pas',
  NIFUtils in '..\source\NIFUtils.pas',
  sNIF.control in '..\source\sNIF.control.pas',
  sNIF.data in '..\source\sNIF.data.pas',
  sNIF.params.CSV in '..\source\sNIF.params.CSV.pas',
  sNIF.params in '..\source\sNIF.params.pas',
  sNIF.worker.candidatos in '..\source\sNIF.worker.candidatos.pas',
  sNIF.worker.explorador in '..\source\sNIF.worker.explorador.pas',
  sNIF.worker in '..\source\sNIF.worker.pas',
  CSVUtils.ThreadSafe in '..\..\LIB\CSVUtils.ThreadSafe.pas';

const
  Status = #13'Se han procesado %d ficheros de %d (%d%%) en %d minutos ...';

var
  params: TsNIFParametros;
  control: TsNIFControl;
  c: integer;

begin
  try

    WriteLn('Comenzando');

    // Parametros de prueba
    params := TsNIFParametros.Create;
    params.Extensiones.Add('*');
    params.Carpetas.Add('C:\temp\sniftest');
    params.Carpetas.Add('C:\temp\');
    params.Carpetas.Add('C:\temp\');
    params.Carpetas.Add('c:\???pero que???');
    params.FicheroSalida := 'c:\temp\snif.csv';
    params.GenerarResumen := true;
    params.IncluirErrores := true;
    params.NumHilos := 1;
    params.comprobarCoherencia;


    control := TsNIFControl.Create;

    control.Iniciar(params);
    c := 0;
    repeat
      Write(Format(Status, [control.CuentaFicherosExplorados,
        control.CuentaTotalFicheros, control.Progreso,
        control.MinutosDeTrabajo]));
      if (c = 2) then
        control.cancelar
      else
        inc(c);
    until control.haTerminado;
    WriteLn('Finalizado');

    readln;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
