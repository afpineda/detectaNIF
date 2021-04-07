unit sNIF.worker.candidatos;

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
  sNIF.worker,
  sNIF.params,
  sNIF.control;

type
  TWorkerCandidatos = class(TsNIFWorker)
    {
      PROPOSITO:

      Explora recursivamente las carpetas indicadas en FsNIFParametros.
      Para cada fichero, comprueba si cumple las condiciones indicadas en
      FsNIFParametros. En caso positivo, inserta el fichero en la cola
      de trabajo. Solamente es necesaria una instancia. De lo contrario,
      se duplicaría el trabajo.
    }
  private
    FPilaCarpetas: TStringList;
    function comprobarExtension(const fichero: string): boolean;
  protected
    procedure Execute; override;
  end;

implementation

uses
  sNIF.data,
  IOUtils,
  IOUtilsFix,
  System.StrUtils,
  System.SysUtils,
  System.DateUtils,
  windows,
  ThreadUtils.SafeDataTypes;

// --------------------------------------------------------------------------
// Hilo de trabajo
// --------------------------------------------------------------------------

{$WARN SYMBOL_PLATFORM OFF}

procedure TWorkerCandidatos.Execute;
var
  carpeta, ext: string;
  srec: TSearchRec;
  candidato: TsNIFData;
  auxDate: TSystemTime;
  fecha, hoy: TDateTime;
  abortar: boolean;
begin
  // Crear pila local de carpetas a explorar inicializada con las carpetas
  // indicadas en los parámetros
  FPilaCarpetas := TStringList.Create;
  FPilaCarpetas.Assign(FParametros.Carpetas);

  hoy := Now;
  abortar := false;
  // Recorrer cola local de carpetas
  while (not Terminated) and (FPilaCarpetas.Count > 0) do
  begin
    // sacar carpeta de la pila local
    carpeta := FPilaCarpetas.Strings[FPilaCarpetas.Count - 1];
    FPilaCarpetas.Delete(FPilaCarpetas.Count - 1);
    FControl.incCuentaCarpetas;

    // buscar ficheros y subcarpetas
    if (FindFirst(TPath.SetExtendedPrefix(TPath.Combine(carpeta, '*')),
      faAnyFile, srec) = 0) then
    begin
      repeat
        if (srec.Name <> '.') and (srec.Name <> '..') then
          try
            if ((srec.Attr and faDirectory) > 0) and (not Terminated) then
              // añadir directorio a la pila local de carpetas
              FPilaCarpetas.Add(TPath.Combine(carpeta, srec.Name))
            else if ((srec.Attr and faAnyFile) > 0) and (not Terminated) then
            begin
              // comprobar si es elegible como candidato
              ext := LowerCase(ReplaceStr(TPath.GetExtension(srec.Name),
                '.', ''));
              FileTimeToSystemTime(srec.FindData.ftCreationTime, auxDate);
              fecha := SystemTimeToDateTime(auxDate);
              if (srec.Size >= FParametros.MinBytes) and
                (Abs(DaysBetween(hoy, fecha)) >= FParametros.MinDiasCreado) and
                (comprobarExtension(srec.Name)) then
              begin
                // añadir fichero a cola de trabajo
                FileTimeToSystemTime(srec.FindData.ftLastAccessTime, auxDate);
                candidato := TsNIFData.Create
                  (IncludeTrailingPathDelimiter(carpeta), srec.Name,
                  SystemTimeToDateTime(auxDate));
                try
                  FControl.ColaCandidatos.Enqueue(candidato);
                  FControl.incCuentaTotalFicheros;
                except
                  // La cola se ha abandonado
                  abortar := true;
                  Terminate;
                end;
              end;
            end;
          except
            // error en acceso a carpeta (ignorar)
            FControl.incCuentaErroresCarpeta;
          end;
      until (Terminated) or (FindNext(srec) <> 0);
      System.SysUtils.FindClose(srec);
    end
    else
      // La carpeta no existe (ignorar)
      FControl.incCuentaErroresCarpeta;
  end; // while
  if (not abortar) then
    // Cerrar cola
    FControl.ColaCandidatos.Close;
  // Liberar recursos
  FPilaCarpetas.Free;
end;

function TWorkerCandidatos.comprobarExtension(const fichero: string): boolean;
var
  i: integer;
begin
  Result := false;
  i := 0;
  while (not Result) and (i < FParametros.Extensiones.Count) do
  begin
    Result := TPath.MatchesPattern(fichero,
      '*.' + FParametros.Extensiones.Strings[i], false);
    inc(i);
  end;
end;

end.
