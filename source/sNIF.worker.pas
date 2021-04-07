unit sNIF.worker;

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
  sNIF.control,
  System.SysUtils,
  sNIF.params;

type
  TsNIFWorker = class(TThread)
    {
      PROPOSITO:

      Clase base para los hilos que realizan el trabajo de exploración.
    }
  protected
    FControl: TsNIFControl;
    FParametros: TsNIFParametros;
  public
    constructor Create(const AControl: TsNIFControl;
      const Parametros: TsNIFParametros);
  end;

implementation

//uses
//  System.SysUtils;

constructor TsNIFWorker.Create(const AControl: TsNIFControl;
  const Parametros: TsNIFParametros);
begin
  inherited Create(true); // crear suspendido
  FControl := AControl;
  FParametros := Parametros;
  FreeOnTerminate := false;
end;

end.
