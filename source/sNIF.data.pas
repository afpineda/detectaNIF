unit sNIF.data;
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
  System.SysUtils,
  ThreadUtils.SafeDataTypes;

type
  {
    PROPOSITO:

    Datos de cada fichero candidato y resultado de su exploración
  }
  TsNIFData = class
    Ruta: string;
    Fichero: string;
    error: boolean;
    mensajeError: string;
    FechaUltimoAcceso: TDateTime;
    CuentaNIF: cardinal;

    constructor Create(const ARuta, AFichero: string;
      const AFechaUltimoAcceso: TDateTime);

  end;

  // --------------------------------------------------------------------------
  TColaResultados = TThreadSafeQueue<TsNIFData>;
  TColaCandidatos = TColaResultados;
  // --------------------------------------------------------------------------

implementation

constructor TsNIFData.Create(const ARuta, AFichero: string;
  const AFechaUltimoAcceso: TDateTime);
begin
  Ruta := ARuta;
  Fichero := AFichero;
  FechaUltimoAcceso := AFechaUltimoAcceso;
  CuentaNIF := 0;
  error := false;
  mensajeError := '';
end;

end.
