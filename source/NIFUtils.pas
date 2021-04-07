unit NIFUtils;
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

const
  BYTES_PER_NIF = 18; // UTF-16 encoded

function isNIF_UTF8(const data: PByte): boolean;
function isNIF_UTF16(const data: PByte): boolean;

implementation

// uses
// SysUtils;

var
  NIFTable: array [0 .. 22] of byte;

function isAsciiDigit(chr: byte): boolean;
begin
  result := (chr >= 48) and (chr <= 57)
end;

function isAsciiCapitalLetter(chr: byte): boolean;
begin
  result := (chr >= 65) and (chr <= 90);
end;

function ControlLetter(Value: integer): byte;
var
  R: integer;
begin
  R := (Value mod 23);
  result := NIFTable[R];
end;

function isNIF(chr0, chr1, chr2, chr3, chr4, chr5, chr6, chr7, chr8: byte)
  : boolean; overload;
var
  val: integer;
begin
  result := isAsciiDigit(chr1) and isAsciiDigit(chr2) and isAsciiDigit(chr3) and
    isAsciiDigit(chr4) and isAsciiDigit(chr5) and isAsciiDigit(chr6) and
    isAsciiDigit(chr7);
  if (result) then
  begin
    val := (chr7 - 48) + ((chr6 - 48) * 10) + ((chr5 - 48) * 100) +
      ((chr4 - 48) * 1000) + ((chr3 - 48) * 10000) + ((chr2 - 48) * 100000) +
      ((chr1 - 48) * 1000000);
    if isAsciiDigit(chr0) then
      // DNI
      result := (chr8 = ControlLetter(val + ((chr0 - 48) * 10000000)))
    else if (chr0 >= byte('X')) and (chr0 <= byte('Z')) then
      // NIE
      result := (chr8 = ControlLetter(val + ((chr0 - byte('X')) * 10000000)))
    else if (chr0 >= byte('K')) and (chr0 <= byte('M')) then
      // NIF no es un DNI ni un NIE
      result := (chr8 = ControlLetter(val))
    else
      result := false;
  end;
end;

function isNIF_UTF8(const data: PByte): boolean;
begin
  result := isNIF(data[0], data[1], data[2], data[3], data[4], data[5], data[6],
    data[7], data[8]);
end;

function isNIF_UTF16(const data: PByte): boolean;
begin
  result := ((data[1] = 0) and (data[3] = 0) and (data[5] = 0) and (data[7] = 0)
    and (data[9] = 0) and (data[11] = 0) and (data[13] = 0) and (data[15] = 0)
    and (data[17] = 0) and isNIF(data[0], data[2], data[4], data[6], data[8],
    data[10], data[12], data[14], data[16]));
end;

initialization

NIFTable[0] := byte('T');
NIFTable[1] := byte('R');
NIFTable[2] := byte('W');
NIFTable[3] := byte('A');
NIFTable[4] := byte('G');
NIFTable[5] := byte('M');
NIFTable[6] := byte('Y');
NIFTable[7] := byte('F');
NIFTable[8] := byte('P');
NIFTable[9] := byte('D');
NIFTable[10] := byte('X');
NIFTable[11] := byte('B');
NIFTable[12] := byte('N');
NIFTable[13] := byte('J');
NIFTable[14] := byte('Z');
NIFTable[15] := byte('S');
NIFTable[16] := byte('Q');
NIFTable[17] := byte('V');
NIFTable[18] := byte('H');
NIFTable[19] := byte('L');
NIFTable[20] := byte('C');
NIFTable[21] := byte('K');
NIFTable[22] := byte('E');

end.
