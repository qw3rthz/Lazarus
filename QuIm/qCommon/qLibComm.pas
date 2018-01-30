unit qLibComm;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

function GPStoDegrees(const Coord: string): string;

implementation

function GPStoDegrees(const Coord: string): string;
var
  DegStr, MinStr: string;
  i: Integer;
  Deg, Min: Double;
begin
  i := Pos('.', Coord);
  DegStr := Copy(Coord, 1, i-3);
  MinStr := Copy(Coord, i-2, 10);    // Accomodates 3 or more decimal places
  Deg := StrToFloat(DegStr);
  Min := StrToFloat(MinStr);
  Result := FloatToStrF(Deg + Min/60, ffNumber,0, 7);
end;

end.

