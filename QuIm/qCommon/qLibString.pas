unit qLibString;
{$mode objfpc}{$H+}

interface

uses
  SysUtils, md5;

function StrToStrDec(str: string; nDec: integer; truncar: boolean = true):string;
function HashStr(str: string):string;

implementation

function StrToStrDec(str: string; nDec: integer; truncar: boolean = true):string;
var
  decimalPos: integer;
  dRed: integer;
begin
  result := str;
  decimalPos := pos(FormatSettings.DecimalSeparator, str);
  if decimalPos > 0 then begin
    result := LeftStr(str, Length(str) - ((Length(str) - decimalPos) - nDec));
    if nDec = 0 then
      result := LeftStr(result, Length(result) - 1);
    if not truncar then begin
      if StrToInt(Copy(str, decimalPos + nDec + 1, 1)) > 4 then begin
        dRed := StrToInt(RightStr(result, 1));
        inc(dRed);
        result := LeftStr(result, Length(result) - 1) + IntToStr(dRed);
      end;
    end;
  end;
end;

function HashStr(str: string):string;
begin
  result := MD5Print(MD5String(str));
end;

end.

