unit qLibDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

function SQLiteLib: string;


implementation


function SQLiteLib: string;
var
  libSQLite: TStringList;
begin
  result := '';
{$ifdef unix}
  libSQLite := FindAllFiles('/usr/lib', 'libsqlite3.so.0', True);
{$else}
  libSQLite := FindAllFiles(GetEnvironmentVariableUTF8('WINDIR'), 'sqlite3.dll', True);
{$endif}
  if libSQLite.Text <> '' then begin
    result := libSQLite[0];
  end;
  FreeAndNil(libSQLite);
end;

end.

