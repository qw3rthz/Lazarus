unit qLibSystem;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF Win32} windows,{$ENDIF}
  Classes, SysUtils, Resource, ElfReader, VersionResource, Process;

function GetProgramVersion(FileName: string = ''): String;
function GetThisComputerName: string;
//returns number of cores: a computer with two hyperthreaded cores will report 4
function GetLogicalCpuCount: Integer;

implementation

{$IFDEF UNIX}
{$IFDEF Darwin}
uses Process,SysUtils,Controls,classes;

function GetLogicalCpuCount: Integer;
//returns number of CPUs for MacOSX computer
//requires Process in Uses Clause
//see http://wiki.lazarus.freepascal.org/Executing_External_Programs
var
   lProcess: TProcess;
   lLen,lPos: integer;
   lStr: string;
   lStringList: TStringList;
begin
     Result := 1;
     lProcess := TProcess.Create(nil);
     lStringList := TStringList.Create;
     lProcess.CommandLine := 'sysctl hw.ncpu';
     lProcess.Options := lProcess.Options + [poWaitOnExit, poUsePipes];
     lProcess.Execute;
     lStringList.LoadFromStream(lProcess.Output);
     lLen := length(lStringList.Text);
     if lLen > 0 then begin
        lStr := '';
        for lPos := 1 to lLen do
            if lStringList.Text[lPos] in ['0'..'9'] then
               lStr := lStr + lStringList.Text[lPos];
        if length(lStr) > 0 then
           result := strtoint(lStr);
     end;//if at least one character returned
     if result < 1 then //just incase there is a horrible error, e.g. 0
        result := 1;
     lStringList.Free;
     lProcess.Free;
end;
{$ELSE} //Not Darwin ... Assume Linux
function GetLogicalCpuCount: Integer;
var lS: TStringList;
    lFilename: string;
    lLine,lnLines: integer;
begin
     result := 1;
     lFilename := '/proc/cpuinfo';
     if not fileexists(lFilename) then exit;
     lS:= TStringList.Create;
     lS.LoadFromFile(lFilename);
     lnLines := lS.Count;
     if lnLines > 0 then begin
        result := 0;
        for lLine := 1 to lnLines do
            if lS[lLine-1] = '' then
               inc(result);
     end;
     if result < 1 then
        result := 1;
     lS.Free;
end;
{$ENDIF} //If Darwin Else Linux

{$ELSE} //If UNIX ELSE NOT Unix - assume Windows
uses Windows;
function GetLogicalCpuCount: Integer;
var
  SystemInfo: _SYSTEM_INFO;
begin
  GetSystemInfo(SystemInfo);
  Result := SystemInfo.dwNumberOfProcessors;
end;
{$ENDIF}

function GetProgramVersion(FileName: string = ''): String;
Var
   RS : TResources;
   E : TElfResourceReader;
   VR : TVersionResource;
   i : Integer;
begin
   result := '';
   RS := TResources.Create;
   try
     E := TElfResourceReader.Create;
     try
       if FileName = '' then
         Rs.LoadFromFile(ParamStr(0),E)
       else
         Rs.LoadFromFile(FileName,E);
     except
         result := '0';
         E.Free;
         exit;
     end;
     E.Free;
     VR := Nil;
     i := 0;
     While (VR=Nil) and (i<RS.Count) do
       begin
       if RS.Items[i] is TVersionResource then
          VR := TVersionResource(RS.Items[i]);
       Inc(i);
       end;
     if VR<>Nil then
       For i := 0 to 3 do
         begin
         If (result<>'') then
           result := result+'.';
         result := result+IntToStr(VR.FixedInfo.FileVersion[i]);
         end;
   Finally
     RS.FRee;
   end;
end;

function GetThisComputerName: string;
var
{$IFDEF Win32}
  c: array[0..127] of Char;
  sz: dword;
{$ENDIF}
  AProcess: TProcess;
  AStringList: TStringList;

begin
{$IFDEF Win32}
  sz := SizeOf(c);
  GetComputerName(c, sz);
  Result := c;
{$ELSE}
  AProcess := TProcess.Create(nil);
  AStringList := TStringList.Create;
  AProcess.Executable := 'hostname';
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  AProcess.Execute;
  AStringList.LoadFromStream(AProcess.Output);
  Result:=AStringList.Strings[0];
  AStringList.Free;
  AProcess.Free;
{$ENDIF}
end;

end.

