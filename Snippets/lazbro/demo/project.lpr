program project;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unitmain, Lazbro;

{$R *.res}

begin
  Application.Title:='Lazbro - Portable HTML Browser';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

