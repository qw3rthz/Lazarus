program PhoneBookDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  pbMainUnit;

begin
  Application.Initialize;
  Application.CreateForm(TpbMainForm, pbMainForm);
  Application.Run;
end.

