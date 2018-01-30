program speller_demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, jspeller, LResources
  { you can add units after this };

{$IFDEF WINDOWS}{$R speller_demo.rc}{$ENDIF}

begin
  {$I speller_demo.lrs}
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.

