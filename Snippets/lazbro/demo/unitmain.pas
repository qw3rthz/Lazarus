(*

Title:    Lazbro Demo App
Author:   Derek John Evans (derek.john.evans@hotmail.com)
Website:  http://www.wascal.net/

Copyright (C) 2013-2014 Derek John Evans
http://www.wascal.net/

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated 
documentation files (the "Software"), to deal in the Software without restriction, including without 
limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies 
of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following 
conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial 
portions of the Software. 

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT 
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN 
NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE 
OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*)

unit unitmain;

interface

uses
  Buttons, ComCtrls, Controls, ExtCtrls, FileUtil, Forms, Lazbro, StdCtrls, UriParser;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonForward: TSpeedButton;
    EditURL: TEdit;
    PageControl: TPageControl;
    LabelAddress: TPanel;
    PanelClient: TPanel;
    PanelTop: TPanel;
    ButtonBack: TSpeedButton;
    ButtonRefresh: TSpeedButton;
    StatusBar: TStatusBar;
    TabSheetBrowser: TTabSheet;
    procedure ButtonBackClick(Sender: TObject);
    procedure ButtonForwardClick(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
    procedure FormCreate(ASender: TObject);
    procedure FormShow(ASender: TObject);
  private
    FLazbro: TLazbro;
  private
    procedure DoOnChangeURI(const ALazbro: TLazbro);
    procedure DoOnDocumentComplete(const ALazbro: TLazbro);
    procedure DoOnStatusTextChanged(const ALazbro: TLazbro);
  public
    procedure SetLocation(const ALocation: String);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

procedure TFormMain.FormCreate(ASender: TObject);
begin
  Caption := Application.Title;

  FLazbro := TLazbro.Create(Self);
  //FLazbro.ShowBorders:=True;
  FLazbro.Align := alClient;
  FLazbro.Parent := TabSheetBrowser;
  FLazbro.OnUrlChange := @DoOnChangeURI;
  FLazbro.OnComplete := @DoOnDocumentComplete;
  FLazbro.OnStatusChange := @DoOnStatusTextChanged;
end;

procedure TFormMain.FormShow(ASender: TObject);
begin
  SetLocation(ProgramDirectory + 'html' + DirectorySeparator + 'index.html');
end;

procedure TFormMain.SetLocation(const ALocation: String);
begin
  if FilenameIsAbsolute(ALocation) then
  begin
    EditURL.Text := FilenameToURI(ALocation);
  end else begin
    EditURL.Text := ALocation;
  end;
  FLazbro.LoadFromUrl(EditURL.Text);
  FLazbro.SetFocus;
end;

procedure TFormMain.DoOnDocumentComplete(const ALazbro: TLazbro);
begin
  if ALazbro = FLazbro then
  begin
    TabSheetBrowser.Caption := ALazbro.Document.Title;
  end;
end;

procedure TFormMain.DoOnStatusTextChanged(const ALazbro: TLazbro);
begin
  StatusBar.SimpleText := ALazbro.StatusText;
  StatusBar.Refresh;
end;

procedure TFormMain.DoOnChangeURI(const ALazbro: TLazbro);
begin
  EditURL.Text := ALazbro.BaseUrl;
end;

procedure TFormMain.ButtonBackClick(Sender: TObject);
begin
  FLazbro.GoBack;
end;

procedure TFormMain.ButtonForwardClick(Sender: TObject);
begin
  FLazbro.GoForward;
end;

procedure TFormMain.ButtonRefreshClick(Sender: TObject);
begin
  FLazbro.Refresh;
end;

end.
