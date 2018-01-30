unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, janSpeller;

type

  { TfMain }

  TfMain = class(TForm)
    btnOpen: TButton;
    btnSpell: TButton;
    Speller: TjanSpeller;
    Memo1: TMemo;
    OpenDlg: TOpenDialog;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    procedure btnOpenClick(Sender: TObject);
    procedure btnSpellClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fMain: TfMain;

implementation

{ TfMain }

procedure TfMain.btnOpenClick(Sender: TObject);
begin
 if OpenDlg.Execute then
 Memo1.Lines.LoadFromFile(OpenDlg.FileName);
end;

procedure TfMain.btnSpellClick(Sender: TObject);
var
 Memo: string;
begin
 if Memo1.Lines.Text = '' then
   begin
   MessageDlg('The page is empty, unable to operate the spell check!',
     mtWarning, [mbOK], 0);
   Exit;
   end;
 with Speller do
   begin
   Memo := Memo1.Lines.Text;
   Spell(Memo);
   Memo1.Lines.Text := Memo;
   MessageDlg('Spell Check Of ' + OpenDlg.FileName + ' Complete',
     mtInformation, [mbOK], 0);
   end;

end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  Speller.LoadDictionary(ExtractFilePath(Application.ExeName)+'dictionaries/english.dic');
  Speller.LoadUserDictionary(ExtractFilePath(Application.ExeName)+'dictionaries/user.dic');
end;

initialization
  {$I main.lrs}

end.

