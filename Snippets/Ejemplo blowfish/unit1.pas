unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  blowfish, base64;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    edClave: TEdit;
    edCadena: TEdit;
    Label1: TLabel;
    lbl1: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  en: TBlowFishEncryptStream;
  de: TBlowFishDeCryptStream;
  s1,s2: TStringStream;
  key,value,temp: String;
begin
  key := edClave.Text;
  value := edCadena.Text;
  { 2 }
  s1 := TStringStream.Create('');
  en := TBlowFishEncryptStream.Create(key,s1);
  { 3 }
  en.WriteAnsiString(value);
  en.Free;
  memo1.Lines.Add('encrypted: ' + EncodeStringBase64(s1.DataString));
  { 4 }
  s2 := TStringStream.Create(s1.DataString);
  s1.Free;
  { 5 }
  de := TBlowFishDeCryptStream.Create(key,s2);
  { 6 }
  temp := de.ReadAnsiString;
  memo1.Lines.Add('decrypted: ' + temp);
  de.Free;
  s2.Free;

end;

end.

