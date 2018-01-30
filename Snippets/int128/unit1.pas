unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Int128;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowResult(AValue: UInt128);
  end;

var
  Form1: TForm1;

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowResult(StrToInt128(Edit1.Text) + StrToInt128(Edit2.Text));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShowResult(StrToInt128(Edit1.Text) - StrToInt128(Edit2.Text));
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ShowResult(StrToInt128(Edit1.Text) * StrToInt128(Edit2.Text));
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  ShowResult(StrToInt128(Edit1.Text) div StrToInt128(Edit2.Text));
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  ShowResult(StrToInt128(Edit1.Text) mod StrToInt128(Edit2.Text));
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  ShowResult(Power128(StrToInt128(Edit1.Text),StrToInt128(Edit2.Text)));
end;

procedure TForm1.ShowResult(AValue: UInt128);
begin
  Edit3.Text := Int128ToStr(AValue);
  Edit4.Text := Int128ToHex(AValue);
end;

{$R *.lfm}

end.

