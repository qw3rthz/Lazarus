unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,synaser;

type

  { TForm1 }

  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    procedure FormCreate(Sender: TObject);
    function GetSerialPortNames: string;
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  combobox1.Items.CommaText := GetSerialPortNames;
end;

function TForm1.GetSerialPortNames: string;
var
  Index: Integer;
  Data: string;
  TmpPorts: String;
  sr : TSearchRec;

  procedure ScanForPorts( const ThisRootStr : string); // added by PDF
  begin
    if FindFirst( ThisRootStr, $FFFFFFFF, sr) = 0 then
    begin
      repeat
        if (sr.Attr and $FFFFFFFF) = Sr.Attr then
        begin
          data := sr.Name;
          index := length(data);
//        while (index > 1) and (data[index] <> '/') do  *** doesn't work
          while (index > 0) and (data[index] <> '/') do
            index := index - 1;
//        TmpPorts := TmpPorts + ' ' + copy(data, 1, index + 1);  *** doesn't work
          TmpPorts := TmpPorts + ' ' + copy(data, index + 1, length( data) - index);
        end;
      until FindNext(sr) <> 0;
    end;
  end;

begin
  try
    TmpPorts := '';
    ScanForPorts( '/dev/ttyS*');
    ScanForPorts( '/dev/ttyA*');
    ScanForPorts( '/dev/ttyUSB*');
    ScanForPorts( '/dev/ttyAM*'); // for ARM board
    FindClose(sr);
  finally
    Result:=TmpPorts;
  end;
end;
end.

