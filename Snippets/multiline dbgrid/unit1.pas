unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  BufDataset, db, DbCtrls, IBConnection, sqldb, Grids, ExtCtrls, Buttons,
  VirtualTrees, vte_treedata;

type

  { TForm1 }

  TForm1 = class(TForm)
    Datasource1 : TDatasource;
    DBGrid1 : TDBGrid;
    DBMemo1 : TDBMemo;
    DBNavigator1 : TDBNavigator;
    IBConnection1 : TIBConnection;
    SQLQuery1 : TSQLQuery;
    SQLTransaction1 : TSQLTransaction;
    procedure BitBtn1Click(Sender : TObject);
    procedure DBGrid1ColumnSized(Sender : TObject);
    procedure DBGrid1DrawColumnCell(Sender : TObject; const Rect : TRect;
      DataCol : Integer; Column : TColumn; State : TGridDrawState);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
  private
    { private declarations }
    FRowHeight : Integer;
    Function GetMaxRowHeight:Integer;
    Procedure SetRowHeight(aNewHeight:Integer);
  public
    { public declarations }

  end;

var
  Form1 : TForm1;

implementation
uses types, LCLIntf, LCLType;
{$R *.lfm}

function WrapText(Canvas: TCanvas; const Text: string; MaxWidth: integer): string;
var
  DC: HDC;
  TextExtent: TSize;
  S, P, E: PChar;
  Line: string;
  IsFirstLine: boolean;
begin
  Result := '';
  DC := Canvas.Handle;
  IsFirstLine := True;
  P := PChar(Text);
  while P^ = ' ' do
    Inc(P);
  while P^ <> #0 do
  begin
    S := P;
    E := nil;
    while (P^ <> #0) and (P^ <> #13) and (P^ <> #10) do
    begin
      LCLIntf.GetTextExtentPoint(DC, S, P - S + 1, TextExtent);
      if (TextExtent.CX > MaxWidth) and (E <> nil) then
      begin
        if (P^ <> ' ') and (P^ <> ^I) then
        begin
          while (E >= S) do
            case E^ of
              '.', ',', ';', '?', '!', '-', ':',
              ')', ']', '}', '>', '/', '\', ' ':
                break;
              else
                Dec(E);
            end;
          if E < S then
            E := P - 1;
        end;
        Break;
      end;
      E := P;
      Inc(P);
    end;
    if E <> nil then
    begin
      while (E >= S) and (P^ = ' ') do
        Dec(E);
    end;
    if E <> nil then
      SetString(Line, S, E - S + 1)
    else
      SetLength(Line, 0);
    if (P^ = #13) or (P^ = #10) then
    begin
      Inc(P);
      if (P^ <> (P - 1)^) and ((P^ = #13) or (P^ = #10)) then
        Inc(P);
      if P^ = #0 then
        Line := Line + LineEnding;
    end
    else if P^ <> ' ' then
      P := E + 1;
    while P^ = ' ' do
      Inc(P);
    if IsFirstLine then
    begin
      Result := Line;
      IsFirstLine := False;
    end
    else
      Result := Result + LineEnding + Line;
  end;
end;

{ TForm1 }
function QueryTextRect(const vCanvas:TCanvas; const aText:String; const MaxRect: TRect):TRect;
const
  DrawTextFlags = DT_NOPREFIX or DT_EDITCONTROL or DT_CALCRECT;
begin
  if (aText <> '') then
  begin
    //Canvas.Font := aFont;
    Result := MaxRect;
    LCLIntf.DrawText(vCanvas.Handle, PChar(aText), Length(aText),  //calculate the text's rectangle required for painting.
      Result, DrawTextFlags);
  end
  else
    FillChar(Result, SizeOf(Result), 0);
end;

procedure TForm1.FormCreate(Sender : TObject);
begin
  //if not FileExists(BufDataset1.FileName) then BufDataset1.CreateDataset else BufDataset1.Active := True;
  IBConnection1.Connected := True;
  SQLQuery1.Active := True;
end;

procedure TForm1.DBGrid1DrawColumnCell(Sender : TObject; const Rect : TRect;
  DataCol : Integer; Column : TColumn; State : TGridDrawState);
var
  textstyle: ttextstyle;
  i,x: integer;
  examine: string;
  TextRect, MaxRect: Trect;
begin
  DBGrid1.Canvas.FillRect(Rect);
  TextStyle := Canvas.TextStyle;
  TextStyle.Wordbreak := True;
  TextStyle.SingleLine := false;
  examine := WrapText(DBGrid1.Canvas, DBGrid1.DataSource.DataSet.Fields[DataCol].AsString, (Rect.Right - Rect.Left) - 2);
  if DataCol = 2 then begin
    MaxRect := classes.Rect(rect.Left,rect.right,0,4096);
    TextRect := querytextrect(dbgrid1.canvas, examine, MaxRect);
    x := (TextRect.Bottom - TextRect.Top) + 3;
    SetRowHeight(x);
  end;
  DBGrid1.Canvas.TextRect(Rect, Rect.Left+1, Rect.Top+1, examine, textstyle);
end;

procedure TForm1.BitBtn1Click(Sender : TObject);
begin
  SQLQuery1.ApplyUpdates;
end;

procedure TForm1.DBGrid1ColumnSized(Sender : TObject);
var
  vNewSize : Integer;
begin
  vNewSize := GetMaxRowHeight;
  FRowHeight := 0;
  SetRowHeight(vNewSize);
end;

procedure TForm1.FormDestroy(Sender : TObject);
begin
   //BufDataset1. ApplyUpdates;
  SQLQuery1.ApplyUpdates;
  SQLQuery1.Close;
  IBConnection1.Connected := False;
end;

Function TForm1.GetMaxRowHeight : Integer;
var
  MaxRect  : TRect;
  TextRect : TRect;
  bkm      : TBookmark;
  examine  : String;
  FinalSize,
  x, i     : Integer;
begin
  SQLQuery1.DisableControls;
  try
    bkm := SQLQuery1.GetBookmark;
    SQLQuery1.First;
    repeat
      MaxRect := classes.Rect(0, DBGrid1.Columns[2].Width,0,4096);
      examine := SQLQuery1.fields[2].AsString;
      examine := WrapText(DBGrid1.Canvas,examine,dbgrid1.Columns[2].Width);
      TextRect := querytextrect(dbgrid1.canvas, examine, MaxRect);
      x := (TextRect.Bottom - TextRect.Top) + 3;
      if FinalSize < x then FinalSize := x;
      SQLQuery1.Next;
    until SQLQuery1.EOF;
    SQLQuery1.GotoBookmark(bkm);
    SQLQuery1.FreeBookmark(bkm);

    Result := FinalSize;
  finally
    SQLQuery1.EnableControls;
  end;
end;

Procedure TForm1.SetRowHeight(aNewHeight : Integer);
var
  Cntr : Integer;
begin
  if aNewHeight < FRowHeight then Exit;
  for Cntr := 2 to TStringGrid(DBGrid1).RowCount do
    TStringGrid(dbgrid1).RowHeights[Cntr-1] := aNewHeight;
  FRowHeight := aNewHeight;
end;

end.

