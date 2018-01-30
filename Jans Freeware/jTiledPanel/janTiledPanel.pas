unit janTiledPanel;

{$MODE Delphi}{$H+}

interface

uses
  Interfaces, LCLIntf, LCLType, LCLProc, LResources, LMessages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type

  { TjanTiledPanel }

  TjanTiledPanel = class(TPanel)
  private
    FBackBitmap: TBitmap;
    FFrameWidth:integer;
    procedure SetFrameWidth(const AValue: Integer);
    procedure SetBackBitmap(const Value: TBitmap);
    procedure BackBitmapChanged(sender:Tobject);
    { Private declarations }
  protected
    { Protected declarations }
    procedure paint; override;
  public
    { Public declarations }
    property FrameWidth      : Integer            read FFrameWidth     write SetFrameWidth;
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy; override;
  published
    { Published declarations }

    property BackBitmap:TBitmap read FBackBitmap write SetBackBitmap;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('janBasic', [TjanTiledPanel]);
end;




{ TjanTiledPanel }

procedure TjanTiledPanel.BackBitmapChanged(sender:Tobject);
begin
  invalidate;
end;

constructor TjanTiledPanel.Create(AOwner: TComponent);
begin
  inherited;
  FBackBitmap:=Tbitmap.create;
  FBackBitmap.OnChange:=BackBitmapChanged;
  FFrameWidth:=1;
end;

destructor TjanTiledPanel.Destroy;
begin
  FBackBitmap.free;
  inherited;

end;

procedure TjanTiledPanel.paint;
var
  ix, iy: Integer;
  BmpWidth, BmpHeight: Integer;
  BmpCanvas: THandle;
  hCanvas:HDC;
  bm:Tbitmap;
  R:Trect;
begin
    canvas.font.assign(font);
    bm:=FBackBitmap;
    if assigned(bm) and (bm.Height <> 0) and
        (bm.Width <> 0) then
      begin
        BmpWidth := bm.Width;
        BmpHeight := bm.Height;
        BmpCanvas := bm.Canvas.Handle;
        for iy := 0 to ClientHeight div BmpHeight do
          for ix := 0 to ClientWidth div BmpWidth do
            BitBlt(Canvas.handle, ix * BmpWidth, iy * BmpHeight,
              BmpWidth, BmpHeight, BmpCanvas,
              0, 0, SRCCOPY);
        R:=clientRect;
        drawText(canvas.handle,@caption[1],-1,R,DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        R:=rect(0,0,width,height);
        Frame3D(canvas.Handle,R,FFRameWidth,bvSpace);
      end
      else
       inherited;
end;

procedure TjanTiledPanel.SetFrameWidth(const AValue: Integer);
begin
  if (AValue <> FFrameWidth) and (AValue in [0..10]) then
  begin
    FFrameWidth := AValue;
    Self.Invalidate;
  end;
end;

procedure TjanTiledPanel.SetBackBitmap(const Value: TBitmap);
begin
  FBackBitmap.assign(Value);
end;

initialization
{$I janTiledPanel.lrs}







end.
