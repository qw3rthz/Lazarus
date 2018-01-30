unit janPanelButton;

{$MODE Delphi}{$H+}

interface

uses
  Interfaces, LCLIntf, LCLType, LCLProc, LResources, ComponentEditors, PropEdits, LMessages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TPanelPosition=(ppPanelDown,ppPanelRight);

  { TjanPanelButton }

  TjanPanelButton = class(TGraphicControl)
  private
    FPanel:TPanel;
    FPushDown: boolean;
    FMouseOver: boolean;
    FHotTrack: boolean;
    FColor: TColor;
    FHotColor: TColor;
    FHotFontColor: Tcolor;
    FMarginHorizontal: integer;
    FAlignment: Talignment;
    FMarginVertical: integer;
    FDownColor: TColor;
    FDownFontColor: TColor;
    FFlat: boolean;
    FPanelPosition: TPanelPosition;
    FGlyph: TBitmap;
    FPanelOffset: integer;
    FFrameWidth:integer;
    procedure SetFrameWidth(const AValue: Integer);
    procedure SetHotTrack(const Value: boolean);
    procedure SetColor(const Value: TColor);
    procedure SetHotColor(const Value: TColor);
    procedure SetHotFontColor(const Value: Tcolor);
    procedure SetMarginHorizontal(const Value: integer);
    procedure SetAlignment(const Value: Talignment);
    procedure SetMarginVertical(const Value: integer);
    procedure SetDownColor(const Value: TColor);
    procedure SetDownFontColor(const Value: TColor);
    procedure SetFlat(const Value: boolean);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure SetPanel(const Value: TPanel);
    procedure AutoFit;
    procedure SetPanelPosition(const Value: TPanelPosition);
    procedure SetGlyph(const Value: TBitmap);
    procedure SetPanelOffset(const Value: integer);
    procedure CustomClick(softclick: boolean);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
    procedure CMMouseLeave(var Message: TLMessage); message CM_MouseLeave;
    procedure CMMouseEnter(var Message: TLMessage); message CM_MouseEnter;
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TLMessage); message CM_TEXTCHANGED;
    procedure GlyphChanged(sender:TObject);

  public
    {With TjanPanelButton you associate a TPanel that will be dropped down/hidden when clicking the button.}
    {}
    {The panel can contain any control including TjanPanelButton instances.}
    {}
    {Closing the panel will also close any panels associated with TjanPanelButtons on subpanels.}
    {}
    {Using this component you can create toolbar menus with any control on it: listboxes, editors etc.}
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    {Creates and initializes an instance of TjanPanelButton. }
    destructor Destroy; override;
    {Destroys an instance of TjanPanelButton.}
    procedure Paint; override;
    {Renders the image of the button.}
  published
    property FrameWidth      : Integer            read FFrameWidth     write SetFrameWidth;
    { Published declarations }
    property Panel:TPanel read FPanel write SetPanel;
    {Determines the panel associated with the component}
    property PanelPosition:TPanelPosition read FPanelPosition write SetPanelPosition;
    {Determines if the panel will be displayed below or to the right of the component.}
    property PanelOffset:integer read FPanelOffset write SetPanelOffset;
    {Determines the extra space in pixels between the component and the panel.}
    property Glyph:TBitmap read FGlyph write SetGlyph;
    {Determines the images that appears on the button.}
    property Align;
    {Determines how the control aligns within its container (parent control).}
    property Alignment: Talignment read FAlignment write SetAlignment;
    {Controls the horizontal placement of the text within the button.}
    property Caption;
    {Specifies a text string that identifies the control to the user.}
    property Constraints;
    {Specifies the size constraints for the control.}
    {}
    {Use Constraints to specify the minimum and maximum width and height of the control. When Constraints contains maximum or minimum values, the control can’t be resized to violate those constraints.}
    property Enabled;
    {Controls whether the control responds to mouse, keyboard, and timer events. }
    property Flat: boolean read FFlat write SetFlat;
    {Determines whether the button has a 3D border that provides a raised or lowered look.}
    property Font;
    {Controls the attributes of text written on the button.}
    property MarginHorizontal: integer read FMarginHorizontal write SetMarginHorizontal;
    {Determines the horizontal margin in pixels between the button caption and the borders.}
    property MarginVertical: integer read FMarginVertical write SetMarginVertical;
    {Determines the vertical margin in pixels between the button caption and the borders.}
    property Color: TColor read FColor write SetColor;
    {Determines the background color of the button.}
    property HotTrack: boolean read FHotTrack write SetHotTrack;
    {Determines whether the button will be automatically highlighted when the mouse moves over it.}
    property HotColor: TColor read FHotColor write SetHotColor;
    {Determines the HotTrack button face color.}
    property DownColor: TColor read FDownColor write SetDownColor;
    {Determines the button face color in the pressed down state.}
    property HotFontColor: TColor read FHotFontColor write SetHotFontColor;
    {Determines the HotTrack color of the caption}
    property DownFontColor: TColor read FDownFontColor write SetDownFontColor;
    {Determines the color of the caption when the button is down.}
    property Hint;
    {Contains the text string that can appear when the user moves the mouse over the button.}
    property ShowHint;
    {Determines whether the control displays a Help Hint when the mouse pointer rests momentarily on the control. }
    property onclick; //event
    {Occurs when the user clicks the button.}
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('JanBasic', [TjanPanelButton]);
end;

{ TjanPanelButton }

procedure TjanPanelButton.Click;
begin
  CustomClick(false);
  inherited click;
end;


procedure TjanPanelButton.CustomClick(softclick:boolean);
var
  i:integer;
  pb:TjanPanelButton;

  procedure hidepanel(b:TjanPanelButton);
  var ii:integer;
      p:TPanel;
  begin
    if b.FPanel<>nil then
    begin
       p:=b.FPanel;
       if p.ControlCount =0 then
         p.Visible :=false
       else
       begin
         for ii:=0 to p.ControlCount-1 do
           if p.controls[ii] is TjanPanelButton then
             hidePanel(TjanPanelButton(p.controls[ii]));
         p.visible:=false;
       end;
    end;
  end;

begin
  // hide panels from buttons on the same parent
  for i:=0 to parent.ControlCount-1 do
    if parent.controls[i] is TjanPanelButton then
      if parent.controls[i]<>self then
      begin
        pb:=TjanPanelButton(parent.controls[i]);
        hidePanel(pb);
      end;
  if FPanel<>nil then
  begin
    FPanel.Visible :=not FPanel.Visible;
    if Fpanel.Visible then
      FPanel.BringToFront
    else
      hidePanel(self);
  end;

end;

constructor TjanPanelButton.Create(AOwner: TComponent);
begin
  inherited;
  width := 24;
  height := 24;
  FColor := $00FF8000;
  FDownColor := clNavy;
  FDownFontColor := clWhite;
  FPushDown := false;
  FMouseOver := false;
  FHotTrack := true;
  FHotFontColor := clYellow;
  FHotColor := $00FF8000;
  FFlat := true;
  FMarginHorizontal := 8;
  FMarginVertical := 4;
  Falignment := taCenter;
  FGlyph:=TBitmap.create;
  FGlyph.onchange:=Glyphchanged;
  FPanelOffset:=2;
  FFrameWidth := 1;
end;

procedure TjanPanelButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FPushDown := true;
  Paint;
end;

procedure TjanPanelButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FPushDown := false;
  invalidate;
end;

procedure TjanPanelButton.Paint;
var
  R, RCap: Trect;
  w, h: integer;


  procedure drawcaption(fontcolor: TColor);
  begin
    if FGlyph<>nil then
    begin
      FGlyph.transparent:=true;
      canvas.Draw (3,2,FGlyph);
      Rcap.left:=Rcap.left+FGlyph.Width+2;
    end;
    canvas.brush.style := bsclear;
    canvas.font.color := Fontcolor;
    case Falignment of
        taCenter:
          DrawText(canvas.handle, @Caption[1], -1, Rcap, DT_SINGLELINE or DT_VCENTER or DT_CENTER);
        taLeftJustify:
          DrawText(canvas.handle, @Caption[1], -1, Rcap, DT_SINGLELINE or DT_VCENTER or DT_LEFT);
        taRightJustify:
          DrawText(canvas.handle, @Caption[1], -1, Rcap, DT_SINGLELINE or DT_VCENTER or DT_RIGHT);
    end;
  end;

  procedure drawbackground(AColor: TColor);
  begin
      canvas.brush.color := AColor;
      canvas.FillRect(R);
  end;


begin
  canvas.font.Assign(Font);
  autofit;
  R := Rect(0, 0, width, height);
  Rcap := rect(FMarginHorizontal, 0, width - FMarginHorizontal, height);
  if FPushDown then
  begin
    RCap.left := Rcap.left + 1;
    RCap.top := RCap.top + 1;
    RCap.Right := RCap.right + 1;
    RCap.Bottom := Rcap.Bottom + 1;
  end;
  w := width - 1;
  h := height - 1;
  if (csDesigning in ComponentState) then
  begin
    drawbackground(FColor);
    Frame3D( Self.Canvas.Handle, R, FFrameWidth, bvRaised );
    drawcaption(font.color);
  end
  else if FPushDown then
  begin // depressed button
    drawbackground(FDownColor);
    Frame3D( Self.Canvas.Handle, R, FFrameWidth, bvLowered );
    drawcaption(FDownFontColor);
  end
  else if FMouseOver and HotTrack then
  begin // raised button with highlight caption
    drawbackground(FHotColor);
    Frame3D( Self.Canvas.Handle, R, FFrameWidth, bvRaised);
    drawcaption(FHotFontcolor);
  end
  else if FMouseOver or (not FFlat) then
  begin // raised button with normal caption
    drawbackground(Fcolor);
    Frame3D( Self.Canvas.Handle, R, FFrameWidth, bvRaised );
    drawcaption(font.color);
  end
  else
  begin // flat button with normal caption
    drawbackground(FColor);
    drawcaption(font.color);
  end;
end;

procedure TjanPanelButton.CMMouseLeave(var Message: TLMessage);
begin
  FMouseOver := false;
  Paint;
end;

procedure TjanPanelButton.SetFrameWidth(const AValue: Integer);
begin
 if (AValue <> FFrameWidth) and (AValue in [0..10]) then
  begin
    FFrameWidth := AValue;
    Self.Invalidate;
  end;
end;

procedure TjanPanelButton.SetHotTrack(const Value: boolean);
begin
  FHotTrack := Value;
end;

procedure TjanPanelButton.CMMouseEnter(var Message: TLMessage);
begin
  FMouseOver := true;
  Paint;
end;

procedure TjanPanelButton.SetColor(const Value: TColor);
begin
  if value <> FColor then
  begin
    FColor := Value;
    invalidate;
  end;
end;

procedure TjanPanelButton.SetHotColor(const Value: TColor);
begin
  if value <> FHotColor then
  begin
    FHotColor := Value;
  end;
end;

procedure TjanPanelButton.SetHotFontColor(const Value: Tcolor);
begin
  FHotFontColor := Value;
end;


procedure TjanPanelButton.SetMarginHorizontal(const Value: integer);
begin
  if value <> FMarginHorizontal then
  begin
    FMarginHorizontal := Value;
    invalidate;
  end;
end;

procedure TjanPanelButton.CMFontChanged(var Message: TLMessage);
begin
  invalidate;
end;

procedure TjanPanelButton.CMTextChanged(var Message: TLMessage);
begin
  invalidate;
end;

procedure TjanPanelButton.SetAlignment(const Value: Talignment);
begin
  if value <> Falignment then
  begin
    FAlignment := Value;
    invalidate;
  end;
end;

procedure TjanPanelButton.SetMarginVertical(const Value: integer);
begin
  if value <> FMarginVertical then
  begin
    FMarginVertical := Value;
    invalidate;
  end;
end;



destructor TjanPanelButton.Destroy;
begin
  FGlyph.free;
  inherited;
end;


procedure TjanPanelButton.SetDownColor(const Value: TColor);
begin
  if value <> FDownColor then
  begin
    FDownColor := Value;
  end;
end;

procedure TjanPanelButton.SetDownFontColor(const Value: TColor);
begin
  FDownFontColor := Value;
end;

procedure TjanPanelButton.SetFlat(const Value: boolean);
begin
  if value <> FFlat then
  begin
    FFlat := Value;
    invalidate;
  end;
end;



//-----------------------------------------------------------------------------


procedure TjanPanelButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled and Visible and
      (Parent <> nil) and Parent.Showing then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;


procedure TjanPanelButton.SetPanel(const Value: TPanel);
var pt:Tpoint;

  function root(AParent:TwinControl):TWincontrol;
  begin
    if Aparent.parent<>nil then
      result:=root(Aparent.parent)
    else
      result:=AParent;
  end;

begin
  FPanel := Value;
  if FPanel=nil then exit;
  FPanel.parent:=root(self.Parent);
  if FPanelPosition=ppPanelDown then
    pt:=clienttoscreen(point(0,height+FPanelOffset))
  else
    pt:=clienttoscreen(point(width+FPanelOffset,0));
  pt:=FPanel.parent.screentoclient(pt);
  FPanel.left:=pt.x;
  FPanel.top:=pt.y;
  if (csdesigning in componentstate) then
    FPanel.Visible :=true
  else
    Fpanel.Visible :=false;

end;

procedure TjanPanelButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (aComponent=FPanel) and (Operation=opremove) then
    Fpanel:=nil;
end;

procedure TjanPanelButton.AutoFit;
var w, h: integer;
  R: Trect;
begin
    w := canvas.TextWidth(Caption) + 2 * FMarginHorizontal;
    h := canvas.TextHeight(Caption) + 2 * FMarginVertical;
    if width < w then width := w;
    if height < h then height := h;
end;


procedure TjanPanelButton.SetPanelPosition(const Value: TPanelPosition);
var pt:Tpoint;
begin
  if value<>FPanelPosition then
  begin
    FPanelPosition := Value;
    if FPanel<>nil then
    begin
      if FPanelPosition=ppPanelDown then
        pt:=clienttoscreen(point(0,height))
      else
        pt:=clienttoscreen(point(width,0));
      pt:=FPanel.parent.screentoclient(pt);
      FPanel.left:=pt.x;
      FPanel.top:=pt.y;
    end;
  end;
end;

procedure TjanPanelButton.SetGlyph(const Value: TBitmap);
begin
  FGlyph.assign(Value);
  if FGlyph<>nil then
  begin
    FGlyph.transparent:=true;
  end;
end;

procedure TjanPanelButton.GlyphChanged(sender: TObject);
begin
  invalidate;
end;

procedure TjanPanelButton.SetPanelOffset(const Value: integer);
begin
  if value<>FPanelOffset then
  begin
    FPanelOffset := Value;
    invalidate;
  end;
end;

initialization
{$I janPanelButton.lrs}

end.

