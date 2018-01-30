unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons,HTMLPars, Unit2;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure LabelClick(Sender:TObject);
  public
    HTMLParser:THTMLParser;
  end;

var
  Form1: TForm1;

implementation


{$R *.DFM}

const DennisCape='Denniscape';


procedure TForm1.Button1Click(Sender: TObject);
var
    s:string;
    j,i:integer;
    obj:TObject;
    HTMLTag:THTMLTag;
    HTMLParam:THTMLParam;


begin
  Memo1.Clear;

  HTMLParser:=THTMLParser.Create;
  HTMLParser.Lines.loadfromfile('D:\test.html');
  HTMLParser.Execute;

  for i:= 1 to HTMLParser.parsed.count do
  begin
   obj:=HTMLParser.parsed[i-1];

   if obj.classtype=THTMLTag then
    begin
     HTMLTag:=THTMLTag(obj);
     s:='TAG: <'+HTMLTag.Name;
     if HTMLTag.Params.count=0 then memo1.Lines.add(s+'>')
     else
     begin
      memo1.Lines.add(s);
      for j:= 1 to HTMLTag.Params.count do
       begin
        HTMLParam:=HTMLTag.Params[j-1];
        s:='  P:   '+HTMLParam.key;
        if HTMLParam.value<>'' then s:=s+'=>'+HTMLParam.value;
        if j=HTMLTag.Params.count then s:=s+'>';
        memo1.Lines.add(s);
       end;
     end;
    end;

   if obj.classtype=THTMLText then memo1.Lines.add('TXT: '+THTMLText(obj).Line);
  end;

  Button2.Enabled:=true;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
    s:string;
    j,i:integer;
    obj:TObject;
    HTMLTag:THTMLTag;
    HTMLParam:THTMLParam;
    CurrentFont:TFont;
    label1:Tlabel;
    x,y:integer;
    OldFont:TFont;
    ignore:boolean;
    isTitle:boolean;
    isLink:boolean;
    Link:string;
    lastHeight:integer;
    Shape:TShape;
    PreFont:boolean;

begin
 form2.updating:=true;

 form2.Caption:=denniscape;
 isTitle:=false;
 isLink:=false;
 PreFont:=false;

 // Clear Page
 for i:= form2.ComponentCount downto 1 do form2.Components[i-1].Free;

 form2.show;

 // set Default font
 CurrentFont:=TFont.Create;
 CurrentFont.Name:='Times New Roman';
 CurrentFont.Size:=12;
 OldFont:=TFont.Create;
 LastHeight:=abs(CurrentFont.Height);

  x:=2;
  y:=2;
  ignore:=true;

  for i:= 1 to HTMLParser.parsed.count do
  begin
   obj:=HTMLParser.parsed[i-1];

   if obj.classtype=THTMLTag then
    begin
     HTMLTag:=THTMLTag(obj);

     if HTMLTag.Name='H1' then begin OldFont.assign(CurrentFont);CurrentFont.Size:=24;CurrentFont.Style:=[fsBold];LastHeight:=abs(CurrentFont.Height);end;
     if HTMLTag.Name='/H1' then begin y:=y+LastHeight;x:=2;CurrentFont.assign(OldFont);y:=y+abs(CurrentFont.Height);lastHeight:=abs(CurrentFont.Height);end;

     if HTMLTag.Name='BR' then begin y:=y+LastHeight;lastHeight:=abs(CurrentFont.Height);x:=2;end;

     if HTMLTag.Name='B' then CurrentFont.style:=CurrentFont.style+[fsBold];
     if HTMLTag.Name='/B' then CurrentFont.style:=CurrentFont.style-[fsBold];

     if HTMLTag.Name='TITLE' then isTitle:=true;
     if HTMLTag.Name='/TITLE' then isTitle:=false;

     if HTMLTag.Name='HR' then
      begin
       y:=y+LastHeight+Lastheight div 2;x:=2;LastHeight:=abs(CurrentFont.Height);
       Shape:=TShape.Create(form2);
       Shape.Top:=y;
       Shape.Parent:=Form2;
       shape.Left:=x;
       Shape.Height:=3;
       Shape.Width:=form2.clientwidth-20;
       Shape.Pen.Color:=clGray;
       y:=y+SHape.Height+Lastheight div 2;
      end;

{     disabled because only .BMP files are displayed ;)

      if HTMLTag.Name='IMG' then
      begin
       for j:= 1 to HTMLTag.Params.count do
       begin
        HTMLParam:=HTMLTag.Params[j-1];
         if HTMLParam.Key='SRC' then begin
                                       Image1:=TImage.Create(form2);
                                       Image1.Parent:=Form2;
                                       Image1.Top:=y;
                                       Image1.Left:=x;
                                       Image1.Picture.Loadfromfile('D:\'+HTMLParam.Value);
                                       Image1.Autosize:=true;
                                       x:=x+Image1.Width;
                                      end;
       end;
      end;
}

     if HTMLTag.Name='BODY' then
      begin
       ignore:=false;
       for j:= 1 to HTMLTag.Params.count do
       begin
        HTMLParam:=HTMLTag.Params[j-1];
         if HTMLParam.Key='BGCOLOR' then begin
                                          s:=HTMLParam.Value;s[1]:='$';
                                          Form2.Color:=StrToInt(s);
                                         end;
       end;
      end;

     if (HTMLTag.Name='PRE') then
      begin
       x:=2;
       y:=y+LastHeight;
       PreFont:=true;
      end;

    if (HTMLTag.Name='/PRE') then
     begin
       x:=2;
       y:=y+LastHeight;
       LastHeight:=abs(CurrentFont.Height);
       PreFont:=false;
     end;

    if (HTMLTag.Name='A') then
     begin
      isLink:=true;
      Link:='';
      for j:= 1 to HTMLTag.Params.count do
       begin
        HTMLParam:=HTMLTag.Params[j-1];
        if HTMLParam.Key='HREF' then Link:=HTMLParam.Value;
       end;
      end;

    if (HTMLTag.Name='/A') then isLink:=false;


     if HTMLTag.Name='FONT' then
     begin
       OldFont.assign(CurrentFont);
       for j:= 1 to HTMLTag.Params.count do
       begin
        HTMLParam:=HTMLTag.Params[j-1];
        if HTMLParam.Key='FACE' then CurrentFont.Name:=HTMLParam.Value;
       end;
     end;

     if HTMLTag.Name='/FONT' then
      begin
       CurrentFont.assign(OldFont);
      end;

    end;


   if obj.classtype=THTMLText then
   if not ignore then
    begin
     Label1:=TLabel.Create(Form2);
     Label1.Parent:=Form2;
     Label1.WordWrap:=true;
     Label1.Top:=y;
     Label1.Left:=x;
     Label1.Autosize:=false;
     Label1.Width:=Form2.ClientWidth-18;
     Label1.Caption:=THTMLText(obj).Line;
     Label1.Font.assign(CurrentFont);
     //label1.Color:=clgreen;

     if PreFont then
      begin
       Label1.FOnt.Name:='Courier';
       Label1.Font.Size:=10;
       Label1.Caption:=THTMLText(obj).Raw;
       Label1.WordWrap:=false;
      end;

     if isLink then
      begin
       Label1.Font.Color:=clBlue;
       Label1.Font.Style:=[fsUnderline];
       Label1.OnClick:=LabelClick;
       Label1.Cursor:=crHandPoint;
       Label1.Hint:=Link;
      end;

     s:=Label1.Caption;
     while pos('&',s)>0 do s[pos('&',s)]:=#1;
     while pos(#1,s)>0 do begin
                           insert('&&',s,pos(#1,s));
                           delete(s,pos(#1,s),1);
                          end;
     Label1.Caption:=s;

     Label1.Autosize:=true;
     x:=x+Label1.Width;
     LastHeight:=Label1.Height;
    end
   else
   if isTitle then Form2.Caption:=THTMLText(obj).Line+' - '+Denniscape;
  end;


 form2.updating:=false;

end;

procedure TForm1.LabelClick(Sender:TObject);
begin
 ShowMessage('Linking to "'+TLabel(Sender).Hint+'"');
end;

end.
