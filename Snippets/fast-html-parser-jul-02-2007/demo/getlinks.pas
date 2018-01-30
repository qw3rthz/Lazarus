{ Gets HREF links from html page 
  License: NRCOL Public Domain

  Lars (L505)
  http://z505.com  }

program getlinks; {$MODE OBJFPC}{$H+}

uses
  FastHtmlParser, HtmlUtil, strwrap1;

type 
  TUrlList = array of string;

  TUrlExtracter = class
      urls: TUrlList;
    public
      procedure GetUrls(fname: string);
      procedure Clear;
    private
      procedure OnTag(UpcaseTag, actualtag: string);
      procedure AddUrl(link: string);
  end;


{ appends url into url list }
procedure TUrlExtracter.AddUrl(link: string);
var oldlen: longword;
begin
  oldlen:= length(urls);
  setlength(urls, oldlen + 1);
  urls[oldlen]:= link;
end;

{ trap tags found }
procedure TUrlExtracter.OnTag(UpcaseTag: string; actualtag: string);
var ExtractedURL: string;
begin
  // < A HREF found
  ExtractedUrl:= htmlutil.GetVal(actualtag, 'HREF');
  if ExtractedURL <> '' then
    AddUrl(ExtractedURL);
end;

{ empty the url list }
procedure TUrlExtracter.Clear;
begin
  setlength(urls, 0);
end;


procedure TUrlExtracter.GetUrls(fname: string);
var 
  Parser : THTMLParser; // fast html parser 
  filestring: string;
begin
  filestring:= StrLoadFile(fname);
  // error checking
  if filestring = '-1NF' then
  begin
    writeln('FILE NOT FOUND: ', fname);
    exit;
  end;
  Parser:= THTMLParser.Create(filestring);
  Parser.OnFoundTag := @OnTag;
  Parser.Exec;
  Parser.Free;
end;

{ show the URL's of an HTML file to STDOUT }
procedure ShowUrls(fname: string);
var
  UrlExt: TUrlExtracter;
  i: longword;
begin
  UrlExt:= TUrlExtracter.create;
  UrlExt.GetUrls(fname);
  if length(UrlExt.urls) > 0 then
    for i:= low(UrlExt.urls) to high(UrlExt.urls) do
      writeln(UrlExt.urls[i]);
  UrlExt.free; UrlExt:= nil;
end;


begin
  // simple test
  ShowUrls('test.htm');
  writeln;
  writeln('[hit enter to exit]');
  readln;
end.
