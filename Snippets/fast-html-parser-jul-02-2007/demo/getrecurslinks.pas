{ Gets HREF links from html page recursively (travels to links found) 
  BE CAREFUL because it will eat up bandwidth and go through entire internet 
  if not careful

  If doing local files, such as file:/// then synapse may not work as I
  don't know if it supports local file:/// syntax for URL's.
  Instead, just use StrLoadFile c:\sub\yourfile.htm or similar

  License: NRCOL Public Domain

  Lars (L505)
  http://z505.com  }

program getrecurslinks; {$MODE OBJFPC}{$H+}

uses
  FastHtmlParser, HtmlUtil, strwrap1, synwrap1, sysutils;

type 
  TUrlList = array of string;

  TUrlExtracter = class
      urls: TUrlList;
    public
      function Getlinks(url: string): boolean;
      procedure Clear;
      function IsDuplicate(link: string): boolean;
    private
      procedure OnTag(UpcaseTag, actualtag: string);
      procedure AddUrl(link: string);
  end;

const
  BASE_URL = 'http://z505.com/recursive-test/'; 
  { Only go in designated areas for recursion, such as this one.
  
   Anyone who recurses through the entire Z505.com/ root part of the website
   will have their fingers cut off. I made this designated area for testing 
   and the loop gets killed on Link #7.  Do not run a robot through a major
   domain like google.com, microsoft.com,  the robot will go through the 
   entire internet if you do, and I will cut off your fingers if your bot hits
   my site's main area such as z505.com root. I also put a limit of 50, see 
   further down. }


{ appends url into url list }
procedure TUrlExtracter.AddUrl(link: string);
var oldlen: longword;
begin
  // don't want duplicate URL's in a recursive extraction
  if IsDuplicate(link) then exit;
  // else add
  oldlen:= length(urls);
  setlength(urls, oldlen + 1);
  urls[oldlen]:= link;
end;

{ check for duplicates (looks through entire array. One should use a real SQL 
  database instead of an Array if serious about building a powerful robot }
function TUrlExtracter.IsDuplicate(link: string): boolean;
var i: longword;
begin
  result:= false;
  if length(urls) < 1 then exit;
  for i:= low(urls) to high(urls) do
    if urls[i] = link then result:= true;
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

{ extract links from a page, return false if error }
function TUrlExtracter.Getlinks(url: string): boolean;
var 
  Parser : THTMLParser; // fast html parser 
  filestring: string;
  httpinfo: TSynwInfo;
  httpresponse: string;
begin
  // get html 
  httpinfo:= synwrap1.Geturl(url);
  if httpinfo.err > 0 then
  begin
    writeln('Problem getting URL from web, httpinfo err: ', httpinfo.err);
    result:= false;
    exit;
  end;

  // prefer 200 or 304 http responses
  if (httpinfo.ErrResponse <> 200 ) and (httpinfo.ErrResponse <> 304) then
  begin
    writeln('Problem getting a URL: http error: ', httpinfo.ErrResponse);
    result:= false;
    exit;
  end;

  // parse gotten HTML 
  Parser:= THTMLParser.Create(httpinfo.UrlHtml);
  Parser.OnFoundTag := @OnTag;
  Parser.Exec;
  Parser.Free;
  result:= true;
end;


{ show the URL's in anchors recursively inside a URL and all its links etc. }
procedure ShowRecursiveUrls(url: string);
var
  UrlExt: TUrlExtracter;
  curidx: longword;

  procedure Loop;
  var 
    iSub: longword;
    site: string;
  begin
    // PUT LIMIT ON AMOUNT OF LINKS ROBOT EXTRACTS
    if curidx > 50 then exit; 

    site:= BASE_URL + UrlExt.urls[curidx];
    writeln('Getting ', site);
    
    // BE NICE, PAUSE ABOUT A SECOND BETWEEN REQUESTS
    SLEEP(800); 

    if not UrlExt.Getlinks(site) then exit;

    inc(curidx);
    if curidx < length(UrlExt.urls) then 
      Loop; //recursive!    
  end;

var
  i: longword;
begin
  UrlExt:= TUrlExtracter.create;
  UrlExt.Clear;
  // get links from first page
  UrlExt.Getlinks(url);
  curidx:= 0;
  // get links recursively now that we got first start page 
  Loop;

  writeln;
  writeln('Urls recursively extracted: ');
  for i:= low(UrlExt.urls) to high(UrlExt.urls) do
    writeln('  ', UrlExt.urls[i]);

  // cleanup
  UrlExt.free; UrlExt:= nil;
end;


begin
  // simple test
  ShowRecursiveUrls(BASE_URL + 'testrecurs1.htm');

  { NOTE: you would have to calculate the BASE_URL yourself on each website 
          based on certain conditions if building a real robot 
      
   Example: check the url for http:// and file://  If it has no http:// or 
            file:// then it is a relative path and not absolute. I did not 
            implement above feature } 

  writeln;
  writeln('[hit enter to exit]');
  readln;
end.
