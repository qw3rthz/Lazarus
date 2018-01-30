{ Testing html utilities for errors and bugs.

  The NAME part of the NAME=VALUE pair should be case insensitive.
  The VALUE part of the NAME=VALUE pair should be case preserved 
  (for example, HREF url's on unix servers)

  Lars (L505)
  http://z505.com  }

program test1; {$MODE OBJFPC}{$H+}

uses
  HtmlUtil;

procedure Testing1;
begin

  // get value from name/value pair snippet
  writeln('Value found: ', GetValFromNameVal('width=4'));
  writeln('Value found: ', GetValFromNameVal('wiDtH=897'));
  writeln('Value found: ', GetValFromNameVal('wiDtH="866"'));
  writeln('Value found: ', GetValFromNameVal('WIDTH=''944'''));
  writeln('Value found: ', GetValFromNameVal('WIDTH=6'));
  writeln('Value found: ', GetValFromNameVal('cOlOr="rEd"'));

end;

procedure Testing2;
var tmptag: string;
begin
  tmptag:= '<table border=0 padding="5">test</a>';
  writeln('Value found: ' + GetVal(tmptag, 'PADDING'));

  tmptag:= '<table width="50" border=0>test</a>';
  writeln('Value found: ' + GetVal(tmptag, 'Border'));
end;


procedure Testing3;
var tmptag: string;
begin
  tmptag:= '<table width="123" >';
  // it should not return anything, but it does in James' unfixed version
  writeln('Check Bug: James'' old code:  id: ', GetVal_JAMES(tmptag, 'id') + ' <-- BAD!');
  writeln('Lars'' fix: id: ', GetVal(tmptag, 'id') + ' <-- SHOULD BE EMPTY!');
end;

procedure Testing4;
var tmptag: string;
begin
  // get value from full tag if NAME known, ignoring case of NAME input
  tmptag:= '<A hrEf="http://thatsite.com/teStiNg.HTm">website</a>';
  writeln('Value found: ', htmlutil.GetVal(tmptag, 'HREf'));

  // get full name/value pair, ignoring case of NAME fed as second parameter  
  tmptag:= '<A hrEf="http://somesite.com/soMefile.HtM" style="text-decoration:none;">test</a>';
  tmptag:= GetNameValPair(tmptag, 'href'); 
  writeln('Full name/value pair: ' + tmptag);
  writeln('Value alone: ' + GetValFromNameVal(tmptag));


  tmptag:= '<TaBlE border=0>';
  writeln('Tag name found: ', GetTagName(tmptag));
  writeln('Tag name in upper case: ', GetUpTagName(tmptag));
  writeln('Border width: ', htmlutil.GetVal(tmptag, 'bOrDeR'));

  // check if spaces at end affect it
  tmptag:= '<TaBlE border=0   >';
  writeln('length of border value: ', length(htmlutil.GetVal(tmptag, 'bOrDeR')), ' <-- SHOULD BE ONE');
end;

begin
  Testing1;
  Testing2;
  Testing3;
  Testing4;
  writeln;
  writeln('[hit enter to exit]');
  readln;
end.
