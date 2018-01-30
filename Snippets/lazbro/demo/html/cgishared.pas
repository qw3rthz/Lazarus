unit CgiShared;

interface

uses
  Classes, HtmlElements, HttpDefs, StrUtils, SysUtils;

const

  itText = 'text';
  itHidden = 'hidden';
  itSubmit = 'submit';
  itTextArea = 'textarea';

const
  pnName = 'name';
  pnAge = 'age';
  pnAddress = 'address';
  pnSurname = 'surname';
  pnCity = 'city';
  pnPostCode = 'postcode';
  pnSubmit = 'submit';
  pnCountry = 'country';
  pnDetails = 'details';

const
  SDone = 'Done';
  SValidate = 'Validate';

function WasSubmitted: Boolean;
procedure WritePageHeader;
procedure PrepareQuery;
procedure WriteTableBegin;
procedure WriteTableEnd;
function IsValidateString(const AName: String): Boolean;
function IsValidateInteger(const AName: String): Boolean;
function HtmlInput(const AType, AName, AValue: String): String;
procedure WriteTableRowRequest(const ACaption, AType, AName, AValue: String; AIsValid: Boolean);
function WriteRequestString(const ACaption, AType, AName: String): Boolean;
function WriteRequestInteger(const ACaption, AType, AName: String): Boolean;
procedure WriteError;

var
  GInvalidFieldName: String;
  GQuery: TStringList;

implementation

function WasSubmitted: Boolean;
begin
  Result := GQuery.Values[pnSubmit] <> EmptyStr;
end;

procedure WritePageHeader;
begin
  WriteLn('<CENTER><IMG src="img/logo.png"><BR><A href="index.html">Go Back to Index</A></CENTER>');
end;

procedure WriteError;
begin
  if GInvalidFieldName <> EmptyStr then
  begin
    Write('<P style=text-align:center;color:#ff0000><B>The field "' + GInvalidFieldName + '" is not valid. Please edit.</B></P>');
  end;
end;

function HtmlInput(const AType, AName, AValue: String): String;
begin
  if SameText(AType, itTextArea) then
  begin
    Result := Format('<TEXTAREA width=100%% height=200 style="background-color:#ffffff" type="%s" name="%s">%s</TEXTAREA>',
      [AType, AName, EscapeHTML(AValue)]);
  end else begin
    if SameText(AType, itSubmit) then
    begin
      Result := Format('<INPUT type="%s" name="%s" value="%s">', [AType, AName, EscapeHTML(AValue)]);
    end else begin
      Result := Format('<INPUT width=100%% style="background-color:#ffffff" type="%s" name="%s" value="%s">',
        [AType, AName, EscapeHTML(AValue)]);
    end;
  end;
end;

procedure WriteTableRowRequest(const ACaption, AType, AName, AValue: String; AIsValid: Boolean);
begin
  AIsValid := (GQuery.Values[pnSubmit] = EmptyStr) or AIsValid;
  if (not AIsValid) and (GInvalidFieldName = EmptyStr) then
  begin
    GInvalidFieldName := AName;
  end;
  WriteLn('<TR>');
  WriteLn('<TH width=30% style="color:' + IfThen(AIsValid, '#000000', '#ff0000') + '">' + ACaption + IfThen(AIsValid, '', '?') +
    '</TH><TD width=70%>' + HtmlInput(AType, AName, AValue) + '</TD>');
  WriteLn('</TR>');
end;

function WriteRequestString(const ACaption, AType, AName: String): Boolean;
begin
  GQuery.Values[AName] := Trim(GQuery.Values[AName]);
  Result := IsValidateString(AName);
  WriteTableRowRequest(ACaption, AType, AName, GQuery.Values[AName], Result);
end;

function WriteRequestInteger(const ACaption, AType, AName: String): Boolean;
begin
  GQuery.Values[AName] := Trim(GQuery.Values[AName]);
  Result := IsValidateInteger(AName);
  WriteTableRowRequest(ACaption, AType, AName, GQuery.Values[AName], Result);
end;

procedure WriteTableBegin;
begin
  Write('<CENTER><TABLE>');
end;

procedure WriteTableEnd;
begin
  Write('</TABLE></CENTER>');
end;

function IsValidateString(const AName: String): Boolean;
begin
  Result := GQuery.Values[AName] <> EmptyStr;
end;

function IsValidateInteger(const AName: String): Boolean;
begin
  try
    StrToInt(GQuery.Values[AName]);
    Result := True;
  except
    Result := False;
  end;
end;

procedure PrepareQuery;
var
  LIdx: Integer;
begin
  GQuery := TStringList.Create;
  GQuery.Delimiter := '&';
  GQuery.StrictDelimiter := True;
  GQuery.DelimitedText := GetEnvironmentVariable('QUERY_STRING');
  for LIdx := 0 to GQuery.Count - 1 do
  begin
    GQuery[LIdx] := HTTPDecode(GQuery.Names[LIdx]) + '=' + HTTPDecode(GQuery.ValueFromIndex[LIdx]);
  end;
end;

end.
