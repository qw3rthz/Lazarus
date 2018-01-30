program project;

uses
  CgiShared,
  SysUtils;

  function WriteInterface: Boolean;
  begin
    Result := WriteRequestString('First Name', itText, pnName);
    Result := WriteRequestString('Surname', itText, pnSurname) and Result;
    Result := WriteRequestString('Address', itText, pnAddress) and Result;
    Result := WriteRequestString('City/Town', itText, pnCity) and Result;
    Result := WriteRequestInteger('Post Code', itText, pnPostCode) and Result;
    Result := WriteRequestString('Country', itText, pnCountry) and Result;
    Result := WriteRequestInteger('Age', itText, pnAge) and Result;
  end;

  procedure WriteHtml;
  var
    LIsValid: Boolean;
  begin
    WriteLn('<HTML>');
    WriteLn('<HEAD><TITLE>Custom CGI Application 1 - Demo</TITLE></HEAD>');
    WriteLn('<BODY style=background-color:#eeeeee><FORM name=f method=get>');
    WritePageHeader;
    WriteTableBegin;
    try
      LIsValid := WriteInterface;
    finally
      WriteTableEnd;
    end;
    WriteLn('<BR>');
    Write('<CENTER>');
    if WasSubmitted and LIsValid then
    begin
      Write(HtmlInput(itSubmit, pnSubmit, SDone));
    end else begin
      if WasSubmitted then
      begin
        WriteError;
      end;
      Write(HtmlInput(itSubmit, pnSubmit, SValidate));
    end;
    WriteLn('</CENTER>');
    WriteLn('</FORM></BODY></HTML>');
  end;

begin
  WriteLn('Content-Type: text/html');
  WriteLn;
  PrepareQuery;
  WriteHtml;
end.
