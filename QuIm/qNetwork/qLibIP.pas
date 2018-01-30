unit qLibIP;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, sockets, baseunix, fphttpclient, SynaIP;

function GetLocalIPs(loopIP: Boolean = False):string;
function GetPublicIP(url: string = 'http://ifconfig.me/ip'): string;
function GetCIDR(mascara: string):integer;
const
  IPPROTO_IP     = 0;
  IF_NAMESIZE    = 16;
  SIOCGIFCONF    = $8912;

type
 {$packrecords c}
  tifr_ifrn = record
    case integer of
      0 : (ifrn_name: array [0..IF_NAMESIZE-1] of char);
  end;
  PIFrec = ^TIFrec;
  TIFrec = record
    ifr_ifrn : tifr_ifrn;
    case integer of
      0 : (ifru_addr      : TSockAddr);
  end;
  TIFConf = record
    ifc_len : longint;
    case integer of
      0 : (ifcu_buf : pointer);
      1 : (ifcu_req : ^tifrec);
  end;

implementation

function GetLocalIPs(loopIP: Boolean = False):string;
var
  i,n,Sd : Integer;
  buf : Array[0..1023] of byte;
  ifc : TIfConf;
  ifp : PIFRec;
  names:string;
begin
    sd:=fpSocket(AF_INET,SOCK_DGRAM,IPPROTO_IP);
    result:='';
    if (sd<0) then
      exit;
    Try
      ifc.ifc_len:=Sizeof(Buf);
      ifc.ifcu_buf:=@buf;
      if fpioctl(sd, SIOCGIFCONF, @ifc)<0 then
        Exit;
      n:= ifc.ifc_len;
      i:=0;
      names:='';
      While (i<n) do
        begin
        ifp:=PIFRec(PByte(ifc.ifcu_buf)+i);
        names:=names+ ifp^.ifr_ifrn.ifrn_name+ ' ';
        if i>0 then
          result:=result+',';
        result:=result+NetAddrToStr(ifp^.ifru_addr.sin_addr);
        i:=i+sizeof(TIFrec);
        end;
    Finally
      fileClose(sd);
    end;
    if not loopIP then
      result := StringReplace(result, '127.0.0.1,', '', [rfReplaceAll]);
end;

function GetPublicIP(url: string = 'http://ifconfig.me/ip'): string;
{Otros servicios:
  http://icanhazip.com/
  http://ipchicken.com/ (requiere parseado)
  http://www.designscripting.com/webtools/find-my-ip/what-is-my-ip-address.php (requiere parseado)
}
begin
  with TFPHTTPClient.Create(nil) do
    try
      result := Get(url);
      result := LeftStr(result, Length(result) - 1)
    finally
      Free;
    end;
end;

function GetCIDR(mascara: string):integer;
var
  n: cardinal;
begin
  n := StrToIP(mascara);
  result := 0;
  while n > 0 do begin
    result := result + 1;
    n := n AND (n - 1);
  end;
end;

end.

