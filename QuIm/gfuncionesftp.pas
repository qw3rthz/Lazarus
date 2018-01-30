unit gFuncionesFTP;
{USO:
  -Crear un record TMySite en la unidad que llama a ésta
  -Configurar los parámetros
  -Llamar a ConfigurarFTP
  -Llamar a AbrirSesionFTP
  -Llamar a los procedimientos necesarios (SendFTPFile, CreateDirFTP,
        LeerDirectorioFTP, BorrarDirectorioFTP, ...)
  -Llamar a CerrarSesionFTP
Cada vez que se cambia un parámetro en el TMysite hay que llamar a ConfigurarFTP
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FTPSend,dialogs;

Type
  TMySite = record
    Host:       string;
    Puerto:     integer;
    User:       string;
    Pass:       string;
    LocalDir:   string; //Path en la máquina local
    LocalFile:  string; //Fichero local a enviar
    RemoteDir:  string; //Path en la máquina remota
    RemoteFile: string; //Fichero remoto a escribir
  end;

var
  Site: TMySite;
  FTPConn: TFTPSend;

procedure ConfigurarFTP(SiteConf: TMySite);
function AbrirSesionFTP:boolean;
function CerrarSesionFTP:boolean;
function SendFTPFile(SiteConf: TMySite):boolean; //No usa TMysite general
function SendFTPFile:boolean;
function RecFTPFile:boolean;
function CreateDirFTP(newDir: string):boolean;
function LeerDirectorioFTP:TStringList;
function BorrarDirectorioFTP: boolean;


implementation

procedure ConfigurarFTP(SiteConf: TMySite);
{Configura la conexión
IN: Site. Parámetros necesarios para la conexión.}
begin
  Site := SiteConf;
end;

function AbrirSesionFTP:boolean;
{Inicia una sesión FTP. No necesita parámetros si se ha llamado
antes a ConfiguraFTP
IN:
OUT:  True si se ha logueado correctamente en el servidor}
begin
  result := false;
  if not Assigned(FTPConn) then
    FTPConn := TFTPSend.Create;
  if (Site.Host <> '') AND (Site.Puerto > 0) then begin
    with FTPConn do begin
      TargetPort := IntToStr(Site.Puerto);
      TargetHost := Site.Host;
      UserName := Site.User;
      Password := Site.Pass;
      if Login then begin
        result := true;
      end;
    end;
  end;
end;

function CerrarSesionFTP:boolean;
{Finaliza una sesión FTP
IN:
OUT:  True si se ha finalizado con un logout. False si se ha forzado la finalización}
begin
  result := false;
  if Assigned(FTPConn) then begin
    if FTPConn.Logout then begin
      result := true;
    end;
    FreeAndNil(FTPConn);
  end;
end;

function SendFTPFile: boolean;
{Envia un fichero al servidor FTP. Se ha de ejecutar antes
ConfiguraFTP y AbreSesionFTP
IN:
OUT:  True si se ha enviado el fichero correctamente}
begin
  result := false;
  if not Assigned(FTPConn) then exit;
  if not FTPConn.NoOp then exit;
  with FTPConn do begin
    DirectFileName := Site.LocalDir + Site.LocalFile;
    DirectFile := true;
    if ChangeWorkingDir(Site.RemoteDir) then
      result := StoreFile(Site.RemoteFile , false);
  end;
end;

function RecFTPFile:boolean;
{Recibe un fichero del servidor FTP. e ha de ejecutar antes
ConfiguraFTP y AbreSesionFTP
IN:
OUT:  True si se ha recibido el fichero correctamente}
begin
  result := false;
  if not Assigned(FTPConn) then exit;
  if not FTPConn.NoOp then exit;
  with FTPConn do begin
    DirectFileName := Site.LocalDir + Site.LocalFile;
    DirectFile := true;
    if ChangeWorkingDir(Site.RemoteDir) then
      result := RetrieveFile(Site.RemoteFile , false);
  end;
end;

function CreateDirFTP(newDir: string):boolean;
{Crea un directorio en el servidor remoto. Se ha de ejecutar antes
ConfiguraFTP y AbreSesionFTP
IN:   newDir. Nombre del nuevo directorio
OUT:  True si se ha creado correctamente}
begin
  result := false;
  if not Assigned(FTPConn) then exit;
  if not FTPConn.NoOp then exit;
  if FTPConn.ChangeWorkingDir(Site.RemoteDir) then
      result := FTPConn.CreateDir(newDir);
end;

function SendFTPFile(SiteConf: TMySite):boolean;
{Envia un fichero al servidor FTP indicado en SiteConf
IN:   SiteConf. Parámetros configuración y transferencia
OUT:  True si se ha enviado el fichero correctamente}
var
  FTPClient: TFTPSend;
begin
  result := false;
  FTPClient := TFTPSend.Create;
  with FTPClient do begin
    TargetPort := IntToStr(SiteConf.Puerto);
    TargetHost := SiteConf.Host;
    UserName := SiteConf.User;
    Password := SiteConf.Pass;

    if not Login then exit;

    DirectFileName := SiteConf.LocalDir + SiteConf.LocalFile;
    DirectFile := true;
    ChangeWorkingDir(SiteConf.RemoteDir);
    result := StoreFile(SiteConf.RemoteFile , false);
    LogOut;
  end;
  FreeAndNil(FTPClient);
end;

function LeerDirectorioFTP:TStringList;
{Lee los ficheros del directorio (no las carpetas)}
var
  i: integer;
begin
    result := TStringList.Create;
    result.Text := '';
    if not Assigned(FTPConn) then exit;
    if not FTPConn.NoOp then exit;
    if FTPConn.List(Site.RemoteDir, false) then begin
      for i := 0 to FTPConn.FtpList.Count - 1 do begin
        if FTPConn.FtpList.Items[i].Directory then begin
          result.Add(FTPConn.FtpList.Items[i].FileName);
        end;
      end;
    end;
end;

function BorrarDirectorioFTP: boolean;
{Borra un directorio. Se ha de ejecutar antes
ConfiguraFTP y AbreSesionFTP}
begin
  result := false;
  if not Assigned(FTPConn) then exit;
  if not FTPConn.NoOp then exit;
  if FTPConn.ChangeWorkingDir(Site.RemoteDir) then
      result := FTPConn.DeleteDir(Site.RemoteDir);
end;

end.

