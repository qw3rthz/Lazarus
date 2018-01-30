unit ptFunciones;

interface
   uses
   SysUtils, Types, Windows, StrUtils, IniFiles, DB, AdoDB, ProgTintDatos, classes,Printers, WinSpool;
   
   function FormatNumber (Number, StrLen: Integer): string;
   function GetAppVersion : string;
   function getTurno(hora: TDateTime): integer;
   function getFechaTurno (fecha,turno: string):integer;
   function getCelda(coordenadas: string): string;
   function fechaACadena(fecha: TDateTime; ordendia: boolean): string;
   function sFecha(fecha: TDateTime; siguiente: Boolean):TDateTime;
   Function esLaborable(Fecha: String):Boolean;
   function DriveAvailable(Drive: Char): Boolean;
   function IsMapped(Drive: Char): Boolean;
   function ComparaVersiones(programa:string; version: string): Boolean;
   function UserName : string;
   function ComputerName: string;
   function CambiaSql(objetoSql, cadenaSql: string): integer;
   function sFechaTurno (fecha: TDateTime; turno: integer; siguiente: boolean): TStringList;
   function dameIP(ip: integer): string;
   function EstadoImpresora(indice: Integer): TStringList;
   function VariableEntorno(Str: String): String;
   function CRC16(Str: string): word;
implementation


var
   iniParam: TIniFile;
   //ListaCubas: TComponentList;

{*
Formatea un entero a string con una longitud de caracteres determinada

@param Number Entero a procesar
@param StrLen Longitud de la cadena que se retorna
@return La cadena formateada
}
function FormatNumber (Number, StrLen: Integer): string;
begin
   Result := Format(Format('%%.%dd', [StrLen]), [Number])
end;

{*
Devuelve un string con la fecha sin separadores

@param fecha Fecha a procesar
@param ordendia Orden de las partes de la fecha (true: ddmmaaa, false: aaammdd)
@return Fecha formateada
}
function fechaACadena(fecha: TDateTime; ordendia: boolean): string;
begin
   if ordendia then
      result := copy(datetostr(fecha), 1, 2)+copy(datetostr(fecha), 4, 2)+rightstr(datetostr(fecha), 4)
   else
      result := rightstr(datetostr(fecha), 4)+copy(datetostr(fecha), 4, 2)+copy(datetostr(fecha), 1, 2);
end;

{*
A partir de la fecha y el turno genera un entero para su proceso ordenado posterior

@param fecha Fecha a procesar en formato 'aaaammdd'
@param turno Turno a procesar (M, T, N)
@return Entero en formato aaaammddT
}
function getFechaTurno (fecha,turno: string):integer;
begin
result := 0;
   case Ord(turno[1]) of
      77: result := strtoint(fecha + '1');//M
      84: result := strtoint(fecha + '2');//T
      78: result := strtoint(fecha + '3');//N
   end;
end;

{*
Devuelve la fecha siguiente/anterior laborable,
según la selección de festivos.

@param fecha La fecha inicial a procesar
@param siguiente Indica si se busca la fecha anterior o posterior
@return La fecha pedida
   }
function sFecha(fecha: TDateTime; siguiente: Boolean):TDateTime;
begin
   if siguiente then
   begin
      fecha := fecha + 1;
      while not esLaborable(FormatDateTime('dd/mm/yyyy', fecha)) do
      begin
         fecha := fecha + 1;
      end;
   end
   else
   begin
      fecha := fecha -1;
      while not esLaborable(FormatDateTime('dd/mm/yyyy', fecha)) do
      begin
         fecha := fecha - 1;
      end;
   end; //if siguiente
   result := fecha;
end;
{*
Devuelve el siguiente/anterior turno laborable, según la selección de festivos

@param fecha La fecha de partida
@param turno El turno de partida (1, 2, 3)
@param siguiente Indica si se busca el turno posterior (true) o anterior (false)
@return La fecha y turno siguiente laborable en un TStringList (dd/mm/yyyy, X)
}
function sFechaTurno (fecha: TDateTime; turno: integer; siguiente: boolean): TStringList;
begin
   result := TStringlist.Create();
   if not eslaborable(FormatDateTime('dd/mm/yyyy', fecha)) then
   begin  //la fecha es festiva
      if siguiente then
      begin //fecha siguiente
         fecha := fecha + 1;
         turno := 1;
         while not esLaborable(FormatDateTime('dd/mm/yyyy', fecha)) do
         begin
            fecha := fecha + 1;
         end;
         result.Add(FormatDateTime('dd/mm/yyyy', fecha));
         result.Add(copy('MTN', turno, 1));
      end
      else
      begin //fecha anterior
         fecha := fecha - 1;
         turno := 3;
         while not esLaborable(FormatDateTime('dd/mm/yyyy', fecha)) do
         begin
            fecha := fecha - 1;
         end;
         result.Add(FormatDateTime('dd/mm/yyyy', fecha));
         result.Add(copy('MTN', turno, 1));
      end;
   end
   else
   begin  //la fecha es laborable
      if siguiente then
      begin
         if turno < 3 then
         begin
            result.Add(FormatDateTime('dd/mm/yyyy', fecha));
            result.Add(copy('MTN', turno + 1, 1));
         end
         else
         begin
            fecha := fecha + 1;
            turno := 1;
            while not esLaborable(FormatDateTime('dd/mm/yyyy', fecha)) do
            begin
               fecha := fecha + 1;
            end;
            result.Add(FormatDateTime('dd/mm/yyyy', fecha));
            result.Add(copy('MTN', turno, 1));
         end;
      end
      else
      begin    //anterior
         if turno > 1 then
         begin
            result.Add(FormatDateTime('dd/mm/yyyy', fecha));
            result.Add(copy('MTN', turno - 1, 1));
            end
         else
         begin
            fecha := fecha - 1;
            turno := 3;
            while not esLaborable(FormatDateTime('dd/mm/yyyy', fecha)) do
            begin
               fecha := fecha - 1;
            end;
            result.Add(FormatDateTime('dd/mm/yyyy', fecha));
            result.Add(copy('MTN', turno, 1));
         end;
      end;
   end;
end;
{*
Indica si la fecha es laborable o fiesta

@param Fecha La fecha que queremos comprobar
@return True: Laborable. False: Fiesta
}
Function esLaborable(Fecha: String):Boolean;
begin
   iniParam := TIniFile.Create('\\fileserver\pub\Software\ProgTint\ProgTint.ini');
   with iniParam do
   begin
      if readString('FechasFiesta',Fecha,'n') = 'n' then
         result := True
      else
         result := False;
      Free;
   end;
end;


{*
Retorna el nº de turno (1:M, 2:T, 3:N) que corresponde a la hora pedida

@param hora Hora a procesar
@return Nº de turno
}
function getTurno(hora: TDateTime): integer;
var
   h, m, s, ms: word;
begin
   decodetime(hora, h, m, s, ms);
   case h of
      6..13: result := 1;
      14..21: result := 2;
   else
      result := 3;
   end;
end;

{*
Retorna la versión de la aplicación

@param
@return Versión formateada
}
function GetAppVersion : string;
var Size, Size2: DWord;
   Pt, Pt2: Pointer;
begin
   Size := GetFileVersionInfoSize(PChar (ParamStr (0)), Size2);
   if Size > 0 then
   begin
      GetMem (Pt, Size);
      try
         GetFileVersionInfo (PChar (ParamStr (0)), 0, Size, Pt);
         VerQueryValue (Pt, '\', Pt2, Size2);
         with TVSFixedFileInfo (Pt2^) do
         begin
            Result:= IntToStr (HiWord (dwFileVersionMS)) + '.' +
                     IntToStr (LoWord (dwFileVersionMS)) + '-' +
                     IntToStr (HiWord (dwFileVersionLS)) + '.' +
                     IntToStr (LoWord (dwFileVersionLS));
         end;
         finally
            FreeMem (Pt);
      end;
   end;
end;

{*
Devuelve el nombre de la celda dadas las coordenadas absolutas o relativas

@param coordenadas coordenadas absolutas (xxyy) o relativas (FFCCCT) de la celda
@return El nombre de la celda (FFCnnTxxyy)
}
function getCelda(coordenadas: string): string;
var
   x, y: integer;
   turno: integer;
begin
   if leftstr(coordenadas, 1) = 'F' then
   begin
      result := coordenadas;
      turno := Ord(coordenadas[6]);
      case turno of
         77: turno := 1;
         84: turno := 2;
         78: turno := 3;
      end;
      y := (strtoint(midstr(coordenadas, 2, 1))*3) + turno -1; // Coordenada Y del nombre
      x := strtoint(midstr(coordenadas, 4, 2));
      case x of
         1..12: x := strtoint(midstr(coordenadas, 4, 2)) + 1;
         13..14: x := strtoint(midstr(coordenadas, 4, 2)) -13;
      end;
      result := result + formatnumber(x, 2) + formatnumber(y, 2);
   end
   else
   begin
      x := strtoint(leftstr(coordenadas, 2));
      y := strtoint(rightstr(coordenadas, 2));
      case y of // Fecha
         0..2:
            result := 'F0C';
         3..5:
            result := 'F1C';
         6..8:
            result := 'F2C';
         9..11:
            result := 'F3C';
         12..14:
            result := 'F4C';
      end;
      case x of // Cuba
         2..13:
            result := result + formatNumber(x - 1, 2);
         0..1:
            result := result + formatNumber(x + 13, 2);
      end;
      case y of // Turno
         0,3,6,9,12:
            result := result + 'M';
         1,4,7,10,13:
            result := result + 'T';
         2,5,8,11,14:
            result := result + 'N';
      end;
      result := result + coordenadas;
   end;
end;

{*
Determina si una unidad lógica está disponible o no

@param Drive Letra del drive que consultamos
@return TRUE si está disponible
}
function DriveAvailable(Drive: Char): Boolean;
var
  DriveType: Cardinal;
begin
  DriveType := GetDriveType(PChar(String(Drive) + ':'));
  Result := DriveType in [DRIVE_NO_ROOT_DIR, DRIVE_REMOTE];
end;

{*
Devuelve la ruta de red asignada a una unidad lógica

@param Drive Letra de la unidad lógica
@return La ruta de red asignada a la letra
}
function GetConnection(Drive: Char): String;
var
  Buffer: PChar;
  BufferLen: Cardinal;
  Code: Cardinal;

begin
  Buffer := nil;
  BufferLen := 0;

  Code := WNetGetConnection(PChar(String(Drive) + ':'), Buffer, BufferLen);
  if Code = ERROR_MORE_DATA then
  begin
    GetMem(Buffer, BufferLen);
    Code := WNetGetConnection(PChar(String(Drive) + ':'), Buffer, BufferLen);
  end;

  case Code of
    NO_ERROR: Result := Buffer;
    //ERROR_NOT_CONNECTED: Result := '';
    //ERROR_CONNECTION_UNAVAIL: Result := 'Connection is unavailable';
  else
      Result := '';
    //raise Exception.CreateFmt('Error reading information for drive %s:', [Drive]);
  end;

  if BufferLen <> 0 then FreeMem(Buffer);
end;

{*
Devuelve si una unidad lógica está redirigida o no

@param Drive Letra de la unidad logica a consultar
@return TRUE si está redirigida
}
function IsMapped(Drive: Char): Boolean;
begin
  Result := GetConnection(Drive) <> '';
end;

{*
Compara la versión instalada con la de la carpeta de setup

@param programa Nombre del programa (ProgTint, ProgTintC, ProgTintV)
@param version Versión del programa instalada
@return True si son iguales
}
function ComparaVersiones(programa:string; version: string): Boolean;
var
   sr: TSearchRec;
   verSetup: string;
begin
   FindFirst('\\fileserver\pub\software\ProgTint\' + programa + 'Setup*', faAnyFile, sr);
   verSetup := leftstr(sr.Name, length(sr.Name)-4);
   verSetup := rightstr(verSetup, length(verSetup)- pos('2', verSetup)+ 1);
   if version =  verSetup then
      result := true
   else
      result := false;
   SysUtils.FindClose(sr);
end;

{*
Devuelve el nombre de usuario del sistema

@return Nombre de usuario
}
function UserName : string;
var
  tamanoBuffer: Cardinal;
  bufferUsuario: array[0..MAX_PATH] of Char;
begin
  tamanoBuffer := SizeOf(bufferUsuario);
  Windows.GetUserName(bufferUsuario, tamanoBuffer);
  Result := bufferUsuario;
end;

{*
Devuelve el nombre del ordenador

@return Nombre del ordenador
}
function ComputerName: string;
var
  Buffer: array[ 0..MAX_COMPUTERNAME_LENGTH ] of Char;
  MaxSize: Cardinal;
begin
  MaxSize := SizeOf( Buffer );
  if not GetComputerName( @Buffer, MaxSize ) then
    raise Exception.Create( 'No puedo determinar el nombre de la máquina' );
  Result := StrPas( @Buffer );
end;

{********************************************************************************}
{************************* Funciones adicionales   ******************************}
{********************************************************************************}
{*
Carga el query solicitado y lo activa

@param objetoSQL Database que se va a modificar (sqlLogic - sqlInfotint)
@param cadenaSQL La sentencia SQL que se va a procesar
@return Número de registros procesados
}
function CambiaSql(objetoSql, cadenaSql: string): integer;
begin
   with TADOQuery(Datos.FindComponent(objetoSql)) do
   begin
      result := 0;
      SQL.Clear;
      SQL.Add(cadenaSql);
      try
         if lowercase(leftstr(cadenaSql, 6)) = 'select' then
         begin
            Open;
            result := RecordCount;
         end
         else
         begin
            result := ExecSQL
         end;
      except
         on E: EDatabaseError do //showmessage(E.Message);
      end;
   end;
end;

{*
Devuelve la IP a partir de una IP codificada como entero

@param ip IP en formato entero a decodificar
@return IP en formato de cadena
}
function dameIP(ip: integer): string;
begin
   Result := Format('%d.%d.%d.%d', [(ip shr 24), (ip shr 16) and $FF,(ip shr 8) and $FF, ip and $FF]);
end;

{*
Devuelve el estado de la impresora

@param indice Nº de índice de la impresora (printer.printerindex). -1 para la impresora por defecto
@return 0- código. 1- Mensaje de estado.
}
function EstadoImpresora(indice: Integer): TStringList;
var
  printerhandle: thandle;
  count,level: DWORD;
  status: dword;
  buffer: Pchar;
  statusmsg: string;
  intresult: integer;
  b: BOOL;
begin
   result := TStringList.Create;
   try
      if indice = -1 then begin
         b := openprinter(PCHAR(printer.printers[printer.printerindex]),printerhandle,nil)
      end else begin
         b := openprinter(PCHAR(printer.printers[indice]),printerhandle,nil)
      end;
      level := 2;
      b := GetPrinter(printerhandle, level, nil, 0, @count);
      if count = 0 then exit;
      try
         GetMem(buffer, count);
         b := GetPrinter(printerhandle, level, pbyte(buffer), count, @count);
         if b then begin
            with PPrinterInfo2(buffer)^ do begin
               case status of
                  //PRINTER_STATUS_READY             : statusmsg := 'Preparada';
                  PRINTER_STATUS_PAUSED            : statusmsg := 'Pausa';
                  PRINTER_STATUS_ERROR             : statusmsg := 'Error';
                  PRINTER_STATUS_PENDING_DELETION  : statusmsg := 'Borrado pendiente';
                  PRINTER_STATUS_PAPER_JAM         : statusmsg := 'Atasco de papel';
                  PRINTER_STATUS_PAPER_OUT         : statusmsg := 'Sin papel';
                  PRINTER_STATUS_MANUAL_FEED       : statusmsg := 'Manual feed';
                  PRINTER_STATUS_PAPER_PROBLEM     : statusmsg := 'Problema papel';
                  PRINTER_STATUS_OFFLINE           : statusmsg := 'Fuera de linea';
                  PRINTER_STATUS_IO_ACTIVE         : statusmsg := 'I/O active';
                  PRINTER_STATUS_BUSY              : statusmsg := 'Ocupada';
                  PRINTER_STATUS_PRINTING          : statusmsg := 'Imprimiendo';
                  PRINTER_STATUS_OUTPUT_BIN_FULL   : statusmsg := 'Bandeja salida llena';
                  PRINTER_STATUS_NOT_AVAILABLE     : statusmsg := 'No disponible';
                  PRINTER_STATUS_WAITING           : statusmsg := 'Esperando';
                  PRINTER_STATUS_PROCESSING        : statusmsg := 'Procesando';
                  PRINTER_STATUS_INITIALIZING      : statusmsg := 'Iniciando';
                  PRINTER_STATUS_WARMING_UP        : statusmsg := 'Calentando';
                  PRINTER_STATUS_TONER_LOW         : statusmsg := 'Toner bajo';
                  PRINTER_STATUS_NO_TONER          : statusmsg := 'Sin toner';
                  PRINTER_STATUS_PAGE_PUNT         : statusmsg := 'Error impresión';
                  PRINTER_STATUS_USER_INTERVENTION : statusmsg := 'Intervención del usuario';
                  PRINTER_STATUS_OUT_OF_MEMORY     : statusmsg := 'Fuera de memoria';
                  PRINTER_STATUS_DOOR_OPEN         : statusmsg := 'Puerta abierta';
                  PRINTER_STATUS_SERVER_UNKNOWN    : statusmsg := 'Servidor desconocido';
                  PRINTER_STATUS_POWER_SAVE        : statusmsg := 'Ahorro energia';
               else                                  statusmsg := IntToStr(Status) + ' - Preparada';
               end; // case
            end; // with printerinfo2
         end // if result
         else begin
            intresult := GetLastError();
         end;
      finally
      FreeMem(buffer,count);
      end;
   finally
   b := closeprinter(printerhandle);
   result.Add(IntToStr(status));
   result.Add(statusmsg);
   end;
end;

{*
Retorna la ruta correspondiente a la variable de entorno pasada

@param Str Variable de entorno en formato '%VARIABLE_DE_ENTORNO%'.
@return Ruta absoluta de la variable. Si no existe retorna la cadena vacia.
}
function VariableEntorno(Str: String): String;
{Retorna la ruta correspondiente a la variable de entorno pasada
IN:   Variable de entorno en formato '%VARIABLE_DE_ENTORNO%'
OUT:  Ruta absoluta de la variable. Si no existe retorna la cadena vacia
}
var
  i: Integer;
  EnvBlock, P: PChar;
begin
  Result:= Str;
  EnvBlock:= GetEnvironmentStrings;
  if EnvBlock <> nil then
  try
    with TStringList.Create do
    try
      P:= EnvBlock;
      while P^ <> #0 do
      begin
        if Pos('=',String(P)) > 1 then
          Add(String(P));
        inc(P,StrLen(P)+1);
      end;
      for i:= 0 to Count - 1 do
        Result:= StringReplace(Result,'%'+ Names[i] +'%', values[names[i]],
          [rfIgnoreCase,rfReplaceAll]);
    finally
      Free;
    end;
  finally
    FreeEnvironmentStrings(EnvBlock);
  end;
  if Result = Str then
   Result := '';
end;

{*
Retorna el CRC16 de la cadena pasada

@param Str Cadena de la que se extraerá el CRC16.

@return CRC16 de la cadena.
}
function CRC16(Str: string): word;
var
   valuehex: word;
   i: integer;
   CRC: word;
Begin
   CRC := 0;
   for i := 1 to length(Str) do begin
      valuehex := ((ord(Str[i]) XOR CRC) AND $0F) * $1081;
      CRC := CRC SHR 4;
      CRC := CRC XOR valuehex;
      valuehex := (((ord(Str[i]) SHR 4) XOR LO(CRC)) AND $0F);
      CRC := CRC SHR 4;
      CRC := CRC XOR (valuehex * $1081);
   end;
   Result := (LO(CRC) SHL 8) OR HI(CRC);
end;


end.

