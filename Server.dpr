program Server;

{$APPTYPE CONSOLE}

uses Windows, SimpleTcp;

var
  Serv: TTcpServer;
  SList: TTcpSocketList;

function ConProc(CtrlType: DWORD): BOOL; stdcall;
begin
  Serv.Close;
  Result := True;
end;

procedure ClientProc(P: Pointer);
var
  S: AnsiString;
  I: Integer;
begin
  while TTcpSocket(P).ReadLn(S) do
  begin
    SList.Lock;
    for I := 0 to SList.Count - 1 do SList[I].WriteLn(S);
    SList.Unlock;
  end;
end;

procedure ServerProc;
var
  I: Integer;
  Sock: TTcpSocket;
begin
  SList := TTcpSocketList.Create;
  Serv := TTcpServer.Create(4879);
  SetConsoleCtrlHandler(@ConProc, True);
  repeat
    Sock := Serv.Accept;
    if Sock = nil then Continue;
    SList.Add(Sock);
    StartThread(ClientProc, Sock);
  until not Serv.Active;
  SList.Lock;
  for I := 0 to SList.Count - 1 do Slist[I].Close;
  SList.Unlock;
  Serv.Free;
  SList.Free;
end;

begin
  ServerProc;
end.
