{ =========================================================================== }
{                                                                             }
{                    Delphi 10.2 SimpleTcp Unit v. 1.7                        }
{        Copyright (c) 2018 Mateusz Piechnat [http://piechnat.pl]             }
{                                                                             }
{ =========================================================================== }

unit SimpleTcp;

interface

{ =========================================================================== }

uses Windows, WinSock, WinSock2;

const
  NUL = #0;
  CR = #13;
  LF = #10;
  CRLF = CR + LF;

type
  TSocks4Header = packed record
    VN: Byte;
    CD: Byte;
    DSTPORT: Word;
    DSTIP: Cardinal;
  end;

  TCriticalSection = class;

  TBaseSocket = class(TObject)
  private
    FActive: Boolean;
    FSocket: TSocket;
    FLastError: Integer;
    FTag: Integer;
    function GetLocalIP: AnsiString;
    function GetLocalPort: Word;
    function CheckError(Value: Integer): Integer; virtual;
  public
    property Active: Boolean read FActive;
    property Socket: TSocket read FSocket;
    property LocalIP: AnsiString read GetLocalIP;
    property LocalPort: Word read GetLocalPort;
    property LastError: Integer read FLastError write FLastError;
    property Tag: Integer read FTag write FTag;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TTcpSocket = class(TBaseSocket)
  private
    FTimeOut: Integer;
    FEndOfLine: AnsiString;
    FWriteCS: TCriticalSection;
    FReadCS: TCriticalSection;
    function GetRemoteIP: AnsiString;
    function GetRemotePort: Word;
    function CheckError(Value: Integer): Integer; override;
    procedure SetTimeOut(TmOut: Integer);
    function Recv(var Buf; Len, Flags: Integer): Integer;
    function Send(var Buf; Len, Flags: Integer): Integer;
  public
    property EndOfLine: AnsiString read FEndOfLine write FEndOfLine;
    property RemoteIP: AnsiString read GetRemoteIP;
    property RemotePort: Word read GetRemotePort;
    property TimeOut: Integer read FTimeOut write SetTimeOut;
    function Write(var Buf; Count: Integer): Integer; overload;
    function Write(const Str: AnsiString): Boolean; overload;
    function WriteLn(const Str: AnsiString): Boolean;
    function Read(var Buf; Count: Integer): Integer;
    function ReadStr(var Str: AnsiString; MaxLen: Integer): Boolean;
    function ReadLn(var Str: AnsiString): Boolean;
    function ReadAll(var Buf; Count: Integer): Integer;
    function Close: Boolean;
    constructor Create; override;
    destructor Destroy; override;
  end;
  PTcpSocket = ^TTcpSocket;

  TTcpClient = class(TTcpSocket)
  public
    function Open(const Host: AnsiString; Port: Word): Boolean;
    constructor Create; overload; override;
    constructor Create(const Host: AnsiString; Port: Word); reintroduce; overload;
  end;
  PTcpClient = ^TTcpClient;

  TTcpServer = class(TBaseSocket)
  private
    function CheckError(Value: Integer): Integer; override;
  public
    function Activate(Port: Word; const Host: AnsiString = '0.0.0.0'): Boolean;
    function Accept: TTcpSocket;
    function Close: Boolean;
    constructor Create; overload; override;
    constructor Create(Port: Word;
      const Host: AnsiString = '0.0.0.0'); reintroduce; overload;
    destructor Destroy; override;
  end;
  PTcpServer = ^TTcpServer;

{ =========================================================================== }

  TCriticalSection = class(TObject)
  protected
    FSection: TRTLCriticalSection;
  public
    procedure Enter;
    procedure Leave;
    constructor Create;
    destructor Destroy; override;
  end;

  TTcpSocketList = class(TObject)
  private
    FList: array of TTcpSocket;
    FCount: Integer;
    FCriticalSection: TCriticalSection;
    function Get(Index: Integer): TTcpSocket;
  public
    property Items[Index: Integer]: TTcpSocket read Get; default;
    property Count: Integer read FCount;
    function Add(Item: TTcpSocket): Integer;
    function IndexOf(Item: TTcpSocket): Integer;
    procedure Delete(Index: Integer);
    function Remove(Item: TTcpSocket): Integer;
    procedure Clear;
    procedure Lock;
    procedure Unlock;
    constructor Create;
    destructor Destroy; override;
  end;
  PTcpSocketList = ^TTcpSocketList;

  TThreadProc = procedure(Param: Pointer);
  PThreadProc = ^TThreadProc;

function HostToIP(Host: AnsiString): AnsiString;
function IPToHost(IP: AnsiString): AnsiString;
function StartThread(ThreadProc: TThreadProc; Param: Pointer = nil): Cardinal;
function StopThread(ThreadHandle: Cardinal; Delay: Cardinal = 0): Integer;

implementation

{ =========================================================================== }

var
  WSAData: TWSAData;

function HostToAddr(Host: AnsiString): Integer;
var
  He: PHostEnt;
begin
  Result := inet_addr(PAnsiChar(Host));
  if Result <> Integer(INADDR_NONE) then Exit;
  He := gethostbyname(PAnsiChar(Host));
  if He <> nil then
    Move(He^.h_addr_list^^, Result, He^.h_length);
end;

function HostToIP(Host: AnsiString): AnsiString;
var
  He: PHostEnt;
  InAddr: TInAddr;
begin
  Result := '';
  He := gethostbyname(PAnsiChar(Host));
  if He <> nil then
  begin
    Move(He^.h_addr_list^^, InAddr.S_addr, He^.h_length);
    Result := AnsiString(inet_ntoa(InAddr));
  end;
end;

function IPToHost(IP: AnsiString): AnsiString;
var
  He: PHostEnt;
  Addr: Integer;
begin
  Result := IP;
  Addr := inet_addr(PAnsiChar(IP));
  He:= gethostbyaddr(@Addr, SizeOf(Addr), AF_INET);
  if He <> nil then Result := He^.h_name;
end;

procedure InitThread(P: Pointer); stdcall;
var
  Param: Pointer;
  ThreadProc: TThreadProc;
begin
  ThreadProc := PThreadProc(P)^;
  Param := PPointer(PAnsiChar(P) + SizeOf(TThreadProc))^;
  FreeMem(P);
  ThreadProc(Param);
  ExitThread(0);
end;

function StartThread(ThreadProc: TThreadProc; Param: Pointer): Cardinal;
var
  P: PAnsiChar;
  C: Cardinal;
begin
  GetMem(P, SizeOf(TThreadProc) + SizeOf(Pointer));
  PThreadProc(P)^ := ThreadProc;
  PPointer(P + SizeOf(TThreadProc))^ := Param;
  Result := CreateThread(nil, 0, @InitThread, P, 0, C);
  if Result = 0 then FreeMem(P);
end;

function StopThread(ThreadHandle, Delay: Cardinal): Integer;
var
  C: Cardinal;
begin
  Result := -1;
  C := WaitForSingleObject(ThreadHandle, Delay);
  if C = WAIT_TIMEOUT then
  begin
    if TerminateThread(ThreadHandle, 0) then
    begin
      Result := 1;
      CloseHandle(ThreadHandle);
    end;
  end else
    if C <> WAIT_FAILED then Result := 0;
end;

{ =========================================================================== }

function TBaseSocket.GetLocalIP: AnsiString;
var
  Size: Integer;
  SockAddrIn: TSockAddrIn;
begin
  Result := '';
  Size := SizeOf(SockAddrIn);
  if getsockname(FSocket, SockAddrIn, Size) = 0 then
    Result := inet_ntoa(SockAddrIn.sin_addr);
end;

function TBaseSocket.GetLocalPort: Word;
var
  Size: Integer;
  SockAddrIn: TSockAddrIn;
begin
  Result := 0;
  Size := SizeOf(SockAddrIn);
  if getsockname(FSocket, SockAddrIn, Size) = 0 then
    Result := ntohs(SockAddrIn.sin_port);
end;

function TBaseSocket.CheckError(Value: Integer): Integer;
begin
  Result := Value;
  if Result = SOCKET_ERROR then FLastError := WSAGetLastError;
end;

constructor TBaseSocket.Create;
begin
  FSocket := INVALID_SOCKET;
end;

destructor TBaseSocket.Destroy;
begin
  inherited Destroy;
end;

{ =========================================================================== }

function TTcpSocket.GetRemoteIP: AnsiString;
var
  Size: Integer;
  SockAddrIn: TSockAddrIn;
begin
  Result := '';
  Size := SizeOf(SockAddrIn);
  if getpeername(FSocket, SockAddrIn, Size) = 0 then
    Result := inet_ntoa(SockAddrIn.sin_addr);
end;

function TTcpSocket.GetRemotePort: Word;
var
  Size: Integer;
  SockAddrIn: TSockAddrIn;
begin
  Result := 0;
  Size := SizeOf(SockAddrIn);
  if getpeername(FSocket, SockAddrIn, Size) = 0 then
    Result := ntohs(SockAddrIn.sin_port);
end;

function TTcpSocket.CheckError(Value: Integer): Integer;
begin
  Result := inherited CheckError(Value);
  if FActive and (Result = SOCKET_ERROR) then
  begin
    case FLastError of
      WSAETIMEDOUT, WSAENOBUFS: {do nothing};
    else
      FActive := False;
    end;
  end;
end;

procedure TTcpSocket.SetTimeOut(TmOut: Integer);
var
  Int: Integer;
begin
  Int := setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @TmOut, SizeOf(TmOut));
  if CheckError(Int) <> SOCKET_ERROR then FTimeOut := TmOut;
end;

function TTcpSocket.Recv(var Buf; Len, Flags: Integer): Integer;
begin
  Result := CheckError(WinSock.recv(FSocket, Buf, Len, Flags));
  if (Result = 0) and (Len <> 0) then FActive := False;
end;

function TTcpSocket.Send(var Buf; Len, Flags: Integer): Integer;
begin
  Result := CheckError(WinSock.send(FSocket, Buf, Len, Flags));
end;

function TTcpSocket.Write(var Buf; Count: Integer): Integer;
begin
  FWriteCS.Enter;
  Result := Send(Buf, Count, 0);
  FWriteCS.Leave;
end;

function TTcpSocket.Write(const Str: AnsiString): Boolean;
var
  I: Integer;
begin
  I := Length(Str);
  Result := Write(Pointer(Str)^, I) = I;
end;

function TTcpSocket.WriteLn(const Str: AnsiString): Boolean;
var
  I: Integer;
begin
  FWriteCS.Enter;
  Result := False;
  I := Length(Str);
  if Send(Pointer(Str)^, I, 0) = I then
  begin
    I := Length(FEndOfLine);
    Result := Send(Pointer(FEndOfLine)^, I, 0) = I;
  end;
  FWriteCS.Leave;
end;

function TTcpSocket.Read(var Buf; Count: Integer): Integer;
begin
  FReadCS.Enter;
  Result := Recv(Buf, Count, 0);
  FReadCS.Leave;
end;

function TTcpSocket.ReadStr(var Str: AnsiString; MaxLen: Integer): Boolean;
var
  I: Integer;
begin
  SetLength(Str, MaxLen);
  I := Read(Pointer(Str)^, MaxLen);
  Result := I > 0;
  if I < 0 then I := 0;
  SetLength(Str, I);
end;

function TTcpSocket.ReadLn(var Str: AnsiString): Boolean;
var
  S: AnsiString;
  I: Integer;
  Buf: array[0..511] of Byte;
begin
  Str := '';
  FReadCS.Enter;
  repeat
    I := Recv(Buf, SizeOf(Buf), MSG_PEEK);
    Result := I > 0;
    if not Result then Break;
    SetString(S, PAnsiChar(@Buf), I);
    I := Pos(FEndOfLine, S);
    Result := I > 0;
    if Result then SetLength(S, Pred(I + Length(FEndOfLine)));
    Recv(Pointer(S)^, Length(S), 0);
    if Result then SetLength(S, Length(S) - Length(FEndOfLine));
    Str := ConCat(Str, S);
  until Result;
  FReadCS.Leave;
  if not Result then Result := Str <> '';
end;

function TTcpSocket.ReadAll(var Buf; Count: Integer): Integer;
var
  I: Integer;
begin
  FReadCS.Enter;
  Result := 0;
  while Result < Count do
  begin
    I := Recv((PAnsiChar(@Buf) + Result)^, Count - Result, 0);
    if I <= 0 then Break;
    Inc(Result, I);
  end;
  FReadCS.Leave;
end;

function TTcpSocket.Close: Boolean;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    FActive := False;
    FLastError := 0;
    FTimeOut := 0;
    shutdown(FSocket, SD_BOTH);
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
    Result := True;
  end else
    Result := False;
end;

constructor TTcpSocket.Create;
begin
  inherited Create;
  FEndOfLine := CRLF;
  FWriteCS := TCriticalSection.Create;
  FReadCS := TCriticalSection.Create;
end;

destructor TTcpSocket.Destroy;
begin
  Close;
  FWriteCS.Free;
  FReadCS.Free;
  inherited Destroy;
end;

{ =========================================================================== }

function TTcpClient.Open(const Host: AnsiString; Port: Word): Boolean;
var
  Int: Integer;
  SockAddrIn: TSockAddrIn;
  S4H: TSocks4Header;
begin
  Result := False;
  Close;
  FSocket := WinSock.socket(AF_INET, SOCK_STREAM, IPPROTO_IP);
  if CheckError(FSocket) = INVALID_SOCKET then Exit;
  with SockAddrIn do
  begin
    sin_family := AF_INET;
    sin_port := htons(Port);
    sin_addr.s_addr := HostToAddr(Host);
  end;
  Int := connect(FSocket, SockAddrIn, SizeOf(SockAddrIn));
  if CheckError(Int) <> 0 then Exit;
  Result := True;
  FActive := True;
end;

constructor TTcpClient.Create;
begin
  inherited Create;
end;

constructor TTcpClient.Create(const Host: AnsiString; Port: Word);
begin
  Create;
  Open(Host, Port);
end;

{ =========================================================================== }

function TTcpServer.CheckError(Value: Integer): Integer;
begin
  Result := inherited CheckError(Value);
  if FActive and (Result = SOCKET_ERROR) then FActive := False;
end;

function TTcpServer.Activate(Port: Word; const Host: AnsiString): Boolean;
var
  Int: Integer;
  SockAddrIn: TSockAddrIn;
begin
  Close;
  Result := False;
  FSocket := WinSock.socket(AF_INET, SOCK_STREAM, IPPROTO_IP);
  if CheckError(FSocket) = INVALID_SOCKET then Exit;
  with SockAddrIn do
  begin
    sin_family := AF_INET;
    sin_port := htons(Port);
    sin_addr.s_addr := HostToAddr(Host);
  end;
  Int := bind(FSocket, SockAddrIn, SizeOf(SockAddrIn));
  if CheckError(Int) = SOCKET_ERROR then Exit;
  Int := listen(FSocket, SOMAXCONN);
  if CheckError(Int) <> 0 then Exit;
  Result := True;
  FActive := True;
end;

function TTcpServer.Accept: TTcpSocket;
var
  Size: Integer;
  Sock: TSocket;
  SockAddrIn: TSockAddrIn;
begin
  Result := nil;
  Size := SizeOf(SockAddrIn);
  Sock := WinSock.accept(FSocket, @SockAddrIn, @Size);
  if CheckError(Sock) <> INVALID_SOCKET then
  begin
    Result := TTcpSocket.Create;
    Result.FSocket := Sock;
    Result.FActive := True;
  end;
end;

function TTcpServer.Close: Boolean;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    FActive := False;
    FLastError := 0;
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
    Result := True;
  end else
    Result := False;
end;

constructor TTcpServer.Create;
begin
  inherited Create;
end;

constructor TTcpServer.Create(Port: Word; const Host: AnsiString);
begin
  Create;
  Activate(Port, Host);
end;

destructor TTcpServer.Destroy;
begin
  Close;
  inherited Destroy;
end;

{ =========================================================================== }

procedure TCriticalSection.Enter;
begin
  EnterCriticalSection(FSection);
end;

procedure TCriticalSection.Leave;
begin
  LeaveCriticalSection(FSection);
end;

constructor TCriticalSection.Create;
begin
  InitializeCriticalSection(FSection);
end;

destructor TCriticalSection.Destroy;
begin
  DeleteCriticalSection(FSection);
  inherited Destroy;
end;

{ =========================================================================== }

function TTcpSocketList.Add(Item: TTcpSocket): Integer;
begin
  Lock;
  Result := FCount;
  Inc(FCount);
  SetLength(FList, FCount);
  FList[Result] := Item;
  Unlock;
end;

function TTcpSocketList.Get(Index: Integer): TTcpSocket;
begin
  Lock;
  if (Index >= 0) and (Index < FCount) then
    Result := FList[Index] else Result := nil;
  Unlock;
end;

function TTcpSocketList.IndexOf(Item: TTcpSocket): Integer;
begin
  Lock;
  Result := 0;
  while (Result < FCount) and (FList[Result] <> Item) do Inc(Result);
  if Result = FCount then Result := -1;
  Unlock;
end;

procedure TTcpSocketList.Delete(Index: Integer);
begin
  Lock;
  if (Index >= 0) and (Index < FCount) then
  begin
    Dec(FCount);
    if Index < FCount then Move(FList[Succ(Index)],
      FList[Index], (FCount - Index) * SizeOf(TTcpSocket));
    SetLength(FList, FCount);
  end;
  Unlock;
end;

function TTcpSocketList.Remove(Item: TTcpSocket): Integer;
begin
  Lock;
  Result := IndexOf(Item);
  if Result <> -1 then Delete(Result);
  Unlock;
end;

procedure TTcpSocketList.Clear;
begin
  Lock;
  FCount := 0;
  SetLength(FList, FCount);
  Unlock;
end;

procedure TTcpSocketList.Lock;
begin
  FCriticalSection.Enter;
end;

procedure TTcpSocketList.Unlock;
begin
  FCriticalSection.Leave;
end;

constructor TTcpSocketList.Create;
begin
  FCriticalSection := TCriticalSection.Create;
end;

destructor TTcpSocketList.Destroy;
begin
  Clear;
  FCriticalSection.Free;
  inherited Destroy;
end;

{ =========================================================================== }

initialization
  WSAStartUp(MAKEWORD(1, 1), WSAData);

finalization
  WSACleanUp();

end.
 
