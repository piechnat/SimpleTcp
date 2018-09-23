program Client;

{$APPTYPE CONSOLE}

uses SimpleTcp;

var
  Str: AnsiString;
  Clnt: TTcpClient;
begin
  Clnt := TTcpClient.Create('www.google.pl', 80);
  Clnt.WriteLn('GET / HTTP/1.0' + CRLF + 'Host: www.google.pl' + CRLF);
  while Clnt.ReadLn(Str) do WriteLn(Str);
  Clnt.Free;
  ReadLn;
end.
