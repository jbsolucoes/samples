unit http;

interface

uses
  Classes, blcksock, winsock, Synautil, SysUtils, fpjson, jsonparser,
  variants, DB, Graphics, log, sndkey32;

type
  TTCPHttpDaemon = class(TThread)
  private
    Sock: TTCPBlockSocket;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TTCPHttpThrd }

  TTCPHttpThrd = class(TThread)
  private
    Sock: TTCPBlockSocket;
  public
    Headers: TStringList;
    InputData, OutputData: TMemoryStream;
    constructor Create(hsock: tSocket);
    destructor Destroy; override;
    procedure Execute; override;
    procedure UpdateCalls;
    function ProcessHttpRequest(Request, URI, RequestData: string): integer;
    function ProcessRequest(ClientLocation, RequestFunction: string;
      RequestParams: array of variant; RequestID: string): string;
    procedure Log(msg: string);
  end;

var
  Json: TJSONObject;

implementation

uses libutil, main;

{ TTCPHttpDaemon }

constructor TTCPHttpDaemon.Create;
begin
  inherited Create(False);
  sock := TTCPBlockSocket.Create;
  FreeOnTerminate := True;
  Priority := tpNormal;
end;

destructor TTCPHttpDaemon.Destroy;
begin
  Sock.Free;
  inherited Destroy;
end;

procedure TTCPHttpDaemon.Execute;
var
  ClientSock: TSocket;
begin
  with sock do
  begin
    CreateSocket;
    setLinger(True, 10);
    bind('0.0.0.0', '3088');
    listen;
    repeat
      if terminated then
        break;
      if canread(500) then
      begin
        ClientSock := accept;
        if lastError = 0 then
          TTCPHttpThrd.Create(ClientSock);
      end;
    until False;
  end;
end;

{ TTCPHttpThrd }

constructor TTCPHttpThrd.Create(Hsock: TSocket);
begin
  inherited Create(False);
  sock := TTCPBlockSocket.Create;
  Headers := TStringList.Create;
  InputData := TMemoryStream.Create;
  OutputData := TMemoryStream.Create;
  Sock.socket := HSock;
  FreeOnTerminate := True;
  Priority := tpNormal;
end;

destructor TTCPHttpThrd.Destroy;
begin
  Sock.Free;
  Headers.Free;
  InputData.Free;
  OutputData.Free;
  inherited Destroy;
end;

procedure TTCPHttpThrd.Execute;
var
  timeout: integer;
  s: string;
  method, uri, protocol: string;
  size: integer;
  x, n: integer;
  resultcode: integer;
  slData: TStringList;
begin
  timeout := 12000;
  //read request line
  s := sock.RecvString(timeout);
  if sock.lasterror <> 0 then
    Exit;
  if s = '' then
    Exit;
  method := fetch(s, ' ');
  if (s = '') or (method = '') then
    Exit;
  uri := fetch(s, ' ');
  if uri = '' then
    Exit;
  protocol := fetch(s, ' ');
  headers.Clear;
  size := -1;
  //read request headers
  if protocol <> '' then
  begin
    if pos('HTTP/', protocol) <> 1 then
      Exit;
    repeat
      s := sock.RecvString(Timeout);
      if sock.lasterror <> 0 then
        Exit;
      if s <> '' then
        Headers.add(s);
      if Pos('CONTENT-LENGTH:', Uppercase(s)) = 1 then
        Size := StrToIntDef(SeparateRight(s, ' '), -1);
    until s = '';
  end;
  //recv document...
  InputData.Clear;
  if size >= 0 then
  begin
    InputData.SetSize(Size);
    x := Sock.RecvBufferEx(InputData.Memory, Size, Timeout);
    InputData.SetSize(x);
    if sock.lasterror <> 0 then
      Exit;
  end;

  InputData.Seek(0, soFromBeginning);

  try
    slData := TStringList.Create;

    slData.Clear;
    slData.LoadFromStream(InputData);

    OutputData.Clear;

    ResultCode := ProcessHttpRequest(method, uri, slData.Text);

  finally
    FreeAndNil(slData);
  end;

  sock.SendString('HTTP/1.0 ' + IntToStr(ResultCode) + CRLF);

  if protocol <> '' then
  begin
    headers.Add('Content-length: ' + IntToStr(OutputData.Size));
    headers.Add('Connection: close');
    headers.Add('Date: ' + Rfc822DateTime(now));
    headers.Add('Server: BCP2PC');
    headers.Add('');
    for n := 0 to headers.Count - 1 do
      sock.sendstring(headers[n] + CRLF);
  end;
  if sock.lasterror <> 0 then
    Exit;
  Sock.SendBuffer(OutputData.Memory, OutputData.Size);
end;

procedure TTCPHttpThrd.UpdateCalls;
begin
  Form1.label2.Caption := IntToStr(StrToInt(Form1.label2.Caption) + 1);
end;

function TTCPHttpThrd.ProcessHttpRequest(Request, URI, RequestData: string): integer;
var
  l: TStringList;
  I, vCount: integer;
  ReqObject: TJSONObject;
  Parser: TJSONParser;
  ReqContent, ReqFunction, ReqID, sParam: string;
  ArrParams: array of variant;
  OutputDataString, ResContent: string;
  CheckFileName, ContentType: string;
begin
  Result := 504;

  if request = 'GET' then
  begin
    headers.Clear;

    CheckFileName := UriToFileName(uri);

    ContentType := GetContentType(CheckFileName);

    headers.Add(Format('Content-type: %s', [ContentType]));

    if IsImage(CheckFileName) or IsFileDownload(CheckFileName) then
    begin
      if FileExists(CheckFileName) then
      begin
        try
          OutputData.LoadFromFile(CheckFileName);
        finally
          Result := 200;
        end;
      end
      else
        Result := GetPageResult(404, OutputData);
    end
    else
    begin
      if FileExists(CheckFileName) then
      begin
        l := TStringList.Create;
        try
          l.LoadFromFile(CheckFileName);
          l.SaveToStream(OutputData);
        finally
          l.Free;
        end;

        Result := 200;
      end
      else
      begin
        headers.Clear;

        headers.Add('Content-type: text/html');

        Result := GetPageResult(404, OutputData);
      end;
    end;
  end
  else
  if request = 'POST' then
  begin
    try
      ReqContent := RequestData;

      if trim(ReqContent) <> '' then
      begin
        Parser := TJSONParser.Create(ReqContent);

        ReqObject := Parser.Parse as TJSONObject;

        ReqFunction := ReqObject.Elements['method'].Value;

        ReqID := ReqObject.Elements['id'].AsJSON;

        if Parser <> nil then
        begin
          vCount := ReqObject.Elements['params'].Count;

          ArrParams := VarArrayCreate([1, vCount], vtVariant);

          for i := 0 to vCount - 1 do
          begin
            sParam := ReqObject.Elements['params'].Items[i].AsJSON;

            ArrParams[i] := sParam;
          end;

          ResContent := ProcessRequest(Sock.GetRemoteSinIP, ReqFunction,
            ArrParams, ReqID);

          OutputDataString := format('{"jsonrpc": "2.0", "result": %s, "id": %s}',
            [ResContent, ReqID]);
        end
        else
        begin
          OutputDataString :=
            '{"jsonrpc": "2.0", "result": "Request Invalid Content", "id": 1}';
        end;
      end
      else
      begin
        OutputDataString := '{"jsonrpc": "2.0", "result": "Invalid command", "id": 1}';
      end;

      headers.Clear;
      headers.Add('application/json');

      l := TStringList.Create;
      try
        l.Text := OutputDataString;
        l.SaveToStream(OutputData);
      finally
        l.Free;
      end;

      Result := 200;

    finally
      if trim(ReqContent) <> '' then
      begin
        FreeAndNil(parser);

        FreeAndNil(ReqObject);
      end;
    end;
  end;
end;

function TTCPHttpThrd.ProcessRequest(ClientLocation, RequestFunction: string;
  RequestParams: array of variant; RequestID: string): string;
var
  params: string;
  sResult: string;
begin
  params := vartostr(RequestParams[0]);

  if RequestFunction = 'READBARCODE' then
  begin
    SendKeys(PChar(stringreplace(params,'"','',[rfReplaceAll])),true);
    //SendKeys(#13#10,true); //send return key
    Log(stringreplace(params,'"','',[rfReplaceAll]));
    sResult := '"OK"';
  end
  else
    sResult := '"NOK"';

  Result := sResult;

  Synchronize(UpdateCalls);
end;

procedure TTCPHttpThrd.Log(msg: string);
begin
  if frmLog <> nil then
    frmLog.memLog.Lines.Add(msg);
end;


end.
