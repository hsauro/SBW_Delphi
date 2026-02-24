unit SBW.Connection;

{******************************************************************************
 * SBW.Connection.pas
 *
 * TCP socket connection handling for the Systems Biology Workbench.
 *
 * Provides:
 * - TSBWConnection: Client connection to a broker (bidirectional)
 * - TSBWConnectionListener: Server socket for the broker
 *
 * Handshake Protocol:
 * 1. Client connects to broker
 * 2. Client sends: [length(4)] [module_name(string with type prefix)]
 * 3. Broker replies: [length(4)] [assigned_module_id(4)]
 *
 * Bidirectional Communication:
 * After connecting, a background receive thread handles all incoming messages:
 * - Replies/Errors are matched to pending calls and signaled
 * - Calls/Sends are dispatched to the OnMessage handler
 * This allows a module to both serve methods AND call other modules.
 *****************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Generics.Collections,
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.WinSock,
  {$ENDIF}
  SBW.Types, SBW.DataBlock, SBW.Message;

type
  ESBWConnectionError = class(Exception);
  ESBWConnectionClosed = class(ESBWConnectionError);
  ESBWConnectionTimeout = class(ESBWConnectionError);

  /// <summary>
  /// Event fired when a message is received
  /// </summary>
  TSBWMessageEvent = procedure(Sender: TObject; const Msg: TSBWMessage) of object;

  /// <summary>
  /// Event fired when connection is closed
  /// </summary>
  TSBWDisconnectEvent = procedure(Sender: TObject) of object;

  /// <summary>
  /// Low-level socket wrapper
  /// </summary>
  TSBWSocket = class
  private
    FSocket: TSocket;
    FConnected: Boolean;
    FLock: TCriticalSection;
  public
    constructor Create; overload;
    constructor Create(ASocket: TSocket); overload;
    destructor Destroy; override;

    procedure Connect(const Host: string; Port: Word);
    procedure Disconnect;

    /// <summary>
    /// Send all bytes, blocks until complete
    /// </summary>
    procedure SendBytes(const Data: TBytes);

    /// <summary>
    /// Receive exact number of bytes, blocks until complete
    /// </summary>
    function ReceiveBytes(Count: Integer): TBytes;

    /// <summary>
    /// Check if data is available to read (non-blocking)
    /// </summary>
    function DataAvailable: Boolean;

    property Connected: Boolean read FConnected;
    property Handle: TSocket read FSocket;
  end;

  TSBWConnection = class;

  /// <summary>
  /// Background thread that receives messages
  /// </summary>
  TSBWReceiveThread = class(TThread)
  private
    FConnection: TSBWConnection;
  protected
    procedure Execute; override;
  public
    constructor Create(AConnection: TSBWConnection);
  end;

  /// <summary>
  /// Client connection to an SBW broker.
  /// Supports bidirectional communication - can both serve and call methods.
  /// </summary>
  TSBWConnection = class
  private
    FSocket: TSBWSocket;
    FModuleID: SBWModuleID;
    FModuleName: string;
    FHost: string;
    FPort: Word;
    FConnected: Boolean;
    FOnMessage: TSBWMessageEvent;
    FOnDisconnect: TSBWDisconnectEvent;
    FReceiveThread: TSBWReceiveThread;
    FPendingCalls: TDictionary<SBWInteger, TSBWMessage>;
    FPendingCallEvents: TDictionary<SBWInteger, TEvent>;
    FPendingLock: TCriticalSection;
    FSendLock: TCriticalSection;
    FThreadedMode: Boolean;

    procedure HandleIncomingMessage(Msg: TSBWMessage);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Connect to the broker and register this module.
    /// If StartReceiveThread is True, starts background thread for bidirectional communication.
    /// </summary>
    procedure Connect(const Host: string; Port: Word; const ModuleName: string;
      StartReceiveThread: Boolean = False);

    /// <summary>
    /// Start the receive thread for bidirectional communication.
    /// Call this after Connect if you need to both serve and call methods.
    /// </summary>
    procedure StartReceiveThread;

    /// <summary>
    /// Disconnect from the broker
    /// </summary>
    procedure Disconnect;

    /// <summary>
    /// Send a call and wait for reply.
    /// Works in both threaded and non-threaded modes.
    /// </summary>
    function Call(DestinationID: SBWModuleID; ServiceID: SBWServiceID;
      MethodID: SBWMethodID; const Args: TBytes; TimeoutMs: Cardinal = 30000): TBytes;

    /// <summary>
    /// Send raw bytes (for low-level use)
    /// </summary>
    procedure SendRaw(const Data: TBytes);

    /// <summary>
    /// Receive a complete message (blocking).
    /// Only use this if NOT in threaded mode.
    /// </summary>
    function ReceiveMessage: TSBWMessage;

    /// <summary>
    /// Check if running in threaded mode (background receive thread active)
    /// </summary>
    property ThreadedMode: Boolean read FThreadedMode;

    property Socket: TSBWSocket read FSocket;
    property ModuleID: SBWModuleID read FModuleID;
    property ModuleName: string read FModuleName;
    property Connected: Boolean read FConnected;
    property OnMessage: TSBWMessageEvent read FOnMessage write FOnMessage;
    property OnDisconnect: TSBWDisconnectEvent read FOnDisconnect write FOnDisconnect;
  end;

  /// <summary>
  /// Represents a connected client on the broker side
  /// </summary>
  TSBWClientConnection = class
  private
    FSocket: TSBWSocket;
    FModuleID: SBWModuleID;
    FModuleName: string;
    FConnected: Boolean;
  public
    constructor Create(ASocket: TSBWSocket; AModuleID: SBWModuleID);
    destructor Destroy; override;

    procedure SendMessage(const Data: TBytes);
    function ReceiveMessage: TSBWMessage;

    property Socket: TSBWSocket read FSocket;
    property ModuleID: SBWModuleID read FModuleID;
    property ModuleName: string read FModuleName write FModuleName;
    property Connected: Boolean read FConnected write FConnected;
  end;

  /// <summary>
  /// Event for new client connections
  /// </summary>
  TSBWClientConnectEvent = procedure(Sender: TObject; Client: TSBWClientConnection) of object;

  /// <summary>
  /// Server socket listener for the broker
  /// </summary>
  TSBWConnectionListener = class
  private
    FSocket: TSocket;
    FPort: Word;
    FListening: Boolean;
    FOnClientConnect: TSBWClientConnectEvent;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Start listening on the specified port
    /// </summary>
    procedure Start(Port: Word);

    /// <summary>
    /// Stop listening
    /// </summary>
    procedure Stop;

    /// <summary>
    /// Accept a new connection (blocking)
    /// </summary>
    function Accept: TSBWSocket;

    property Port: Word read FPort;
    property Listening: Boolean read FListening;
    property OnClientConnect: TSBWClientConnectEvent read FOnClientConnect write FOnClientConnect;
  end;


  implementation

var
  GSocketsInitialized: Boolean = False;
  GSocketsRefCount: Integer = 0;
  GSocketsLock: TCriticalSection;

procedure AcquireSockets;
{$IFDEF MSWINDOWS}
var
  WSAData: TWSAData;
  Res: Integer;
{$ENDIF}
begin
  GSocketsLock.Enter;
  try
    Inc(GSocketsRefCount);
    if not GSocketsInitialized then
    begin
      {$IFDEF MSWINDOWS}
      Res := WSAStartup($0202, WSAData);
      if Res <> 0 then
        raise ESBWConnectionError.CreateFmt('Failed to initialize Winsock (error %d)', [Res]);
      {$ENDIF}
      GSocketsInitialized := True;
    end;
  finally
    GSocketsLock.Leave;
  end;
end;

procedure ReleaseSockets;
begin
  GSocketsLock.Enter;
  try
    Dec(GSocketsRefCount);
    if (GSocketsRefCount <= 0) and GSocketsInitialized then
    begin
      {$IFDEF MSWINDOWS}
      WSACleanup;
      {$ENDIF}
      GSocketsInitialized := False;
      GSocketsRefCount := 0;
    end;
  finally
    GSocketsLock.Leave;
  end;
end;

{ TSBWSocket }

constructor TSBWSocket.Create;
begin
  inherited Create;
  FSocket := INVALID_SOCKET;
  FConnected := False;
  FLock := TCriticalSection.Create;
end;

constructor TSBWSocket.Create(ASocket: TSocket);
begin
  inherited Create;
  FSocket := ASocket;
  FConnected := ASocket <> INVALID_SOCKET;
  FLock := TCriticalSection.Create;
end;

destructor TSBWSocket.Destroy;
begin
  Disconnect;        // Call the Disconnect method
  FLock.Free;
  ReleaseSockets;    // Auto-cleanup when last socket freed
  inherited;
end;

procedure TSBWSocket.Connect(const Host: string; Port: Word);
var
  Addr: TSockAddrIn;
  HostEnt: PHostEnt;
  IP: Cardinal;
begin
  inherited Create;
  AcquireSockets;  // Auto-initialize on first socket
  FSocket := INVALID_SOCKET;

  FSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FSocket = INVALID_SOCKET then
    raise ESBWConnectionError.Create('Failed to create socket');

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);

  // Try to parse as IP address first
  IP := inet_addr(PAnsiChar(AnsiString(Host)));
  if IP <> INADDR_NONE then
  begin
    Addr.sin_addr.S_addr := IP;
  end
  else
  begin
    // Resolve hostname
    HostEnt := gethostbyname(PAnsiChar(AnsiString(Host)));
    if HostEnt = nil then
      raise ESBWConnectionError.CreateFmt('Failed to resolve host: %s', [Host]);
    Addr.sin_addr.S_addr := PCardinal(HostEnt^.h_addr_list^)^;
  end;

  if Winapi.WinSock.connect(FSocket, TSockAddr(Addr), SizeOf(Addr)) <> 0 then
  begin
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
    raise ESBWConnectionError.CreateFmt('Failed to connect to %s:%d', [Host, Port]);
  end;

  FConnected := True;
end;

procedure TSBWSocket.Disconnect;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
  FConnected := False;
end;

procedure TSBWSocket.SendBytes(const Data: TBytes);
var
  Sent, Total, Remaining: Integer;
  Ptr: PByte;
begin
  if not FConnected then
    raise ESBWConnectionClosed.Create('Socket not connected');

  FLock.Enter;
  try
    Total := Length(Data);
    Remaining := Total;
    Ptr := @Data[0];

    while Remaining > 0 do
    begin
      Sent := send(FSocket, Ptr^, Remaining, 0);
      if Sent <= 0 then
      begin
        FConnected := False;
        raise ESBWConnectionClosed.Create('Connection closed during send');
      end;
      Inc(Ptr, Sent);
      Dec(Remaining, Sent);
    end;
  finally
    FLock.Leave;
  end;
end;

function TSBWSocket.ReceiveBytes(Count: Integer): TBytes;
var
  Received, Total, Remaining: Integer;
  Ptr: PByte;
begin
  if not FConnected then
    raise ESBWConnectionClosed.Create('Socket not connected');

  SetLength(Result, Count);
  if Count = 0 then
    Exit;

  Total := Count;
  Remaining := Total;
  Ptr := @Result[0];

  while Remaining > 0 do
  begin
    Received := recv(FSocket, Ptr^, Remaining, 0);
    if Received <= 0 then
    begin
      FConnected := False;
      raise ESBWConnectionClosed.Create('Connection closed during receive');
    end;
    Inc(Ptr, Received);
    Dec(Remaining, Received);
  end;
end;

function TSBWSocket.DataAvailable: Boolean;
var
  FDSet: TFDSet;
  Timeout: TTimeVal;
  Ret: Integer;
begin
  if not FConnected then
    Exit(False);

  FD_ZERO(FDSet);
  FD_SET(FSocket, FDSet);
  Timeout.tv_sec := 0;
  Timeout.tv_usec := 0;

  Ret := select(0, @FDSet, nil, nil, @Timeout);
  Result := Ret > 0;
end;

{ TSBWReceiveThread }

constructor TSBWReceiveThread.Create(AConnection: TSBWConnection);
begin
  inherited Create(True); // Create suspended
  FreeOnTerminate := False;
  FConnection := AConnection;
end;

procedure TSBWReceiveThread.Execute;
var
  Msg: TSBWMessage;
begin
  while not Terminated and FConnection.Connected do
  begin
    try
      if FConnection.Socket.DataAvailable then
      begin
        Msg := FConnection.ReceiveMessage;
        FConnection.HandleIncomingMessage(Msg);
      end
      else
        Sleep(5);
    except
      on E: ESBWConnectionClosed do
      begin
        if Assigned(FConnection.OnDisconnect) then
          FConnection.OnDisconnect(FConnection);
        Break;
      end;
      on E: Exception do
      begin
        // Log error but continue unless terminated
        if Terminated then
          Break;
      end;
    end;
  end;
end;

{ TSBWConnection }

constructor TSBWConnection.Create;
begin
  inherited Create;
  FSocket := TSBWSocket.Create;
  FModuleID := SBW_NO_MODULE_ID;
  FConnected := False;
  FThreadedMode := False;
  FPendingCalls := TDictionary<SBWInteger, TSBWMessage>.Create;
  FPendingCallEvents := TDictionary<SBWInteger, TEvent>.Create;
  FPendingLock := TCriticalSection.Create;
  FSendLock := TCriticalSection.Create;
end;

destructor TSBWConnection.Destroy;
begin
  Disconnect;
  FPendingCallEvents.Free;
  FPendingCalls.Free;
  FPendingLock.Free;
  FSendLock.Free;
  FSocket.Free;
  inherited;
end;

procedure TSBWConnection.Connect(const Host: string; Port: Word; const ModuleName: string;
  StartReceiveThread: Boolean);
var
  Writer: TSBWDataBlockWriter;
  HandshakeData: TBytes;
  LengthBytes: TBytes;
  ResponseLength: SBWInteger;
  ResponseData: TBytes;
begin
  FHost := Host;
  FPort := Port;
  FModuleName := ModuleName;

  // Connect socket
  FSocket.Connect(Host, Port);

  // Send handshake: [length(4)] [module_name(string)]
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteString(ModuleName);
    HandshakeData := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  // Prepend length
  SetLength(LengthBytes, 4);
  var TotalLength: SBWInteger := 4 + Length(HandshakeData);
  Move(TotalLength, LengthBytes[0], 4);

  FSocket.SendBytes(LengthBytes);
  FSocket.SendBytes(HandshakeData);

  // Receive response: [length(4)] [module_id(4)]
  LengthBytes := FSocket.ReceiveBytes(4);
  Move(LengthBytes[0], ResponseLength, 4);

  ResponseData := FSocket.ReceiveBytes(ResponseLength - 4);
  Move(ResponseData[0], FModuleID, 4);

  FConnected := True;

  // Optionally start receive thread for bidirectional communication
  if StartReceiveThread then
    Self.StartReceiveThread;
end;

procedure TSBWConnection.StartReceiveThread;
begin
  if FReceiveThread <> nil then
    Exit; // Already running
    
  FThreadedMode := True;
  FReceiveThread := TSBWReceiveThread.Create(Self);
  FReceiveThread.Start;
end;

procedure TSBWConnection.Disconnect;
begin
  FConnected := False;
  FThreadedMode := False;
  if FReceiveThread <> nil then
  begin
    FReceiveThread.Terminate;
    FReceiveThread.WaitFor;
    FreeAndNil(FReceiveThread);
  end;
  FSocket.Disconnect;
  FModuleID := SBW_NO_MODULE_ID;
end;

procedure TSBWConnection.SendRaw(const Data: TBytes);
begin
  FSendLock.Enter;
  try
    FSocket.SendBytes(Data);
  finally
    FSendLock.Leave;
  end;
end;

function TSBWConnection.Call(DestinationID: SBWModuleID; ServiceID: SBWServiceID;
  MethodID: SBWMethodID; const Args: TBytes; TimeoutMs: Cardinal): TBytes;
var
  UID: SBWInteger;
  CallData: TBytes;
  Response: TSBWMessage;
  ReplyMsg: TSBWReplyMessage;
  ErrorMsg: TSBWErrorMessage;
  WaitEvent: TEvent;
  WaitResult: TWaitResult;
begin
  UID := TSBWUIDGenerator.NextUID;

  // Build call message
  CallData := TSBWMessageWriter.BuildCallMessage(
    DestinationID, FModuleID, UID, ServiceID, MethodID, Args, False);

  if FThreadedMode then
  begin
    // Threaded mode: register wait event, send, wait for receive thread to signal
    WaitEvent := TEvent.Create(nil, True, False, '');
    try
      FPendingLock.Enter;
      try
        FPendingCallEvents.Add(UID, WaitEvent);
      finally
        FPendingLock.Leave;
      end;

      // Send the call
      SendRaw(CallData);

      // Wait for response
      WaitResult := WaitEvent.WaitFor(TimeoutMs);

      FPendingLock.Enter;
      try
        FPendingCallEvents.Remove(UID);

        if WaitResult = wrTimeout then
        begin
          FPendingCalls.Remove(UID);
          raise ESBWConnectionTimeout.Create('Call timed out');
        end;

        if not FPendingCalls.TryGetValue(UID, Response) then
          raise ESBWConnectionError.Create('Response not found after signal');

        FPendingCalls.Remove(UID);
      finally
        FPendingLock.Leave;
      end;
    finally
      WaitEvent.Free;
    end;
  end
  else
  begin
    // Non-threaded mode: send and directly receive response
    FSendLock.Enter;
    try
      FSocket.SendBytes(CallData);
    finally
      FSendLock.Leave;
    end;

    Response := ReceiveMessage;
  end;

  // Process the response
  try
    // Verify the UID matches
    if Response.UID <> UID then
      raise ESBWConnectionError.CreateFmt('UID mismatch: expected %d, got %d',
        [UID, Response.UID]);

    if Response is TSBWReplyMessage then
    begin
      ReplyMsg := Response as TSBWReplyMessage;
      Result := ReplyMsg.Payload;
    end
    else if Response is TSBWErrorMessage then
    begin
      ErrorMsg := Response as TSBWErrorMessage;
      raise ESBWConnectionError.CreateFmt('%s: %s',
        [ErrorMsg.ErrorMessage, ErrorMsg.DetailedMessage]);
    end
    else
      raise ESBWConnectionError.CreateFmt('Unexpected response type: %d',
        [Ord(Response.MessageType)]);
  finally
    Response.Free;
  end;
end;

function TSBWConnection.ReceiveMessage: TSBWMessage;
var
  LengthBytes: TBytes;
  MsgLength: SBWInteger;
  MsgData: TBytes;
begin
  // Read length
  LengthBytes := FSocket.ReceiveBytes(4);
  Move(LengthBytes[0], MsgLength, 4);

  // Read rest of message
  SetLength(MsgData, MsgLength);
  Move(LengthBytes[0], MsgData[0], 4);

  if MsgLength > 4 then
  begin
    var RestData := FSocket.ReceiveBytes(MsgLength - 4);
    Move(RestData[0], MsgData[4], MsgLength - 4);
  end;

  Result := TSBWMessageReader.ParseMessage(MsgData);
end;

procedure TSBWConnection.HandleIncomingMessage(Msg: TSBWMessage);
var
  WaitEvent: TEvent;
begin
  if (Msg.MessageType = mtReply) or (Msg.MessageType = mtError) then
  begin
    // This is a response to a pending call
    FPendingLock.Enter;
    try
      if FPendingCallEvents.TryGetValue(Msg.UID, WaitEvent) then
      begin
        // Store the message for the caller and signal the event
        FPendingCalls.Add(Msg.UID, Msg);
        WaitEvent.SetEvent;
        Exit; // Don't free the message, it's stored for the caller
      end;
    finally
      FPendingLock.Leave;
    end;
  end;

  // Fire event for incoming calls/sends
  if Assigned(FOnMessage) then
  begin
    FOnMessage(Self, Msg);
    // Message is freed by the handler via OnMessage
  end
  else
  begin
    // No handler, free the message
    Msg.Free;
  end;
end;

{ TSBWClientConnection }

constructor TSBWClientConnection.Create(ASocket: TSBWSocket; AModuleID: SBWModuleID);
begin
  inherited Create;
  FSocket := ASocket;
  FModuleID := AModuleID;
  FConnected := True;
end;

destructor TSBWClientConnection.Destroy;
begin
  FSocket.Free;
  inherited;
end;

procedure TSBWClientConnection.SendMessage(const Data: TBytes);
begin
  FSocket.SendBytes(Data);
end;

function TSBWClientConnection.ReceiveMessage: TSBWMessage;
var
  LengthBytes: TBytes;
  MsgLength: SBWInteger;
  MsgData: TBytes;
begin
  // Read length
  LengthBytes := FSocket.ReceiveBytes(4);
  Move(LengthBytes[0], MsgLength, 4);

  // Read complete message
  SetLength(MsgData, MsgLength);
  Move(LengthBytes[0], MsgData[0], 4);

  if MsgLength > 4 then
  begin
    var RestData := FSocket.ReceiveBytes(MsgLength - 4);
    Move(RestData[0], MsgData[4], MsgLength - 4);
  end;

  Result := TSBWMessageReader.ParseMessage(MsgData);
end;

{ TSBWConnectionListener }

constructor TSBWConnectionListener.Create;
begin
  inherited Create;
  FSocket := INVALID_SOCKET;
  FListening := False;
end;

destructor TSBWConnectionListener.Destroy;
begin
  Stop;
  inherited;
end;

procedure TSBWConnectionListener.Start(Port: Word);
var
  Addr: TSockAddrIn;
  OptVal: Integer;
begin
  AcquireSockets;

  FSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FSocket = INVALID_SOCKET then
    raise ESBWConnectionError.Create('Failed to create server socket');

  // Allow address reuse
  OptVal := 1;
  setsockopt(FSocket, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);
  Addr.sin_addr.S_addr := INADDR_ANY;

  if bind(FSocket, TSockAddr(Addr), SizeOf(Addr)) <> 0 then
  begin
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
    raise ESBWConnectionError.CreateFmt('Failed to bind to port %d', [Port]);
  end;

  if listen(FSocket, SOMAXCONN) <> 0 then
  begin
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
    raise ESBWConnectionError.Create('Failed to listen on socket');
  end;

  FPort := Port;
  FListening := True;
end;

procedure TSBWConnectionListener.Stop;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
    ReleaseSockets;
  end;
  FListening := False;
end;

function TSBWConnectionListener.Accept: TSBWSocket;
var
  ClientSocket: TSocket;
  ClientAddr: TSockAddrIn;
  AddrLen: Integer;
begin
  if not FListening then
    raise ESBWConnectionError.Create('Not listening');

  AddrLen := SizeOf(ClientAddr);
  ClientSocket := Winapi.WinSock.accept(FSocket, @ClientAddr, @AddrLen);

  if ClientSocket = INVALID_SOCKET then
    raise ESBWConnectionError.Create('Accept failed');

  Result := TSBWSocket.Create(ClientSocket);
end;

initialization
 if GSocketsLock = nil then
    GSocketsLock := TCriticalSection.Create;

finalization
  // Safety cleanup if someone forgot
  if GSocketsInitialized then
    WSACleanup;
  GSocketsLock.Free;
end.
