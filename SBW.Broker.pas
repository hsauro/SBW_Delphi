unit SBW.Broker;

(******************************************************************************
 * SBW.Broker.pas
 *
 * SBW Broker implementation - Core message router.
 *
 * The broker is the central message router for SBW. It:
 * - Accepts connections from modules
 * - Assigns module IDs
 * - Routes messages between modules
 * - Tracks registered services
 *
 * Service handlers are in separate units:
 * - SBW.Broker.ModuleService: Module System Service (service -1)
 * - SBW.Broker.SystemService: Broker System Service (service 0)
 *****************************************************************************)

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Generics.Collections,
  SBW.Types, SBW.DataBlock, SBW.Message, SBW.Connection, SBW.BrokerTypes,
  SBW.Broker.Registry, SBW.Broker.ModuleService;

const
  VERSION = '0.52';

type
  TSBWBroker = class;

  /// <summary>
  /// Thread that handles communication with a single client
  /// </summary>
  TSBWClientThread = class(TThread)
  private
    FBroker: TSBWBroker;
    FClient: TSBWClientConnection;
    FModuleInfo: TSBWModuleInfo;
  protected
    procedure Execute; override;
  public
    constructor Create(ABroker: TSBWBroker; AClient: TSBWClientConnection;
      AModuleInfo: TSBWModuleInfo);
  end;

  /// <summary>
  /// Thread that accepts new connections
  /// </summary>
  TSBWAcceptThread = class(TThread)
  private
    FBroker: TSBWBroker;
    FListener: TSBWConnectionListener;
  protected
    procedure Execute; override;
  public
    constructor Create(ABroker: TSBWBroker; AListener: TSBWConnectionListener);
  end;

  /// <summary>
  /// Event fired when a module connects
  /// </summary>
  TSBWModuleConnectEvent = procedure(Sender: TObject; ModuleInfo: TSBWModuleInfo) of object;

  /// <summary>
  /// Event fired when a module disconnects
  /// </summary>
  TSBWModuleDisconnectEvent = procedure(Sender: TObject; ModuleID: SBWModuleID) of object;

  /// <summary>
  /// Event for logging
  /// </summary>
  TSBWLogEvent = procedure(Sender: TObject; const Msg: string) of object;

  /// <summary>
  /// The SBW Broker - central message router
  /// </summary>
  TSBWBroker = class
  private
    FListener: TSBWConnectionListener;
    FAcceptThread: TSBWAcceptThread;
    FModules: TObjectDictionary<SBWModuleID, TSBWModuleInfo>;
    FClientThreads: TObjectList<TSBWClientThread>;
    FNextModuleID: SBWModuleID;
    FLock: TCriticalSection;
    FRunning: Boolean;
    FPort: Word;

    FModuleServiceHandler: TObject; //TSBWModuleServiceHandler;
    FSystemServiceHandler: TObject; //TSBWSystemServiceHandler;
    FRegistry: TSBWModuleRegistry;

    FOnModuleConnect: TSBWModuleConnectEvent;
    FOnModuleDisconnect: TSBWModuleDisconnectEvent;
    FOnLog: TSBWLogEvent;

    function AllocateModuleID: SBWModuleID;
    procedure HandleClientHandshake(Socket: TSBWSocket);
    procedure RouteMessage(const Msg: TSBWMessage; SourceModule: TSBWModuleInfo);
    procedure HandleSystemCall(const CallMsg: TSBWCallMessage; SourceModule: TSBWModuleInfo);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Start the broker on the specified port
    /// </summary>
    procedure Start(Port: Word = SBW_DEFAULT_PORT);

    /// <summary>
    /// Stop the broker
    /// </summary>
    procedure Stop;

    /// <summary>
    /// Log a message
    /// </summary>
    procedure Log(const Msg: string);

    /// <summary>
    /// Get module by ID
    /// </summary>
    function GetModule(ModuleID: SBWModuleID): TSBWModuleInfo;

    /// <summary>
    /// Get module by name
    /// </summary>
    function GetModuleByName(const ModuleName: string): TSBWModuleInfo;

    /// <summary>
    /// Get all connected modules
    /// </summary>
    function GetAllModules: TArray<TSBWModuleInfo>;

    /// <summary>
    /// Remove a module (called when it disconnects)
    /// </summary>
    procedure RemoveModule(ModuleID: SBWModuleID);

    /// <summary>
    /// Send a message to a specific module
    /// </summary>
    procedure SendToModule(ModuleID: SBWModuleID; const Data: TBytes);

    /// <summary>
    /// Get the broker version string
    /// </summary>
    class function GetVersion: string;

    /// <summary>
    /// Acquire lock for thread-safe access
    /// </summary>
    procedure Lock;

    /// <summary>
    /// Release lock
    /// </summary>
    procedure Unlock;

    property Running: Boolean read FRunning;
    property Port: Word read FPort;
    property Version: string read GetVersion;
    property Registry: TSBWModuleRegistry read FRegistry;
    property OnModuleConnect: TSBWModuleConnectEvent read FOnModuleConnect write FOnModuleConnect;
    property OnModuleDisconnect: TSBWModuleDisconnectEvent read FOnModuleDisconnect write FOnModuleDisconnect;
    property OnLog: TSBWLogEvent read FOnLog write FOnLog;
  end;

implementation

uses
  SBW.Broker.SystemService;

{ TSBWClientThread }

constructor TSBWClientThread.Create(ABroker: TSBWBroker; AClient: TSBWClientConnection;
  AModuleInfo: TSBWModuleInfo);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FBroker := ABroker;
  FClient := AClient;
  FModuleInfo := AModuleInfo;
end;

procedure TSBWClientThread.Execute;
var
  Msg: TSBWMessage;
begin
  FBroker.Log(Format('Client thread started for module %d (%s)',
    [FModuleInfo.ModuleID, FModuleInfo.ModuleName]));

  while not Terminated and FClient.Connected do
  begin
    try
      if FClient.Socket.DataAvailable then
      begin
        Msg := FClient.ReceiveMessage;
        try
          FBroker.RouteMessage(Msg, FModuleInfo);
        finally
          Msg.Free;
        end;
      end
      else
        Sleep(10);
    except
      on E: ESBWConnectionClosed do
      begin
        FBroker.Log(Format('Module %d disconnected', [FModuleInfo.ModuleID]));
        Break;
      end;
      on E: Exception do
      begin
        FBroker.Log(Format('Error in client thread for module %d: %s',
          [FModuleInfo.ModuleID, E.Message]));
        Break;
      end;
    end;
  end;

  FBroker.RemoveModule(FModuleInfo.ModuleID);
  FBroker.Log(Format('Client thread ended for module %d', [FModuleInfo.ModuleID]));
end;

{ TSBWAcceptThread }

constructor TSBWAcceptThread.Create(ABroker: TSBWBroker; AListener: TSBWConnectionListener);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FBroker := ABroker;
  FListener := AListener;
end;

procedure TSBWAcceptThread.Execute;
var
  ClientSocket: TSBWSocket;
begin
  FBroker.Log('Accept thread started');

  while not Terminated and FListener.Listening do
  begin
    try
      ClientSocket := FListener.Accept;
      FBroker.HandleClientHandshake(ClientSocket);
    except
      on E: ESBWConnectionError do
      begin
        if not Terminated then
          FBroker.Log('Accept error: ' + E.Message);
      end;
    end;
  end;

  FBroker.Log('Accept thread ended');
end;

{ TSBWBroker }

constructor TSBWBroker.Create;
begin
  inherited Create;
  FListener := TSBWConnectionListener.Create;
  FModules := TObjectDictionary<SBWModuleID, TSBWModuleInfo>.Create([]);
  FClientThreads := TObjectList<TSBWClientThread>.Create(True);
  FLock := TCriticalSection.Create;
  FRegistry := TSBWModuleRegistry.Create;
  FNextModuleID := 1; // 0 is reserved for broker

  // NEW: Enable persistence
  FRegistry.AutoSave := True;
  FRegistry.LoadIfExists;

  FRunning := False;

  // Create service handlers
  FModuleServiceHandler := TSBWModuleServiceHandler.Create(Self);
  FSystemServiceHandler := TSBWSystemServiceHandler.Create(Self);
end;


destructor TSBWBroker.Destroy;
begin
  Stop;
  FSystemServiceHandler.Free;
  FModuleServiceHandler.Free;

  // NEW: Save registry if there are unsaved changes
  if FRegistry.Dirty then
    FRegistry.SaveToFile;

  FRegistry.Free;
  FClientThreads.Free;
  FModules.Free;
  FListener.Free;
  FLock.Free;
  inherited;
end;

procedure TSBWBroker.Log(const Msg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Msg);
end;

procedure TSBWBroker.Lock;
begin
  FLock.Enter;
end;

procedure TSBWBroker.Unlock;
begin
  FLock.Leave;
end;

function TSBWBroker.AllocateModuleID: SBWModuleID;
begin
  FLock.Enter;
  try
    Result := FNextModuleID;
    Inc(FNextModuleID);
  finally
    FLock.Leave;
  end;
end;

procedure TSBWBroker.Start(Port: Word);
begin
  if FRunning then
    Exit;

  FPort := Port;
  FListener.Start(Port);

  FAcceptThread := TSBWAcceptThread.Create(Self, FListener);
  FAcceptThread.Start;

  FRunning := True;
  Log(Format('Broker started on port %d', [Port]));
end;

procedure TSBWBroker.Stop;
var
  ClientThreadsToWait: TList<TSBWClientThread>;
  CThread: TSBWClientThread;
begin
  if not FRunning then
    Exit;

  FRunning := False;
  FListener.Stop;

  if FAcceptThread <> nil then
  begin
    FAcceptThread.Terminate;
    FAcceptThread.WaitFor;
    FreeAndNil(FAcceptThread);
  end;

  ClientThreadsToWait := TList<TSBWClientThread>.Create;
  try
    FLock.Enter;
    try
      for CThread in FClientThreads do
      begin
        CThread.Terminate;
        ClientThreadsToWait.Add(CThread);
      end;
    finally
      FLock.Leave;
    end;

    for CThread in ClientThreadsToWait do
      CThread.WaitFor;

    FLock.Enter;
    try
      FClientThreads.Clear;
      FModules.Clear;
    finally
      FLock.Leave;
    end;
  finally
    ClientThreadsToWait.Free;
  end;

  Log('Broker stopped');
end;

procedure TSBWBroker.HandleClientHandshake(Socket: TSBWSocket);
var
  LengthBytes: TBytes;
  MsgLength: SBWInteger;
  HandshakeData: TBytes;
  Reader: TSBWDataBlockReader;
  ModuleName: string;
  ModuleID: SBWModuleID;
  ResponseLength: SBWInteger;
  ResponseData: TBytes;
  Client: TSBWClientConnection;
  ModuleInfo: TSBWModuleInfo;
  ClientThread: TSBWClientThread;
begin
  try
    LengthBytes := Socket.ReceiveBytes(4);
    Move(LengthBytes[0], MsgLength, 4);

    HandshakeData := Socket.ReceiveBytes(MsgLength - 4);

    Reader := TSBWDataBlockReader.Create(HandshakeData);
    try
      ModuleName := Reader.ReadString;
    finally
      Reader.Free;
    end;

    ModuleID := AllocateModuleID;

    ResponseLength := 8;
    SetLength(ResponseData, 8);
    Move(ResponseLength, ResponseData[0], 4);
    Move(ModuleID, ResponseData[4], 4);
    Socket.SendBytes(ResponseData);

    Client := TSBWClientConnection.Create(Socket, ModuleID);
    Client.ModuleName := ModuleName;

    ModuleInfo := TSBWModuleInfo.Create(ModuleID, ModuleName, Client);

    FLock.Enter;
    try
      FModules.Add(ModuleID, ModuleInfo);
    finally
      FLock.Leave;
    end;

    Log(Format('Module connected: %d (%s)', [ModuleID, ModuleName]));

    if Assigned(FOnModuleConnect) then
      FOnModuleConnect(Self, ModuleInfo);

    ClientThread := TSBWClientThread.Create(Self, Client, ModuleInfo);
    FLock.Enter;
    try
      FClientThreads.Add(ClientThread);
    finally
      FLock.Leave;
    end;
    ClientThread.Start;

  except
    on E: Exception do
    begin
      Log('Handshake failed: ' + E.Message);
      Socket.Free;
    end;
  end;
end;

procedure TSBWBroker.RouteMessage(const Msg: TSBWMessage; SourceModule: TSBWModuleInfo);
var
  DestModule: TSBWModuleInfo;
  MsgData: TBytes;
begin
  // Check if this is a call to the broker itself (module 0)
  if Msg.DestinationID = SBW_BROKER_MODULE_ID then
  begin
    if Msg is TSBWCallMessage then
      HandleSystemCall(TSBWCallMessage(Msg), SourceModule);
    Exit;
  end;

  Log(Format('Routing message from %d to %d (type=%d, uid=%d)',
    [SourceModule.ModuleID, Msg.DestinationID, Ord(Msg.MessageType), Msg.UID]));

  FLock.Enter;
  try
    if not FModules.TryGetValue(Msg.DestinationID, DestModule) then
    begin
      Log(Format('Destination module %d not found', [Msg.DestinationID]));

      if Msg.MessageType = mtCall then
      begin
        MsgData := TSBWMessageWriter.BuildErrorMessage(
          SourceModule.ModuleID,
          Msg.UID,
          ecModuleNotFound,
          Format('Module %d not found', [Msg.DestinationID]),
          '');
        SourceModule.Connection.SendMessage(MsgData);
      end;
      Exit;
    end;
  finally
    FLock.Leave;
  end;

  MsgData := Msg.ToBytes;
  try
    DestModule.Connection.SendMessage(MsgData);
  except
    on E: Exception do
    begin
      Log(Format('Failed to send to module %d: %s', [Msg.DestinationID, E.Message]));
      if Msg.MessageType = mtCall then
      begin
        MsgData := TSBWMessageWriter.BuildErrorMessage(
          SourceModule.ModuleID,
          Msg.UID,
          ecCommunication,
          'Failed to deliver message',
          E.Message);
        SourceModule.Connection.SendMessage(MsgData);
      end;
    end;
  end;
end;

procedure TSBWBroker.HandleSystemCall(const CallMsg: TSBWCallMessage;
  SourceModule: TSBWModuleInfo);
var
  ReplyData: TBytes;
begin
  Log(Format('System call from %d: service=%d method=%d',
    [SourceModule.ModuleID, CallMsg.ServiceID, CallMsg.MethodID]));

  case CallMsg.ServiceID of
    MODULE_SYSTEM_SERVICE_ID:  // Service -1: Module System Service
      TSBWModuleServiceHandler(FModuleServiceHandler).HandleCall(CallMsg, SourceModule);

    BROKER_SYSTEM_SERVICE_ID:  // Service 0: Broker's SYSTEM service
      TSBWSystemServiceHandler(FSystemServiceHandler).HandleCall(CallMsg, SourceModule);
  else
    ReplyData := TSBWMessageWriter.BuildErrorMessage(
      SourceModule.ModuleID, CallMsg.UID, ecServiceNotFound,
      Format('Broker service %d not found', [CallMsg.ServiceID]), '');
    SourceModule.Connection.SendMessage(ReplyData);
  end;
end;

function TSBWBroker.GetModule(ModuleID: SBWModuleID): TSBWModuleInfo;
begin
  FLock.Enter;
  try
    if not FModules.TryGetValue(ModuleID, Result) then
      Result := nil;
  finally
    FLock.Leave;
  end;
end;

class function TSBWBroker.GetVersion: string;
begin
  Result := SBW.Broker.VERSION;
end;

function TSBWBroker.GetModuleByName(const ModuleName: string): TSBWModuleInfo;
var
  Module: TSBWModuleInfo;
  LowerName: string;
begin
  LowerName := LowerCase(ModuleName);
  FLock.Enter;
  try
    for Module in FModules.Values do
      if LowerCase(Module.ModuleName) = LowerName then
        Exit(Module);
    Result := nil;
  finally
    FLock.Leave;
  end;
end;

function TSBWBroker.GetAllModules: TArray<TSBWModuleInfo>;
begin
  FLock.Enter;
  try
    Result := FModules.Values.ToArray;
  finally
    FLock.Leave;
  end;
end;

procedure TSBWBroker.RemoveModule(ModuleID: SBWModuleID);
begin
  FLock.Enter;
  try
    if FModules.ContainsKey(ModuleID) then
    begin
      FModules.Remove(ModuleID);
      Log(Format('Module %d removed', [ModuleID]));

      if Assigned(FOnModuleDisconnect) then
        FOnModuleDisconnect(Self, ModuleID);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TSBWBroker.SendToModule(ModuleID: SBWModuleID; const Data: TBytes);
var
  Module: TSBWModuleInfo;
begin
  Module := GetModule(ModuleID);
  if Module <> nil then
    Module.Connection.SendMessage(Data);
end;

end.
