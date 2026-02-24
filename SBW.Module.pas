unit SBW.Module;

{******************************************************************************
 * SBW.Module.pas
 *
 * Module implementation for SBW.
 *
 * TSBWModuleImpl is the main class for creating SBW modules. It handles:
 * - Connection to the broker
 * - Service registration
 * - Incoming call dispatch
 * - Outgoing calls to other modules
 *****************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Generics.Collections,
  SBW.Types, SBW.DataBlock, SBW.Message, SBW.Signature, SBW.Service, SBW.Connection,
  SBW.List;

type
  TSBWModuleImpl = class;

  /// <summary>
  /// Event fired when an incoming call is received
  /// </summary>
  TSBWIncomingCallEvent = procedure(Sender: TObject; FromModuleID: SBWModuleID;
    ServiceID: SBWServiceID; MethodID: SBWMethodID) of object;

  /// <summary>
  /// Proxy for a remote module instance
  /// </summary>
  TSBWRemoteModule = class
  private
    FOwner: TSBWModuleImpl;
    FModuleID: SBWModuleID;
  public
    constructor Create(AOwner: TSBWModuleImpl; AModuleID: SBWModuleID);

    /// <summary>
    /// Get a service proxy
    /// </summary>
    function GetService(const ServiceName: string): SBWServiceID;

    /// <summary>
    /// Shutdown this module instance
    /// </summary>
    procedure Shutdown;

    property ModuleID: SBWModuleID read FModuleID;
  end;

  /// <summary>
  /// Proxy for a remote service
  /// </summary>
  TSBWRemoteService = class
  private
    FOwner: TSBWModuleImpl;
    FModuleID: SBWModuleID;
    FServiceID: SBWServiceID;
  public
    constructor Create(AOwner: TSBWModuleImpl; AModuleID: SBWModuleID; AServiceID: SBWServiceID);

    /// <summary>
    /// Get a method by signature
    /// </summary>
    function GetMethod(const Signature: string): SBWMethodID;

    /// <summary>
    /// Call a method and return the result
    /// </summary>
    function Call(MethodID: SBWMethodID; Args: TSBWDataBlockWriter): TSBWDataBlockReader;

    /// <summary>
    /// Send a message (fire and forget)
    /// </summary>
    procedure Send(MethodID: SBWMethodID; Args: TSBWDataBlockWriter);

    property ModuleID: SBWModuleID read FModuleID;
    property ServiceID: SBWServiceID read FServiceID;
  end;

  /// <summary>
  /// Main module implementation class
  /// </summary>
  TSBWModuleImpl = class
  private
    FModuleName: string;
    FDisplayName: string;
    FManagementType: TSBWModuleManagementType;
    FHelp: string;
    FConnection: TSBWConnection;
    FServices: TSBWServiceRegistry;
    FRunning: Boolean;
    FLock: TCriticalSection;
    FOnIncomingCall: TSBWIncomingCallEvent;

    procedure HandleMessage(Sender: TObject; const Msg: TSBWMessage);
    procedure HandleCallMessage(const CallMsg: TSBWCallMessage);
    procedure HandleModuleSystemCall(const CallMsg: TSBWCallMessage);
    procedure HandleDisconnect(Sender: TObject);
    procedure RegisterServicesWithBroker;
  public
    constructor Create(const AModuleName, ADisplayName: string;
      AManagementType: TSBWModuleManagementType; const AHelp: string = '');
    destructor Destroy; override;

    /// <summary>
    /// Add a service to this module
    /// </summary>
    function AddService(const ServiceName, DisplayName, Category: string;
      const Help: string = ''): TSBWService;

    /// <summary>
    /// Connect to the broker and enable services.
    /// Use Bidirectional=True if this module needs to both serve AND call methods.
    /// </summary>
    procedure Connect(const Host: string = '127.0.0.1'; Port: Word = SBW_DEFAULT_PORT;
      Bidirectional: Boolean = False);

    /// <summary>
    /// Disconnect from the broker
    /// </summary>
    procedure Disconnect;

    /// <summary>
    /// Run the module's receive loop (blocks until shutdown).
    /// Only use this for server-only modules (Bidirectional=False).
    /// For bidirectional modules, the receive thread runs automatically.
    /// </summary>
    procedure Run;

    /// <summary>
    /// Signal the module to stop running
    /// </summary>
    procedure SignalDisconnect;

    /// <summary>
    /// Wait for disconnect to complete
    /// </summary>
    procedure WaitForDisconnect;

    /// <summary>
    /// Get an instance of another module
    /// </summary>
    function GetModuleInstance(const ModuleName: string): TSBWRemoteModule;

    /// <summary>
    /// Call a method on a remote module (low-level)
    /// </summary>
    function CallMethod(DestModuleID: SBWModuleID; ServiceID: SBWServiceID;
      MethodID: SBWMethodID; Args: TSBWDataBlockWriter;
      TimeoutMs: Cardinal = 30000): TSBWDataBlockReader;

    /// <summary>
    /// Send a message to a remote module (fire and forget)
    /// </summary>
    procedure SendMethod(DestModuleID: SBWModuleID; ServiceID: SBWServiceID;
      MethodID: SBWMethodID; Args: TSBWDataBlockWriter);

    /// <summary>
    /// Get this module's ID (assigned by broker)
    /// </summary>
    function GetModuleID: SBWModuleID;

    //=========================================================================
    // Broker Query Methods
    // These return TSBWList for language-agnostic data exchange
    //=========================================================================

    function GetVersion: string;

    /// <summary>
    /// Get list of all connected modules.
    /// Returns TSBWList where items alternate: [int moduleID, string moduleName, ...]
    /// Caller must free the returned list.
    /// </summary>
    function GetListOfModules: TSBWList;

    /// <summary>
    /// Get module ID by name. Returns SBW_NO_MODULE_ID (-1) if not found.
    /// </summary>
    function GetModuleIdByName(const ModuleName: string): SBWModuleID;

    /// <summary>
    /// Get list of services for a module by ID.
    /// Returns TSBWList where items alternate: [int serviceID, string serviceName, ...]
    /// Caller must free the returned list.
    /// </summary>
    function GetServiceIds(ModuleID: SBWModuleID): TSBWList; overload;

    /// <summary>
    /// Get list of services for a module by name.
    /// Returns TSBWList where items alternate: [int serviceID, string serviceName, ...]
    /// Caller must free the returned list.
    /// </summary>
    function GetServiceIds(const ModuleName: string): TSBWList; overload;

    /// <summary>
    /// Get list of methods for a service by IDs.
    /// Returns TSBWList where items alternate: [int methodID, string signature, ...]
    /// Caller must free the returned list.
    /// </summary>
    function GetMethodIds(ModuleID: SBWModuleID; ServiceID: SBWServiceID): TSBWList; overload;

    /// <summary>
    /// Get list of methods for a service by names.
    /// Returns TSBWList where items alternate: [int methodID, string signature, ...]
    /// Caller must free the returned list.
    /// </summary>
    function GetMethodIds(const ModuleName, ServiceName: string): TSBWList; overload;

    /// <summary>
    /// Get all module descriptors.
    /// Returns array of module descriptors. Each descriptor is a TSBWList:
    /// [moduleName, displayName, managementType, commandLine, help]
    /// Caller must free each list in the returned array.
    /// </summary>
    /// <param name="LocalOnly">If true, only search for local modules</param>
    /// <param name="IncludeRunning">If true, include currently running modules</param>
    function GetModuleDescriptors(LocalOnly, IncludeRunning: Boolean): TArray<TSBWList>;

    /// <summary>
    /// Get module descriptor by name.
    /// Returns TSBWList representing the descriptor:
    /// [moduleName, displayName, managementType, commandLine, help]
    /// Returns empty list if not found. Caller must free the returned list.
    /// </summary>
    /// <param name="ModuleName">Name of the module to find</param>
    /// <param name="IncludeRunning">If true, include running modules in search</param>
    function GetModuleDescriptor(const ModuleName: string; IncludeRunning: Boolean): TSBWList; overload;

    /// <summary>
    /// Get module descriptor by ID.
    /// Returns TSBWList representing the descriptor:
    /// [moduleName, displayName, managementType, commandLine, help]
    /// Returns empty list if not found. Caller must free the returned list.
    /// </summary>
    /// <param name="ModuleID">ID of the module</param>
    function GetModuleDescriptor(ModuleID: SBWModuleID): TSBWList; overload;

    /// <summary>
    /// Get help string for a method by IDs.
    /// Calls the target module's Module System Service (service -1).
    /// </summary>
    function GetMethodHelp(ModuleID: SBWModuleID; ServiceID: SBWServiceID;
      MethodID: SBWMethodID): string;

    /// <summary>
    /// Get help string for a method by names.
    /// Returns empty string if module, service, or method not found.
    /// </summary>
    function GetMethodHelpByName(const ModuleName, ServiceName, MethodName: string): string;

    /// <summary>
    /// Find a service ID by name within a module.
    /// Returns -1 if not found.
    /// </summary>
    function FindServiceId(ModuleID: SBWModuleID; const ServiceName: string): SBWServiceID;

    /// <summary>
    /// Find a method ID by name within a service.
    /// Searches method signatures for the given name.
    /// Returns -1 if not found.
    /// </summary>
    function FindMethodId(ModuleID: SBWModuleID; ServiceID: SBWServiceID;
      const MethodName: string): SBWMethodID;

    //=========================================================================
    // Additional Broker Methods (10-20)
    //=========================================================================

    /// <summary>
    /// Get or start a module instance by name.
    /// If the module is running, returns its ID. If registered but not running,
    /// launches it and waits for connection. Returns SBW_NO_MODULE_ID (-1) if not found.
    /// </summary>
    function GetModuleInstanceByName(const ModuleName: string): SBWModuleID;

    /// <summary>
    /// Find services matching a category.
    /// Returns array of service descriptors. Each descriptor is a TSBWList:
    /// [moduleName, serviceName, displayName, category, help]
    /// Caller must free each list in the returned array.
    /// </summary>
    /// <param name="Category">Category to search (e.g., "Analysis" or "Analysis/Simulation")</param>
    /// <param name="Recursive">If true, include services in subcategories</param>
    function FindServices(const Category: string; Recursive: Boolean): TArray<TSBWList>;

    /// <summary>
    /// Get subcategories of a category.
    /// Returns array of subcategory names (immediate children only).
    /// </summary>
    /// <param name="Category">Parent category (empty string for root categories)</param>
    function GetServiceCategories(const Category: string): TArray<string>;

    /// <summary>
    /// Release/disconnect a module from the broker.
    /// </summary>
    /// <param name="ModuleID">ID of the module to release</param>
    procedure ReleaseModule(ModuleID: SBWModuleID);

    /// <summary>
    /// Get service descriptor by module ID and service name.
    /// Returns TSBWList: [moduleName, serviceName, displayName, category, help]
    /// Returns empty list if not found. Caller must free the returned list.
    /// </summary>
    function GetServiceDescriptor(ModuleID: SBWModuleID; const ServiceName: string): TSBWList; overload;

    /// <summary>
    /// Get service descriptor by module ID and service ID.
    /// Returns TSBWList: [moduleName, serviceName, displayName, category, help]
    /// Returns empty list if not found. Caller must free the returned list.
    /// </summary>
    function GetServiceDescriptor(ModuleID: SBWModuleID; ServiceID: SBWServiceID): TSBWList; overload;

    /// <summary>
    /// Get all service descriptors for a module.
    /// Returns array of service descriptors. Each descriptor is a TSBWList:
    /// [moduleName, serviceName, displayName, category, help]
    /// Caller must free each list in the returned array.
    /// </summary>
    function GetServiceDescriptors(ModuleID: SBWModuleID): TArray<TSBWList>;

    /// <summary>
    /// Get IDs of all currently connected module instances.
    /// </summary>
    function GetExistingModuleInstanceIds: TArray<SBWModuleID>;

    /// <summary>
    /// Unregister a module from the broker's registry.
    /// </summary>
    /// <param name="ModuleName">Name of the module to unregister</param>
    procedure UnregisterModule(const ModuleName: string);

    /// <summary>
    /// Register a module with the broker's registry for auto-launch.
    /// </summary>
    /// <param name="Name">Unique module name</param>
    /// <param name="DisplayName">Human-readable name</param>
    /// <param name="ModuleType">Management type (0=unique, 1=self-managed)</param>
    /// <param name="CommandLine">Command to launch the module</param>
    /// <param name="Help">Documentation string</param>
    procedure RegisterModule(const Name, DisplayName: string; ModuleType: Integer;
      const CommandLine, Help: string);


    /// <summary>
    /// Shutdown the broker. Use with caution!
    /// </summary>
    procedure ShutdownBroker;

    /// <summary>
    /// Get all service descriptors for a module by name.
    /// Returns array of service descriptors. Each descriptor is a TSBWList:
    /// [moduleName, serviceName, displayName, category, help]
    /// Caller must free each list in the returned array.
    /// </summary>
    function GetServiceDescriptorsByName(const ModuleName: string): TArray<TSBWList>;

    /// <summary>
    /// Register a service in the broker's registry.
    /// </summary>
    /// <param name="ModuleName">Name of the module that provides this service</param>
    /// <param name="ServiceName">Unique service name within the module</param>
    /// <param name="DisplayName">Human-readable name</param>
    /// <param name="Category">Service category (e.g., "Analysis/Simulation")</param>
    /// <param name="Help">Documentation string</param>
    procedure RegisterService(const ModuleName, ServiceName, DisplayName,
      Category, Help: string);

    property ModuleName: string read FModuleName;
    property DisplayName: string read FDisplayName;
    property ManagementType: TSBWModuleManagementType read FManagementType;
    property Help: string read FHelp;
    property Services: TSBWServiceRegistry read FServices;
    property Running: Boolean read FRunning;
    property Connection: TSBWConnection read FConnection;
    property OnIncomingCall: TSBWIncomingCallEvent read FOnIncomingCall write FOnIncomingCall;
  end;

implementation

const
  MODULE_SYSTEM_SERVICE_ID = -1;

{ TSBWRemoteModule }

constructor TSBWRemoteModule.Create(AOwner: TSBWModuleImpl; AModuleID: SBWModuleID);
begin
  inherited Create;
  FOwner := AOwner;
  FModuleID := AModuleID;
end;

function TSBWRemoteModule.GetService(const ServiceName: string): SBWServiceID;
begin
  // TODO: Query broker for service ID
  // For now, return 0 (first service)
  Result := 0;
end;

procedure TSBWRemoteModule.Shutdown;
begin
  // TODO: Send shutdown message
end;

{ TSBWRemoteService }

constructor TSBWRemoteService.Create(AOwner: TSBWModuleImpl; AModuleID: SBWModuleID;
  AServiceID: SBWServiceID);
begin
  inherited Create;
  FOwner := AOwner;
  FModuleID := AModuleID;
  FServiceID := AServiceID;
end;

function TSBWRemoteService.GetMethod(const Signature: string): SBWMethodID;
begin
  // TODO: Query broker for method ID
  // For now, return 0 (first method)
  Result := 0;
end;

function TSBWRemoteService.Call(MethodID: SBWMethodID; Args: TSBWDataBlockWriter): TSBWDataBlockReader;
begin
  Result := FOwner.CallMethod(FModuleID, FServiceID, MethodID, Args);
end;

procedure TSBWRemoteService.Send(MethodID: SBWMethodID; Args: TSBWDataBlockWriter);
begin
  FOwner.SendMethod(FModuleID, FServiceID, MethodID, Args);
end;

{ TSBWModuleImpl }

constructor TSBWModuleImpl.Create(const AModuleName, ADisplayName: string;
  AManagementType: TSBWModuleManagementType; const AHelp: string);
begin
  inherited Create;
  FModuleName := AModuleName;
  FDisplayName := ADisplayName;
  FManagementType := AManagementType;
  FHelp := AHelp;
  FServices := TSBWServiceRegistry.Create;
  FConnection := TSBWConnection.Create;
  FLock := TCriticalSection.Create;
  FRunning := False;
end;

destructor TSBWModuleImpl.Destroy;
begin
  Disconnect;
  FLock.Free;
  FConnection.Free;
  FServices.Free;
  inherited;
end;

function TSBWModuleImpl.AddService(const ServiceName, DisplayName, Category: string;
  const Help: string): TSBWService;
begin
  Result := FServices.AddService(ServiceName, DisplayName, Category, Help);
end;

procedure TSBWModuleImpl.Connect(const Host: string; Port: Word; Bidirectional: Boolean);
begin
  FConnection.OnMessage := HandleMessage;
  FConnection.OnDisconnect := HandleDisconnect;
  FConnection.Connect(Host, Port, FModuleName, Bidirectional);
  FRunning := True;
  
  // Register our services with the broker
  RegisterServicesWithBroker;
end;

procedure TSBWModuleImpl.RegisterServicesWithBroker;
const
  SYS_METHOD_REGISTER_SERVICES = 100;
var
  Writer: TSBWDataBlockWriter;
  Service: TSBWService;
  Method: TSBWServiceMethod;
  ArgBytes, ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
begin
  // Build registration payload:
  //   serviceCount: int
  //   For each service:
  //     serviceID: int
  //     name: string
  //     displayName: string
  //     category: string
  //     help: string
  //     methodCount: int
  //     For each method:
  //       methodID: int
  //       name: string
  //       signature: string
  //       help: string

  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteInteger(FServices.Services.Count);
    
    for Service in FServices.Services do
    begin
      Writer.WriteInteger(Service.ServiceID);
      Writer.WriteString(Service.Name);
      Writer.WriteString(Service.DisplayName);
      Writer.WriteString(Service.Category);
      Writer.WriteString(Service.Help);
      Writer.WriteInteger(Service.Methods.Count);
      
      for Method in Service.Methods do
      begin
        Writer.WriteInteger(Method.MethodID);
        Writer.WriteString(Method.Name);
        Writer.WriteString(Method.SignatureString);
        Writer.WriteString(Method.Help);
      end;
    end;
    
    ArgBytes := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  // Call broker's register services method (module 0, service 0, method 100)
  ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_REGISTER_SERVICES, 
    ArgBytes, 30000);
  
  // Read success flag (we don't really need to check it, but consume the response)
  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    Reader.ReadBoolean;
  finally
    Reader.Free;
  end;
end;

procedure TSBWModuleImpl.Disconnect;
begin
  FRunning := False;
  FConnection.Disconnect;
end;

procedure TSBWModuleImpl.Run;
var
  Msg: TSBWMessage;
begin
  // Only use for non-bidirectional modules
  // For bidirectional modules, the connection's receive thread handles everything
  if FConnection.ThreadedMode then
  begin
    // In threaded mode, just wait until we're told to stop
    while FRunning and FConnection.Connected do
      Sleep(100);
  end
  else
  begin
    // Non-threaded mode: run receive loop in current thread
    while FRunning and FConnection.Connected do
    begin
      try
        if FConnection.Socket.DataAvailable then
        begin
          Msg := FConnection.ReceiveMessage;
          try
            HandleMessage(Self, Msg);
          finally
            Msg.Free;
          end;
        end
        else
          Sleep(10);
      except
        on E: ESBWConnectionClosed do
        begin
          FRunning := False;
          Break;
        end;
        on E: Exception do
        begin
          // Log error but continue
        end;
      end;
    end;
  end;
end;

procedure TSBWModuleImpl.SignalDisconnect;
begin
  FRunning := False;
end;

procedure TSBWModuleImpl.WaitForDisconnect;
begin
  while FConnection.Connected do
    Sleep(100);
end;

procedure TSBWModuleImpl.HandleMessage(Sender: TObject; const Msg: TSBWMessage);
begin
  if Msg is TSBWCallMessage then
    HandleCallMessage(TSBWCallMessage(Msg));
  // Reply and Error messages are handled by the Connection's pending call mechanism
end;


procedure TSBWModuleImpl.HandleModuleSystemCall(const CallMsg: TSBWCallMessage);
var
  Reader: TSBWDataBlockReader;
  Writer: TSBWDataBlockWriter;
  ResultData, ReplyData: TBytes;
  ServiceID: SBWServiceID;
  MethodID: SBWMethodID;
  Service: TSBWService;
  Method: TSBWServiceMethod;
  I: Integer;
begin
  try
    Reader := TSBWDataBlockReader.Create(CallMsg.Payload);
    try
      Writer := TSBWDataBlockWriter.Create;
      try
        case CallMsg.MethodID of
          0: // getServices() -> string[]
            begin
              var Names: TArray<string>;
              SetLength(Names, FServices.Services.Count);
              for I := 0 to FServices.Services.Count - 1 do
                Names[I] := FServices.Services[I].Name;
              Writer.WriteStringArray(Names);
            end;

          1: // getMethods(int serviceId) -> string[]
            begin
              ServiceID := Reader.ReadInteger;
              Service := FServices.FindServiceByID(ServiceID);
              if Service <> nil then
                Writer.WriteStringArray(Service.GetMethodSignatures)
              else
                Writer.WriteStringArray([]);
            end;

          2, 5, 6: // notification methods - just acknowledge
            begin
              // Empty result
            end;

          3: // shutdown() -> void
            begin
              FRunning := False;
            end;

          4: // getMethodHelp(int serviceId, int methodId) -> string
            begin
              ServiceID := Reader.ReadInteger;
              MethodID := Reader.ReadInteger;
              Service := FServices.FindServiceByID(ServiceID);
              if Service <> nil then
              begin
                Method := Service.FindMethodByID(MethodID);
                if Method <> nil then
                  Writer.WriteString(Method.Help)
                else
                  Writer.WriteString('');
              end
              else
                Writer.WriteString('');
            end;
        else
          begin
            ReplyData := TSBWMessageWriter.BuildErrorMessage(
              CallMsg.SourceID, CallMsg.UID, ecMethodNotFound,
              Format('Module system method %d not found', [CallMsg.MethodID]), '');
            FConnection.SendRaw(ReplyData);
            Exit;
          end;
        end;

        ResultData := Writer.ToBytes;
      finally
        Writer.Free;
      end;
    finally
      Reader.Free;
    end;

    ReplyData := TSBWMessageWriter.BuildReplyMessage(
      CallMsg.SourceID, CallMsg.UID, ResultData);
    FConnection.SendRaw(ReplyData);

  except
    on E: Exception do
    begin
      ReplyData := TSBWMessageWriter.BuildErrorMessage(
        CallMsg.SourceID, CallMsg.UID, ecApplication,
        E.Message, E.ClassName);
      FConnection.SendRaw(ReplyData);
    end;
  end;
end;


procedure TSBWModuleImpl.HandleCallMessage(const CallMsg: TSBWCallMessage);
var
  Service: TSBWService;
  Method: TSBWServiceMethod;
  ArgsReader: TSBWDataBlockReader;
  ResultWriter: TSBWDataBlockWriter;
  ResultData, ReplyData: TBytes;
begin
  try
    // Handle Module System Service (service -1) first
    if CallMsg.ServiceID = -1 then
    begin
      HandleModuleSystemCall(CallMsg);
      Exit;
    end;

    // Find user service
    Service := FServices.FindServiceByID(CallMsg.ServiceID);
    if Service = nil then
    begin
      ReplyData := TSBWMessageWriter.BuildErrorMessage(
        CallMsg.SourceID, CallMsg.UID, ecServiceNotFound,
        Format('Service %d not found', [CallMsg.ServiceID]), '');
      FConnection.SendRaw(ReplyData);
      Exit;
    end;

    // Find method
    Method := Service.FindMethodByID(CallMsg.MethodID);
    if Method = nil then
    begin
      ReplyData := TSBWMessageWriter.BuildErrorMessage(
        CallMsg.SourceID, CallMsg.UID, ecMethodNotFound,
        Format('Method %d not found', [CallMsg.MethodID]), '');
      FConnection.SendRaw(ReplyData);
      Exit;
    end;

    // Invoke method
    ArgsReader := TSBWDataBlockReader.Create(CallMsg.Payload);
    try
      ResultWriter := Method.Invoke(CallMsg.SourceID, ArgsReader);
      try
        ResultData := ResultWriter.ToBytes;
      finally
        ResultWriter.Free;
      end;
    finally
      ArgsReader.Free;
    end;

    // Send reply
    ReplyData := TSBWMessageWriter.BuildReplyMessage(
      CallMsg.SourceID, CallMsg.UID, ResultData);
    FConnection.SendRaw(ReplyData);

  except
    on E: Exception do
    begin
      // Always send an error reply so caller doesn't hang
      ReplyData := TSBWMessageWriter.BuildErrorMessage(
        CallMsg.SourceID, CallMsg.UID, ecApplication,
        E.Message, E.ClassName);
      FConnection.SendRaw(ReplyData);
    end;
  end;
end;


procedure TSBWModuleImpl.HandleDisconnect(Sender: TObject);
begin
  FRunning := False;
end;

function TSBWModuleImpl.GetModuleInstance(const ModuleName: string): TSBWRemoteModule;
begin
  // TODO: Query broker for module instance
  // For now, create a placeholder
  Result := TSBWRemoteModule.Create(Self, 0);
end;

function TSBWModuleImpl.CallMethod(DestModuleID: SBWModuleID; ServiceID: SBWServiceID;
  MethodID: SBWMethodID; Args: TSBWDataBlockWriter; TimeoutMs: Cardinal): TSBWDataBlockReader;
var
  ArgBytes, ResultBytes: TBytes;
begin
  if Args <> nil then
    ArgBytes := Args.ToBytes
  else
    ArgBytes := [];

  ResultBytes := FConnection.Call(DestModuleID, ServiceID, MethodID, ArgBytes, TimeoutMs);
  Result := TSBWDataBlockReader.Create(ResultBytes);
end;

procedure TSBWModuleImpl.SendMethod(DestModuleID: SBWModuleID; ServiceID: SBWServiceID;
  MethodID: SBWMethodID; Args: TSBWDataBlockWriter);
var
  ArgBytes: TBytes;
  MsgData: TBytes;
begin
  if Args <> nil then
    ArgBytes := Args.ToBytes
  else
    ArgBytes := [];

  MsgData := TSBWMessageWriter.BuildCallMessage(
    DestModuleID,
    GetModuleID,
    TSBWUIDGenerator.NextUID,
    ServiceID,
    MethodID,
    ArgBytes,
    True); // IsSend = True

  FConnection.SendRaw(MsgData);
end;

function TSBWModuleImpl.GetModuleID: SBWModuleID;
begin
  Result := FConnection.ModuleID;
end;

const
  // Module System Service (service -1) method IDs
  MODSYS_GET_METHOD_HELP = 4;

function TSBWModuleImpl.GetMethodHelp(ModuleID: SBWModuleID; ServiceID: SBWServiceID;
  MethodID: SBWMethodID): string;
var
  Args: TSBWDataBlockWriter;
  ResultReader: TSBWDataBlockReader;
  ResultBytes: TBytes;
begin
  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteInteger(ServiceID);
    Args.WriteInteger(MethodID);

    // Call Module System Service (service -1), method 4 (getMethodHelp)
    ResultBytes := FConnection.Call(ModuleID, MODULE_SYSTEM_SERVICE_ID, MODSYS_GET_METHOD_HELP,
      Args.ToBytes, 30000);
  finally
    Args.Free;
  end;

  ResultReader := TSBWDataBlockReader.Create(ResultBytes);
  try
    Result := ResultReader.ReadString;
  finally
    ResultReader.Free;
  end;
end;

function TSBWModuleImpl.FindServiceId(ModuleID: SBWModuleID;
  const ServiceName: string): SBWServiceID;
var
  ServiceList, InnerList: TSBWList;
  I: Integer;
begin
  Result := -1;
  ServiceList := GetServiceIds(ModuleID);
  try
    for I := 0 to ServiceList.Count - 1 do
    begin
      InnerList := ServiceList[I].GetList;
      if SameText(InnerList[1].GetString, ServiceName) then
      begin
        Result := InnerList[0].GetInteger;
        Break;
      end;
    end;
  finally
    ServiceList.Free;
  end;
end;

function TSBWModuleImpl.FindMethodId(ModuleID: SBWModuleID; ServiceID: SBWServiceID;
  const MethodName: string): SBWMethodID;
var
  MethodList, InnerList: TSBWList;
  Signature, LowerName: string;
  I: Integer;
begin
  Result := -1;
  LowerName := LowerCase(MethodName);
  MethodList := GetMethodIds(ModuleID, ServiceID);
  try
    for I := 0 to MethodList.Count - 1 do
    begin
      InnerList := MethodList[I].GetList;
      Signature := LowerCase(InnerList[1].GetString);
      // Check if method name appears before the '(' in signature
      // e.g., "double add(double, double)" contains "add("
      if Pos(LowerName + '(', Signature) > 0 then
      begin
        Result := InnerList[0].GetInteger;
        Break;
      end;
    end;
  finally
    MethodList.Free;
  end;
end;

function TSBWModuleImpl.GetMethodHelpByName(const ModuleName, ServiceName,
  MethodName: string): string;
var
  ModuleID: SBWModuleID;
  ServiceID: SBWServiceID;
  MethodID: SBWMethodID;
begin
  Result := '';

  // Find module
  ModuleID := GetModuleIdByName(ModuleName);
  if ModuleID = SBW_NO_MODULE_ID then
    Exit;

  // Find service
  ServiceID := FindServiceId(ModuleID, ServiceName);
  if ServiceID < 0 then
    Exit;

  // Find method
  MethodID := FindMethodId(ModuleID, ServiceID, MethodName);
  if MethodID < 0 then
    Exit;

  // Get help
  Result := GetMethodHelp(ModuleID, ServiceID, MethodID);
end;

//=============================================================================
// Broker Query Method Implementations
//=============================================================================

const
  SYS_METHOD_GET_LIST_OF_MODULES     = 0;
  SYS_METHOD_GET_SERVICE_IDS_BY_ID   = 1;
  SYS_METHOD_GET_SERVICE_IDS_BY_NAME = 2;
  SYS_METHOD_GET_METHOD_IDS_BY_ID    = 3;
  SYS_METHOD_GET_METHOD_IDS_BY_NAME  = 4;
  SYS_METHOD_GET_MODULE_ID           = 5;
  SYS_METHOD_GET_VERSION             = 6;
  SYS_METHOD_GET_MODULE_DESCRIPTORS  = 7;
  SYS_METHOD_GET_MODULE_DESC_BY_NAME = 8;
  SYS_METHOD_GET_MODULE_DESC_BY_ID   = 9;
  SYS_METHOD_GET_MODULE_INSTANCE     = 10;
  SYS_METHOD_FIND_SERVICES           = 11;
  SYS_METHOD_GET_SERVICE_CATEGORIES  = 12;
  SYS_METHOD_RELEASE                 = 13;
  SYS_METHOD_GET_SERVICE_DESC_BY_NAME = 14;
  SYS_METHOD_GET_SERVICE_DESC_BY_ID  = 15;
  SYS_METHOD_GET_SERVICE_DESCRIPTORS = 16;
  SYS_METHOD_GET_EXISTING_MODULE_IDS = 17;
  SYS_METHOD_UNREGISTER_MODULE       = 18;
  SYS_METHOD_REGISTER_MODULE         = 19;
  SYS_METHOD_REGISTER_SERVICE        = 20;
  SYS_METHOD_SHUTDOWN_BROKER            = 21;
  SYS_METHOD_GET_SERVICE_DESCS_BY_NAME  = 22;


function TSBWModuleImpl.GetListOfModules: TSBWList;
var
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
begin
  // Call broker method 0 (no args)
  ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_GET_LIST_OF_MODULES, [], 30000);
  
  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    Result := TSBWList.ReadFrom(Reader);
  finally
    Reader.Free;
  end;
end;

function TSBWModuleImpl.GetModuleIdByName(const ModuleName: string): SBWModuleID;
var
  Writer: TSBWDataBlockWriter;
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteString(ModuleName);
    ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_GET_MODULE_ID, 
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;
  
  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    Result := Reader.ReadInteger;
  finally
    Reader.Free;
  end;
end;

function TSBWModuleImpl.GetServiceIds(ModuleID: SBWModuleID): TSBWList;
var
  Writer: TSBWDataBlockWriter;
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteInteger(ModuleID);
    ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_GET_SERVICE_IDS_BY_ID, 
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;
  
  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    Result := TSBWList.ReadFrom(Reader);
  finally
    Reader.Free;
  end;
end;

function TSBWModuleImpl.GetServiceIds(const ModuleName: string): TSBWList;
var
  Writer: TSBWDataBlockWriter;
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteString(ModuleName);
    ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_GET_SERVICE_IDS_BY_NAME, 
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;
  
  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    Result := TSBWList.ReadFrom(Reader);
  finally
    Reader.Free;
  end;
end;

function TSBWModuleImpl.GetMethodIds(ModuleID: SBWModuleID; ServiceID: SBWServiceID): TSBWList;
var
  Writer: TSBWDataBlockWriter;
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteInteger(ModuleID);
    Writer.WriteInteger(ServiceID);
    ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_GET_METHOD_IDS_BY_ID, 
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;
  
  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    Result := TSBWList.ReadFrom(Reader);
  finally
    Reader.Free;
  end;
end;

function TSBWModuleImpl.GetMethodIds(const ModuleName, ServiceName: string): TSBWList;
var
  Writer: TSBWDataBlockWriter;
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteString(ModuleName);
    Writer.WriteString(ServiceName);
    ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_GET_METHOD_IDS_BY_NAME, 
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;
  
  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    Result := TSBWList.ReadFrom(Reader);
  finally
    Reader.Free;
  end;
end;


function TSBWModuleImpl.GetVersion: string;
var
  Writer: TSBWDataBlockWriter;
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_GET_VERSION, Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    Result := Reader.ReadString;
  finally
    Reader.Free;
  end;
end;

function TSBWModuleImpl.GetModuleDescriptors(LocalOnly, IncludeRunning: Boolean): TArray<TSBWList>;
var
  Writer: TSBWDataBlockWriter;
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
  TypeByte, ElementTypeByte: Byte;
  Dims, Size, I: Int32;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteBoolean(LocalOnly);
    Writer.WriteBoolean(IncludeRunning);
    ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_GET_MODULE_DESCRIPTORS,
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    // Read array header
    Reader.Stream.ReadBuffer(TypeByte, 1);
    if TSBWDataBlockType(TypeByte) <> dbtArray then
      raise ESBWDataBlockError.Create('Expected array of lists');
    
    Reader.Stream.ReadBuffer(ElementTypeByte, 1);
    if TSBWDataBlockType(ElementTypeByte) <> dbtList then
      raise ESBWDataBlockError.Create('Expected array of lists');
    
    Reader.Stream.ReadBuffer(Dims, SizeOf(Int32));
    if Dims <> 1 then
      raise ESBWDataBlockError.Create('Expected 1D array');
    
    Reader.Stream.ReadBuffer(Size, SizeOf(Int32));
    
    // Read each list
    SetLength(Result, Size);
    for I := 0 to Size - 1 do
      Result[I] := TSBWList.ReadFrom(Reader);
  finally
    Reader.Free;
  end;
end;

function TSBWModuleImpl.GetModuleDescriptor(const ModuleName: string; 
  IncludeRunning: Boolean): TSBWList;
var
  Writer: TSBWDataBlockWriter;
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteString(ModuleName);
    Writer.WriteBoolean(IncludeRunning);
    ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_GET_MODULE_DESC_BY_NAME,
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    Result := TSBWList.ReadFrom(Reader);
  finally
    Reader.Free;
  end;
end;

function TSBWModuleImpl.GetModuleDescriptor(ModuleID: SBWModuleID): TSBWList;
var
  Writer: TSBWDataBlockWriter;
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteInteger(ModuleID);
    ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_GET_MODULE_DESC_BY_ID,
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    Result := TSBWList.ReadFrom(Reader);
  finally
    Reader.Free;
  end;
end;

//=============================================================================
// Additional Broker Method Implementations (10-20)
//=============================================================================

function TSBWModuleImpl.GetModuleInstanceByName(const ModuleName: string): SBWModuleID;
var
  Writer: TSBWDataBlockWriter;
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteString(ModuleName);
    ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_GET_MODULE_INSTANCE,
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    Result := Reader.ReadInteger;
  finally
    Reader.Free;
  end;
end;

function TSBWModuleImpl.FindServices(const Category: string; Recursive: Boolean): TArray<TSBWList>;
var
  Writer: TSBWDataBlockWriter;
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
  TypeByte, ElementTypeByte: Byte;
  Dims, Size, I: Int32;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteString(Category);
    Writer.WriteBoolean(Recursive);
    ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_FIND_SERVICES,
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    // Read array header
    Reader.Stream.ReadBuffer(TypeByte, 1);
    if TSBWDataBlockType(TypeByte) <> dbtArray then
      raise ESBWDataBlockError.Create('Expected array of lists');

    Reader.Stream.ReadBuffer(ElementTypeByte, 1);
    if TSBWDataBlockType(ElementTypeByte) <> dbtList then
      raise ESBWDataBlockError.Create('Expected array of lists');

    Reader.Stream.ReadBuffer(Dims, SizeOf(Int32));
    if Dims <> 1 then
      raise ESBWDataBlockError.Create('Expected 1D array');

    Reader.Stream.ReadBuffer(Size, SizeOf(Int32));

    // Read each list
    SetLength(Result, Size);
    for I := 0 to Size - 1 do
      Result[I] := TSBWList.ReadFrom(Reader);
  finally
    Reader.Free;
  end;
end;

function TSBWModuleImpl.GetServiceCategories(const Category: string): TArray<string>;
var
  Writer: TSBWDataBlockWriter;
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteString(Category);
    ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_GET_SERVICE_CATEGORIES,
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    Result := Reader.ReadStringArray;
  finally
    Reader.Free;
  end;
end;

procedure TSBWModuleImpl.ReleaseModule(ModuleID: SBWModuleID);
var
  Writer: TSBWDataBlockWriter;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteInteger(ModuleID);
    FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_RELEASE,
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;
end;

function TSBWModuleImpl.GetServiceDescriptor(ModuleID: SBWModuleID;
  const ServiceName: string): TSBWList;
var
  Writer: TSBWDataBlockWriter;
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteInteger(ModuleID);
    Writer.WriteString(ServiceName);
    ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_GET_SERVICE_DESC_BY_NAME,
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    Result := TSBWList.ReadFrom(Reader);
  finally
    Reader.Free;
  end;
end;

function TSBWModuleImpl.GetServiceDescriptor(ModuleID: SBWModuleID;
  ServiceID: SBWServiceID): TSBWList;
var
  Writer: TSBWDataBlockWriter;
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteInteger(ModuleID);
    Writer.WriteInteger(ServiceID);
    ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_GET_SERVICE_DESC_BY_ID,
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    Result := TSBWList.ReadFrom(Reader);
  finally
    Reader.Free;
  end;
end;

function TSBWModuleImpl.GetServiceDescriptors(ModuleID: SBWModuleID): TArray<TSBWList>;
var
  Writer: TSBWDataBlockWriter;
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
  TypeByte, ElementTypeByte: Byte;
  Dims, Size, I: Int32;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteInteger(ModuleID);
    ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_GET_SERVICE_DESCRIPTORS,
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    // Read array header
    Reader.Stream.ReadBuffer(TypeByte, 1);
    if TSBWDataBlockType(TypeByte) <> dbtArray then
      raise ESBWDataBlockError.Create('Expected array of lists');

    Reader.Stream.ReadBuffer(ElementTypeByte, 1);
    if TSBWDataBlockType(ElementTypeByte) <> dbtList then
      raise ESBWDataBlockError.Create('Expected array of lists');

    Reader.Stream.ReadBuffer(Dims, SizeOf(Int32));
    if Dims <> 1 then
      raise ESBWDataBlockError.Create('Expected 1D array');

    Reader.Stream.ReadBuffer(Size, SizeOf(Int32));

    // Read each list
    SetLength(Result, Size);
    for I := 0 to Size - 1 do
      Result[I] := TSBWList.ReadFrom(Reader);
  finally
    Reader.Free;
  end;
end;

function TSBWModuleImpl.GetExistingModuleInstanceIds: TArray<SBWModuleID>;
var
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
begin
  ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_GET_EXISTING_MODULE_IDS,
    [], 30000);

  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    Result := Reader.ReadIntegerArray;
  finally
    Reader.Free;
  end;
end;

procedure TSBWModuleImpl.UnregisterModule(const ModuleName: string);
var
  Writer: TSBWDataBlockWriter;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteString(ModuleName);
    FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_UNREGISTER_MODULE,
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;
end;

procedure TSBWModuleImpl.RegisterModule(const Name, DisplayName: string;
  ModuleType: Integer; const CommandLine, Help: string);
var
  Writer: TSBWDataBlockWriter;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteString(Name);
    Writer.WriteString(DisplayName);
    Writer.WriteInteger(ModuleType);
    Writer.WriteString(CommandLine);
    Writer.WriteString(Help);
    FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_REGISTER_MODULE,
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;
end;

procedure TSBWModuleImpl.RegisterService(const ModuleName, ServiceName,
  DisplayName, Category, Help: string);
var
  Writer: TSBWDataBlockWriter;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteString(ModuleName);
    Writer.WriteString(ServiceName);
    Writer.WriteString(DisplayName);
    Writer.WriteString(Category);
    Writer.WriteString(Help);
    FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_REGISTER_SERVICE,
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;
end;


procedure TSBWModuleImpl.ShutdownBroker;
begin
  FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_SHUTDOWN_BROKER, [], 30000);
end;

function TSBWModuleImpl.GetServiceDescriptorsByName(const ModuleName: string): TArray<TSBWList>;
var
  Writer: TSBWDataBlockWriter;
  ResultBytes: TBytes;
  Reader: TSBWDataBlockReader;
  TypeByte, ElementTypeByte: Byte;
  Dims, Size, I: Int32;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteString(ModuleName);
    ResultBytes := FConnection.Call(SBW_BROKER_MODULE_ID, 0, SYS_METHOD_GET_SERVICE_DESCS_BY_NAME,
      Writer.ToBytes, 30000);
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(ResultBytes);
  try
    // Read array header
    Reader.Stream.ReadBuffer(TypeByte, 1);
    if TSBWDataBlockType(TypeByte) <> dbtArray then
      raise ESBWDataBlockError.Create('Expected array of lists');

    Reader.Stream.ReadBuffer(ElementTypeByte, 1);
    if TSBWDataBlockType(ElementTypeByte) <> dbtList then
      raise ESBWDataBlockError.Create('Expected array of lists');

    Reader.Stream.ReadBuffer(Dims, SizeOf(Int32));
    if Dims <> 1 then
      raise ESBWDataBlockError.Create('Expected 1D array');

    Reader.Stream.ReadBuffer(Size, SizeOf(Int32));

    // Read each list
    SetLength(Result, Size);
    for I := 0 to Size - 1 do
      Result[I] := TSBWList.ReadFrom(Reader);
  finally
    Reader.Free;
  end;
end;


end.

