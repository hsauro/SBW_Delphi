unit SBW.Broker.ModuleService;

(******************************************************************************
 * SBW.Broker.ModuleService.pas
 *
 * Module System Service (Service -1) handler for the SBW Broker.
 *
 * This internal service is implemented by ALL modules (including the broker)
 * and provides infrastructure for the client libraries to query module metadata.
 *
 * Methods:
 *   0: string[] getServices()
 *      Returns names of services implemented on this module
 *   1: string[] getMethods(int serviceId)
 *      Returns method signatures for the given service
 *   2: void onOtherModuleInstanceShutdown(int moduleId)
 *      Called when another module instance shuts down
 *   3: void shutdown()
 *      Requests module termination
 *   4: string getMethodHelp(int serviceId, int methodId)
 *      Returns documentation for the given method
 *   5: void onOtherModuleInstanceStartup(int moduleId)
 *      Called when another module instance starts up
 *   6: void onRegistrationChange()
 *      Called when the broker registry changes
 *****************************************************************************)

interface

uses
  System.SysUtils, System.Classes,
  SBW.Types, SBW.DataBlock, SBW.Message, SBW.Connection, SBW.BrokerTypes;

const
  // Module System Service ID
  MODULE_SYSTEM_SERVICE_ID = -1;

  // Hardcoded method IDs for Module System Service (service -1)
  MODSYS_GET_SERVICES             = 0;
  MODSYS_GET_METHODS              = 1;
  MODSYS_ON_OTHER_MODULE_SHUTDOWN = 2;
  MODSYS_SHUTDOWN                 = 3;
  MODSYS_GET_METHOD_HELP          = 4;
  MODSYS_ON_OTHER_MODULE_STARTUP  = 5;
  MODSYS_ON_REGISTRATION_CHANGE   = 6;

type
  /// <summary>
  /// Handler for Module System Service (service -1) calls to the broker
  /// </summary>
  TSBWModuleServiceHandler = class
  private
    FBroker: TObject;  // Actually TSBWBroker, but avoiding circular reference

    function GetServices: TBytes;
    function GetMethods(ServiceID: SBWServiceID): TBytes;
    function GetMethodHelp(ServiceID: SBWServiceID; MethodID: SBWMethodID): TBytes;
  public
    constructor Create(ABroker: TObject);

    /// <summary>
    /// Handle an incoming call to service -1
    /// </summary>
    procedure HandleCall(const CallMsg: TSBWCallMessage; SourceModule: TSBWModuleInfo);
  end;

implementation

uses
  SBW.Broker.SystemService;

{ TSBWModuleServiceHandler }

constructor TSBWModuleServiceHandler.Create(ABroker: TObject);
begin
  inherited Create;
  FBroker := ABroker;
end;

procedure TSBWModuleServiceHandler.HandleCall(const CallMsg: TSBWCallMessage;
  SourceModule: TSBWModuleInfo);
var
  Reader: TSBWDataBlockReader;
  ResultData, ReplyData: TBytes;
  ServiceID: SBWServiceID;
  MethodID: SBWMethodID;
  Writer: TSBWDataBlockWriter;
begin
  try
    Reader := TSBWDataBlockReader.Create(CallMsg.Payload);
    try
      case CallMsg.MethodID of
        MODSYS_GET_SERVICES:
          ResultData := GetServices;

        MODSYS_GET_METHODS:
          begin
            ServiceID := Reader.ReadInteger;
            ResultData := GetMethods(ServiceID);
          end;

        MODSYS_ON_OTHER_MODULE_SHUTDOWN,
        MODSYS_ON_OTHER_MODULE_STARTUP,
        MODSYS_ON_REGISTRATION_CHANGE:
          begin
            // These are notifications TO the broker - just acknowledge
            Writer := TSBWDataBlockWriter.Create;
            try
              ResultData := Writer.ToBytes;
            finally
              Writer.Free;
            end;
          end;

        MODSYS_SHUTDOWN:
          begin
            // Broker ignores shutdown requests but acknowledges them
            Writer := TSBWDataBlockWriter.Create;
            try
              ResultData := Writer.ToBytes;
            finally
              Writer.Free;
            end;
          end;

        MODSYS_GET_METHOD_HELP:
          begin
            ServiceID := Reader.ReadInteger;
            MethodID := Reader.ReadInteger;
            ResultData := GetMethodHelp(ServiceID, MethodID);
          end;
      else
        ReplyData := TSBWMessageWriter.BuildErrorMessage(
          SourceModule.ModuleID, CallMsg.UID, ecMethodNotFound,
          Format('Module system method %d not found', [CallMsg.MethodID]), '');
        SourceModule.Connection.SendMessage(ReplyData);
        Exit;
      end;
    finally
      Reader.Free;
    end;

    ReplyData := TSBWMessageWriter.BuildReplyMessage(
      SourceModule.ModuleID, CallMsg.UID, ResultData);
    SourceModule.Connection.SendMessage(ReplyData);

  except
    on E: Exception do
    begin
      ReplyData := TSBWMessageWriter.BuildErrorMessage(
        SourceModule.ModuleID, CallMsg.UID, ecApplication,
        E.Message, E.ClassName);
      SourceModule.Connection.SendMessage(ReplyData);
    end;
  end;
end;

function TSBWModuleServiceHandler.GetServices: TBytes;
var
  Writer: TSBWDataBlockWriter;
begin
  // Returns string[] - the names of services on the broker
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteStringArray(['SYSTEM']);
    Result := Writer.ToBytes;
  finally
    Writer.Free;
  end;
end;

function TSBWModuleServiceHandler.GetMethods(ServiceID: SBWServiceID): TBytes;
var
  Writer: TSBWDataBlockWriter;
  Signatures: TArray<string>;
begin
  // Returns string[] - the method signatures for the given service
  Writer := TSBWDataBlockWriter.Create;
  try
    if ServiceID = BROKER_SYSTEM_SERVICE_ID then
    begin
      Signatures := [
        SIG_GET_LIST_OF_MODULES,
        SIG_GET_SERVICE_IDS_BY_ID,
        SIG_GET_SERVICE_IDS_BY_NAME,
        SIG_GET_METHOD_IDS_BY_ID,
        SIG_GET_METHOD_IDS_BY_NAME,
        SIG_GET_MODULE_ID,
        SIG_GET_VERSION,
        SIG_GET_MODULE_DESCRIPTORS,
        SIG_GET_MODULE_DESC_BY_NAME,
        SIG_GET_MODULE_DESC_BY_ID,
        SIG_GET_MODULE_INSTANCE,
        SIG_FIND_SERVICES,
        SIG_GET_SERVICE_CATEGORIES,
        SIG_RELEASE,
        SIG_GET_SERVICE_DESC_BY_NAME,
        SIG_GET_SERVICE_DESC_BY_ID,
        SIG_GET_SERVICE_DESCRIPTORS,
        SIG_GET_EXISTING_MODULE_IDS,
        SIG_UNREGISTER_MODULE,
        SIG_REGISTER_MODULE,
        SIG_REGISTER_SERVICE,
        SIG_SHUTDOWN_BROKER,
        SIG_GET_SERVICE_DESCS_BY_NAME
      ];
      Writer.WriteStringArray(Signatures);
    end
    else
      Writer.WriteStringArray([]);

    Result := Writer.ToBytes;
  finally
    Writer.Free;
  end;
end;

function TSBWModuleServiceHandler.GetMethodHelp(ServiceID: SBWServiceID;
  MethodID: SBWMethodID): TBytes;
var
  Writer: TSBWDataBlockWriter;
  HelpStr: string;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    HelpStr := '';

    if ServiceID = BROKER_SYSTEM_SERVICE_ID then
    begin
      case MethodID of
        SYS_METHOD_GET_LIST_OF_MODULES:
          HelpStr := 'Returns list of [moduleID, moduleName] for all connected modules';
        SYS_METHOD_GET_SERVICE_IDS_BY_ID:
          HelpStr := 'Returns list of [serviceID, serviceName] for a module by ID';
        SYS_METHOD_GET_SERVICE_IDS_BY_NAME:
          HelpStr := 'Returns list of [serviceID, serviceName] for a module by name';
        SYS_METHOD_GET_METHOD_IDS_BY_ID:
          HelpStr := 'Returns list of [methodID, signature] for a service by IDs';
        SYS_METHOD_GET_METHOD_IDS_BY_NAME:
          HelpStr := 'Returns list of [methodID, signature] for a service by names';
        SYS_METHOD_GET_MODULE_ID:
          HelpStr := 'Returns module ID for a module name, or -1 if not found';
        SYS_METHOD_GET_VERSION:
          HelpStr := 'Returns broker version string';
        SYS_METHOD_GET_MODULE_DESCRIPTORS:
          HelpStr := 'Returns array of module descriptors for registered/running modules';
        SYS_METHOD_GET_MODULE_DESC_BY_NAME:
          HelpStr := 'Returns module descriptor for the named module';
        SYS_METHOD_GET_MODULE_DESC_BY_ID:
          HelpStr := 'Returns module descriptor for the module with given ID';
        SYS_METHOD_GET_MODULE_INSTANCE:
          HelpStr := 'Returns module instance ID, starting module if necessary';
        SYS_METHOD_FIND_SERVICES:
          HelpStr := 'Returns service descriptors for services in a category';
        SYS_METHOD_GET_SERVICE_CATEGORIES:
          HelpStr := 'Returns subcategories of the given category';
        SYS_METHOD_RELEASE:
          HelpStr := 'Disconnects broker from a module';
        SYS_METHOD_GET_SERVICE_DESC_BY_NAME:
          HelpStr := 'Returns service descriptor by module ID and service name';
        SYS_METHOD_GET_SERVICE_DESC_BY_ID:
          HelpStr := 'Returns service descriptor by module ID and service ID';
        SYS_METHOD_GET_SERVICE_DESCRIPTORS:
          HelpStr := 'Returns all service descriptors for a module';
        SYS_METHOD_GET_EXISTING_MODULE_IDS:
          HelpStr := 'Returns array of connected module instance IDs';
        SYS_METHOD_UNREGISTER_MODULE:
          HelpStr := 'Removes module from registry';
        SYS_METHOD_REGISTER_MODULE:
          HelpStr := 'Adds module to registry';
        SYS_METHOD_REGISTER_SERVICE:
          HelpStr := 'Adds service to registry for a module';
      end;
    end
    else if ServiceID = MODULE_SYSTEM_SERVICE_ID then
    begin
      case MethodID of
        MODSYS_GET_SERVICES:
          HelpStr := 'Returns the names of services implemented on this module';
        MODSYS_GET_METHODS:
          HelpStr := 'Returns the method signatures for the given service';
        MODSYS_ON_OTHER_MODULE_SHUTDOWN:
          HelpStr := 'Called when another module instance shuts down';
        MODSYS_SHUTDOWN:
          HelpStr := 'Requests module termination';
        MODSYS_GET_METHOD_HELP:
          HelpStr := 'Returns documentation for the given method';
        MODSYS_ON_OTHER_MODULE_STARTUP:
          HelpStr := 'Called when another module instance starts up';
        MODSYS_ON_REGISTRATION_CHANGE:
          HelpStr := 'Called when the broker registry changes';
      end;
    end;

    Writer.WriteString(HelpStr);
    Result := Writer.ToBytes;
  finally
    Writer.Free;
  end;
end;

end.
