unit SBW.Broker.SystemService;

(******************************************************************************
 * SBW.Broker.SystemService.pas
 *
 * Broker System Service (Service 0) handler for the SBW Broker.
 *
 * This public service provides discovery and management methods.
 *
 * Methods:
 *   0: {} getListOfModules()
 *   1: {} getServiceIds(int moduleId)
 *   2: {} getServiceIds(string moduleName)
 *   3: {} getMethodIds(int moduleId, int serviceId)
 *   4: {} getMethodIds(string moduleName, string serviceName)
 *   5: int getModuleId(string moduleName)
 *   6: string getVersion()
 *   7: {}[] getModuleDescriptors(boolean localOnly, boolean includeRunning)
 *   8: {} getModuleDescriptor(string moduleName, boolean includeRunning)
 *   9: {} getModuleDescriptor(int moduleId)
 *  10: int getModuleInstance(string moduleName)
 *  11: {}[] findServices(string category, boolean recursive)
 *  12: string[] getServiceCategories(string category)
 *  13: void release(int moduleId)
 *  14: {} getServiceDescriptor(int moduleId, string serviceName)
 *  15: {} getServiceDescriptor(int moduleId, int serviceId)
 *  16: {}[] getServiceDescriptors(int moduleId)
 *  17: int[] getExistingModuleInstanceIds()
 *  18: void unregisterModule(string moduleName)
 *  19: void registerModule(string name, string displayName, int type, string cmdLine, string help)
 *  20: void registerService(string moduleName, string serviceName, string displayName, string category, string help)
 * 100: Internal registerServices (module registers its services)
 *****************************************************************************)

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  SBW.Types, SBW.DataBlock, SBW.Message, SBW.BrokerTypes;

const
  // Broker's public SYSTEM service (service 0)
  BROKER_SYSTEM_SERVICE_ID = 0;
  BROKER_SERVICE_NAME = 'SYSTEM';
  BROKER_SERVICE_DISPLAY = 'Broker System Service';
  BROKER_SERVICE_CATEGORY = 'System';
  BROKER_SERVICE_HELP = 'Core broker discovery and management methods';

  // System service method IDs (service 0)
  SYS_METHOD_GET_LIST_OF_MODULES       = 0;
  SYS_METHOD_GET_SERVICE_IDS_BY_ID     = 1;
  SYS_METHOD_GET_SERVICE_IDS_BY_NAME   = 2;
  SYS_METHOD_GET_METHOD_IDS_BY_ID      = 3;
  SYS_METHOD_GET_METHOD_IDS_BY_NAME    = 4;
  SYS_METHOD_GET_MODULE_ID             = 5;
  SYS_METHOD_GET_VERSION               = 6;
  SYS_METHOD_GET_MODULE_DESCRIPTORS    = 7;
  SYS_METHOD_GET_MODULE_DESC_BY_NAME   = 8;
  SYS_METHOD_GET_MODULE_DESC_BY_ID     = 9;
  SYS_METHOD_GET_MODULE_INSTANCE       = 10;
  SYS_METHOD_FIND_SERVICES             = 11;
  SYS_METHOD_GET_SERVICE_CATEGORIES    = 12;
  SYS_METHOD_RELEASE                   = 13;
  SYS_METHOD_GET_SERVICE_DESC_BY_NAME  = 14;
  SYS_METHOD_GET_SERVICE_DESC_BY_ID    = 15;
  SYS_METHOD_GET_SERVICE_DESCRIPTORS   = 16;
  SYS_METHOD_GET_EXISTING_MODULE_IDS   = 17;
  SYS_METHOD_UNREGISTER_MODULE         = 18;
  SYS_METHOD_REGISTER_MODULE           = 19;
  SYS_METHOD_REGISTER_SERVICE          = 20;
  SYS_METHOD_SHUTDOWN_BROKER            = 21;
  SYS_METHOD_GET_SERVICE_DESCS_BY_NAME  = 22;
  SYS_METHOD_REGISTER_SERVICES         = 100;  // Internal: module registers its services

  // Method signatures for the system service
  SIG_GET_LIST_OF_MODULES      = '{} getListOfModules()';
  SIG_GET_SERVICE_IDS_BY_ID    = '{} getServiceIds(int)';
  SIG_GET_SERVICE_IDS_BY_NAME  = '{} getServiceIds(string)';
  SIG_GET_METHOD_IDS_BY_ID     = '{} getMethodIds(int,int)';
  SIG_GET_METHOD_IDS_BY_NAME   = '{} getMethodIds(string,string)';
  SIG_GET_MODULE_ID            = 'int getModuleId(string)';
  SIG_GET_VERSION              = 'string getVersion()';
  SIG_GET_MODULE_DESCRIPTORS   = '{}[] getModuleDescriptors(boolean,boolean)';
  SIG_GET_MODULE_DESC_BY_NAME  = '{} getModuleDescriptor(string,boolean)';
  SIG_GET_MODULE_DESC_BY_ID    = '{} getModuleDescriptor(int)';
  SIG_GET_MODULE_INSTANCE      = 'int getModuleInstance(string)';
  SIG_FIND_SERVICES            = '{}[] findServices(string,boolean)';
  SIG_GET_SERVICE_CATEGORIES   = 'string[] getServiceCategories(string)';
  SIG_RELEASE                  = 'void release(int)';
  SIG_GET_SERVICE_DESC_BY_NAME = '{} getServiceDescriptor(int,string)';
  SIG_GET_SERVICE_DESC_BY_ID   = '{} getServiceDescriptor(int,int)';
  SIG_GET_SERVICE_DESCRIPTORS  = '{}[] getServiceDescriptors(int)';
  SIG_GET_EXISTING_MODULE_IDS  = 'int[] getExistingModuleInstanceIds()';
  SIG_UNREGISTER_MODULE        = 'void unregisterModule(string)';
  SIG_REGISTER_MODULE          = 'void registerModule(string,string,int,string,string)';
  SIG_REGISTER_SERVICE         = 'void registerService(string,string,string,string,string)';
  SIG_SHUTDOWN_BROKER          = 'void shutdownBroker()';
  SIG_GET_SERVICE_DESCS_BY_NAME = '{}[] getServiceDescriptors(string)';

type
  /// <summary>
  /// Handler for Broker System Service (service 0) calls
  /// </summary>
  TSBWSystemServiceHandler = class
  private
    FBroker: TObject; //TSBWBroker;

    // Existing methods
    function GetListOfModules: TBytes;
    function GetServiceIdsByModuleId(ModuleID: SBWModuleID): TBytes;
    function GetServiceIdsByModuleName(const ModuleName: string): TBytes;
    function GetMethodIdsByIds(ModuleID: SBWModuleID; ServiceID: SBWServiceID): TBytes;
    function GetMethodIdsByNames(const ModuleName, ServiceName: string): TBytes;
    function GetModuleId(const ModuleName: string): TBytes;
    function GetVersion: TBytes;
    function GetModuleDescriptors(LocalOnly, IncludeRunning: Boolean): TBytes;
    function GetModuleDescriptorByName(const ModuleName: string; IncludeRunning: Boolean): TBytes;
    function GetModuleDescriptorById(ModuleID: SBWModuleID): TBytes;

    // New methods
    function GetModuleInstance(const ModuleName: string): TBytes;
    function FindServices(const Category: string; Recursive: Boolean): TBytes;
    function GetServiceCategories(const Category: string): TBytes;
    procedure Release(ModuleID: SBWModuleID);
    function GetServiceDescriptorByName(ModuleID: SBWModuleID; const ServiceName: string): TBytes;
    function GetServiceDescriptorById(ModuleID: SBWModuleID; ServiceID: SBWServiceID): TBytes;
    function GetServiceDescriptors(ModuleID: SBWModuleID): TBytes;
    function GetExistingModuleInstanceIds: TBytes;
    procedure UnregisterModule(const ModuleName: string);
    procedure RegisterModule(const Name, DisplayName: string; ModuleType: Integer;
      const CommandLine, Help: string);
    procedure RegisterServiceInRegistry(const ModuleName, ServiceName, DisplayName,
      Category, Help: string);
    procedure ShutdownBroker;
    function GetServiceDescriptorsByName(const ModuleName: string): TBytes;

    // Internal
    procedure HandleRegisterServices(const CallMsg: TSBWCallMessage; SourceModule: TSBWModuleInfo);

    // Helpers
    procedure WriteModuleDescriptor(Writer: TSBWDataBlockWriter; Module: TSBWModuleInfo);
    procedure WriteBrokerDescriptor(Writer: TSBWDataBlockWriter);
    procedure WriteServiceDescriptor(Writer: TSBWDataBlockWriter; Module: TSBWModuleInfo;
      Service: TSBWBrokerServiceInfo);
  public
    constructor Create(ABroker: TObject);

    /// <summary>
    /// Handle an incoming call to service 0
    /// </summary>
    procedure HandleCall(const CallMsg: TSBWCallMessage; SourceModule: TSBWModuleInfo);
  end;

implementation

uses
  SBW.Broker, SBW.Connection, SBW.Broker.Registry;

const
  VERSION = '0.51';

{ TSBWSystemServiceHandler }

constructor TSBWSystemServiceHandler.Create(ABroker: TObject);
begin
  inherited Create;
  FBroker := ABroker;
end;

procedure TSBWSystemServiceHandler.HandleCall(const CallMsg: TSBWCallMessage;
  SourceModule: TSBWModuleInfo);
var
  Reader: TSBWDataBlockReader;
  ResultData, ReplyData: TBytes;
  ModuleID: SBWModuleID;
  ServiceID: SBWServiceID;
  ModuleName, ServiceName: string;
  Writer: TSBWDataBlockWriter;
begin
  try
    Reader := TSBWDataBlockReader.Create(CallMsg.Payload);
    try
      case CallMsg.MethodID of
        SYS_METHOD_GET_LIST_OF_MODULES:
          ResultData := GetListOfModules;

        SYS_METHOD_GET_SERVICE_IDS_BY_ID:
          begin
            ModuleID := Reader.ReadInteger;
            ResultData := GetServiceIdsByModuleId(ModuleID);
          end;

        SYS_METHOD_GET_SERVICE_IDS_BY_NAME:
          begin
            ModuleName := Reader.ReadString;
            ResultData := GetServiceIdsByModuleName(ModuleName);
          end;

        SYS_METHOD_GET_METHOD_IDS_BY_ID:
          begin
            ModuleID := Reader.ReadInteger;
            ServiceID := Reader.ReadInteger;
            ResultData := GetMethodIdsByIds(ModuleID, ServiceID);
          end;

        SYS_METHOD_GET_METHOD_IDS_BY_NAME:
          begin
            ModuleName := Reader.ReadString;
            ServiceName := Reader.ReadString;
            ResultData := GetMethodIdsByNames(ModuleName, ServiceName);
          end;

        SYS_METHOD_GET_MODULE_ID:
          begin
            ModuleName := Reader.ReadString;
            ResultData := GetModuleId(ModuleName);
          end;

        SYS_METHOD_GET_VERSION:
          ResultData := GetVersion;

        SYS_METHOD_GET_MODULE_DESCRIPTORS:
          begin
            var LocalOnly := Reader.ReadBoolean;
            var IncludeRunning := Reader.ReadBoolean;
            ResultData := GetModuleDescriptors(LocalOnly, IncludeRunning);
          end;

        SYS_METHOD_GET_MODULE_DESC_BY_NAME:
          begin
            ModuleName := Reader.ReadString;
            var IncludeRunning := Reader.ReadBoolean;
            ResultData := GetModuleDescriptorByName(ModuleName, IncludeRunning);
          end;

        SYS_METHOD_GET_MODULE_DESC_BY_ID:
          begin
            ModuleID := Reader.ReadInteger;
            ResultData := GetModuleDescriptorById(ModuleID);
          end;

        SYS_METHOD_GET_MODULE_INSTANCE:
          begin
            ModuleName := Reader.ReadString;
            ResultData := GetModuleInstance(ModuleName);
          end;

        SYS_METHOD_FIND_SERVICES:
          begin
            var Category := Reader.ReadString;
            var Recursive := Reader.ReadBoolean;
            ResultData := FindServices(Category, Recursive);
          end;

        SYS_METHOD_GET_SERVICE_CATEGORIES:
          begin
            var Category := Reader.ReadString;
            ResultData := GetServiceCategories(Category);
          end;

        SYS_METHOD_RELEASE:
          begin
            ModuleID := Reader.ReadInteger;
            Release(ModuleID);
            Writer := TSBWDataBlockWriter.Create;
            try
              ResultData := Writer.ToBytes;
            finally
              Writer.Free;
            end;
          end;

        SYS_METHOD_GET_SERVICE_DESC_BY_NAME:
          begin
            ModuleID := Reader.ReadInteger;
            ServiceName := Reader.ReadString;
            ResultData := GetServiceDescriptorByName(ModuleID, ServiceName);
          end;

        SYS_METHOD_GET_SERVICE_DESC_BY_ID:
          begin
            ModuleID := Reader.ReadInteger;
            ServiceID := Reader.ReadInteger;
            ResultData := GetServiceDescriptorById(ModuleID, ServiceID);
          end;

        SYS_METHOD_GET_SERVICE_DESCRIPTORS:
          begin
            ModuleID := Reader.ReadInteger;
            ResultData := GetServiceDescriptors(ModuleID);
          end;

        SYS_METHOD_GET_EXISTING_MODULE_IDS:
          ResultData := GetExistingModuleInstanceIds;

        SYS_METHOD_UNREGISTER_MODULE:
          begin
            ModuleName := Reader.ReadString;
            UnregisterModule(ModuleName);
            Writer := TSBWDataBlockWriter.Create;
            try
              ResultData := Writer.ToBytes;
            finally
              Writer.Free;
            end;
          end;

        SYS_METHOD_REGISTER_MODULE:
          begin
            var Name := Reader.ReadString;
            var DisplayName := Reader.ReadString;
            var ModType := Reader.ReadInteger;
            var CmdLine := Reader.ReadString;
            var HelpStr := Reader.ReadString;
            RegisterModule(Name, DisplayName, ModType, CmdLine, HelpStr);
            Writer := TSBWDataBlockWriter.Create;
            try
              ResultData := Writer.ToBytes;
            finally
              Writer.Free;
            end;
          end;

        SYS_METHOD_REGISTER_SERVICE:
          begin
            ModuleName := Reader.ReadString;
            ServiceName := Reader.ReadString;
            var DisplayName := Reader.ReadString;
            var Category := Reader.ReadString;
            var HelpStr := Reader.ReadString;
            RegisterServiceInRegistry(ModuleName, ServiceName, DisplayName, Category, HelpStr);
            Writer := TSBWDataBlockWriter.Create;
            try
              ResultData := Writer.ToBytes;
            finally
              Writer.Free;
            end;
          end;

        SYS_METHOD_SHUTDOWN_BROKER:
          begin
            ShutdownBroker;
            Writer := TSBWDataBlockWriter.Create;
            try
              ResultData := Writer.ToBytes;
            finally
              Writer.Free;
            end;
          end;

        SYS_METHOD_GET_SERVICE_DESCS_BY_NAME:
          begin
            ModuleName := Reader.ReadString;
            ResultData := GetServiceDescriptorsByName(ModuleName);
          end;

        SYS_METHOD_REGISTER_SERVICES:
          begin
            HandleRegisterServices(CallMsg, SourceModule);
            Exit; // HandleRegisterServices sends its own reply
          end;
      else
        ReplyData := TSBWMessageWriter.BuildErrorMessage(
          SourceModule.ModuleID, CallMsg.UID, ecMethodNotFound,
          Format('Broker method %d not found', [CallMsg.MethodID]), '');
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

{ Helper methods }

procedure TSBWSystemServiceHandler.WriteBrokerDescriptor(Writer: TSBWDataBlockWriter);
begin
  Writer.WriteListBegin(5);
  Writer.WriteString('BROKER');
  Writer.WriteString('SBW Broker');
  Writer.WriteInteger(Ord(mmtUnique));
  Writer.WriteString('');
  Writer.WriteString('SBW message broker and module registry');
end;

procedure TSBWSystemServiceHandler.WriteModuleDescriptor(Writer: TSBWDataBlockWriter;
  Module: TSBWModuleInfo);
begin
  Writer.WriteListBegin(5);
  Writer.WriteString(Module.ModuleName);
  Writer.WriteString(Module.DisplayName);
  Writer.WriteInteger(Ord(Module.ModuleType));
  Writer.WriteString(Module.CommandLine);
  Writer.WriteString(Module.Help);
end;

procedure TSBWSystemServiceHandler.WriteServiceDescriptor(Writer: TSBWDataBlockWriter;
  Module: TSBWModuleInfo; Service: TSBWBrokerServiceInfo);
begin
  // Service descriptor: [moduleName, serviceName, displayName, category, help]
  Writer.WriteListBegin(5);
  Writer.WriteString(Module.ModuleName);
  Writer.WriteString(Service.Name);
  Writer.WriteString(Service.DisplayName);
  Writer.WriteString(Service.Category);
  Writer.WriteString(Service.Help);
end;

{ Existing method implementations }

function TSBWSystemServiceHandler.GetListOfModules: TBytes;
var
  Writer: TSBWDataBlockWriter;
  Module: TSBWModuleInfo;
  ModuleList: TArray<TSBWModuleInfo>;
  I: Integer;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    ModuleList := (FBroker as TSBWBroker).GetAllModules;

    Writer.WriteListBegin(Length(ModuleList) + 1);

    // First, write the broker as module 0
    Writer.WriteListBegin(2);
    Writer.WriteInteger(SBW_BROKER_MODULE_ID);
    Writer.WriteString('BROKER');

    // Then write all connected modules
    for I := 0 to High(ModuleList) do
    begin
      Module := ModuleList[I];
      Writer.WriteListBegin(2);
      Writer.WriteInteger(Module.ModuleID);
      Writer.WriteString(Module.ModuleName);
    end;

    Result := Writer.ToBytes;
  finally
    Writer.Free;
  end;
end;

function TSBWSystemServiceHandler.GetServiceIdsByModuleId(ModuleID: SBWModuleID): TBytes;
var
  Writer: TSBWDataBlockWriter;
  Module: TSBWModuleInfo;
  Svc: TSBWBrokerServiceInfo;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    if ModuleID = SBW_BROKER_MODULE_ID then
    begin
      Writer.WriteListBegin(1);
      Writer.WriteListBegin(2);
      Writer.WriteInteger(BROKER_SYSTEM_SERVICE_ID);
      Writer.WriteString(BROKER_SERVICE_NAME);
    end
    else
    begin
      Module := (FBroker as TSBWBroker).GetModule(ModuleID);
      if Module <> nil then
      begin
        Writer.WriteListBegin(Module.Services.Count);
        for Svc in Module.Services do
        begin
          Writer.WriteListBegin(2);
          Writer.WriteInteger(Svc.ServiceID);
          Writer.WriteString(Svc.Name);
        end;
      end
      else
        Writer.WriteListBegin(0);
    end;
    Result := Writer.ToBytes;
  finally
    Writer.Free;
  end;
end;

function TSBWSystemServiceHandler.GetServiceIdsByModuleName(const ModuleName: string): TBytes;
var
  Module: TSBWModuleInfo;
begin
  if SameText(ModuleName, 'BROKER') then
    Result := GetServiceIdsByModuleId(SBW_BROKER_MODULE_ID)
  else
  begin
    Module := (FBroker as TSBWBroker).GetModuleByName(ModuleName);
    if Module <> nil then
      Result := GetServiceIdsByModuleId(Module.ModuleID)
    else
    begin
      var Writer := TSBWDataBlockWriter.Create;
      try
        Writer.WriteListBegin(0);
        Result := Writer.ToBytes;
      finally
        Writer.Free;
      end;
    end;
  end;
end;

function TSBWSystemServiceHandler.GetMethodIdsByIds(ModuleID: SBWModuleID;
  ServiceID: SBWServiceID): TBytes;
var
  Writer: TSBWDataBlockWriter;
  Module: TSBWModuleInfo;
  Svc: TSBWBrokerServiceInfo;
  Meth: TSBWBrokerMethodInfo;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    if (ModuleID = SBW_BROKER_MODULE_ID) and (ServiceID = BROKER_SYSTEM_SERVICE_ID) then
    begin
      // Return broker's system service methods
      Writer.WriteListBegin(23);  // 21 methods (0-20, excluding 100)

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_GET_LIST_OF_MODULES);
      Writer.WriteString(SIG_GET_LIST_OF_MODULES);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_GET_SERVICE_IDS_BY_ID);
      Writer.WriteString(SIG_GET_SERVICE_IDS_BY_ID);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_GET_SERVICE_IDS_BY_NAME);
      Writer.WriteString(SIG_GET_SERVICE_IDS_BY_NAME);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_GET_METHOD_IDS_BY_ID);
      Writer.WriteString(SIG_GET_METHOD_IDS_BY_ID);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_GET_METHOD_IDS_BY_NAME);
      Writer.WriteString(SIG_GET_METHOD_IDS_BY_NAME);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_GET_MODULE_ID);
      Writer.WriteString(SIG_GET_MODULE_ID);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_GET_VERSION);
      Writer.WriteString(SIG_GET_VERSION);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_GET_MODULE_DESCRIPTORS);
      Writer.WriteString(SIG_GET_MODULE_DESCRIPTORS);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_GET_MODULE_DESC_BY_NAME);
      Writer.WriteString(SIG_GET_MODULE_DESC_BY_NAME);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_GET_MODULE_DESC_BY_ID);
      Writer.WriteString(SIG_GET_MODULE_DESC_BY_ID);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_GET_MODULE_INSTANCE);
      Writer.WriteString(SIG_GET_MODULE_INSTANCE);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_FIND_SERVICES);
      Writer.WriteString(SIG_FIND_SERVICES);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_GET_SERVICE_CATEGORIES);
      Writer.WriteString(SIG_GET_SERVICE_CATEGORIES);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_RELEASE);
      Writer.WriteString(SIG_RELEASE);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_GET_SERVICE_DESC_BY_NAME);
      Writer.WriteString(SIG_GET_SERVICE_DESC_BY_NAME);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_GET_SERVICE_DESC_BY_ID);
      Writer.WriteString(SIG_GET_SERVICE_DESC_BY_ID);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_GET_SERVICE_DESCRIPTORS);
      Writer.WriteString(SIG_GET_SERVICE_DESCRIPTORS);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_GET_EXISTING_MODULE_IDS);
      Writer.WriteString(SIG_GET_EXISTING_MODULE_IDS);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_UNREGISTER_MODULE);
      Writer.WriteString(SIG_UNREGISTER_MODULE);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_REGISTER_MODULE);
      Writer.WriteString(SIG_REGISTER_MODULE);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_REGISTER_SERVICE);
      Writer.WriteString(SIG_REGISTER_SERVICE);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_SHUTDOWN_BROKER);
      Writer.WriteString(SIG_SHUTDOWN_BROKER);

      Writer.WriteListBegin(2);
      Writer.WriteInteger(SYS_METHOD_GET_SERVICE_DESCS_BY_NAME);
      Writer.WriteString(SIG_GET_SERVICE_DESCS_BY_NAME);
    end
    else
    begin
      Module := (FBroker as TSBWBroker).GetModule(ModuleID);
      if Module <> nil then
      begin
        Svc := Module.FindServiceByID(ServiceID);
        if Svc <> nil then
        begin
          Writer.WriteListBegin(Svc.Methods.Count);
          for Meth in Svc.Methods do
          begin
            Writer.WriteListBegin(2);
            Writer.WriteInteger(Meth.MethodID);
            Writer.WriteString(Meth.Signature);
          end;
        end
        else
          Writer.WriteListBegin(0);
      end
      else
        Writer.WriteListBegin(0);
    end;
    Result := Writer.ToBytes;
  finally
    Writer.Free;
  end;
end;

function TSBWSystemServiceHandler.GetMethodIdsByNames(const ModuleName,
  ServiceName: string): TBytes;
var
  Module: TSBWModuleInfo;
  Svc: TSBWBrokerServiceInfo;
begin
  if SameText(ModuleName, 'BROKER') and SameText(ServiceName, 'SYSTEM') then
    Result := GetMethodIdsByIds(SBW_BROKER_MODULE_ID, BROKER_SYSTEM_SERVICE_ID)
  else
  begin
    Module := (FBroker as TSBWBroker).GetModuleByName(ModuleName);
    if Module <> nil then
    begin
      Svc := Module.FindServiceByName(ServiceName);
      if Svc <> nil then
        Result := GetMethodIdsByIds(Module.ModuleID, Svc.ServiceID)
      else
      begin
        var Writer := TSBWDataBlockWriter.Create;
        try
          Writer.WriteListBegin(0);
          Result := Writer.ToBytes;
        finally
          Writer.Free;
        end;
      end;
    end
    else
    begin
      var Writer := TSBWDataBlockWriter.Create;
      try
        Writer.WriteListBegin(0);
        Result := Writer.ToBytes;
      finally
        Writer.Free;
      end;
    end;
  end;
end;

function TSBWSystemServiceHandler.GetModuleId(const ModuleName: string): TBytes;
var
  Writer: TSBWDataBlockWriter;
  Module: TSBWModuleInfo;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    if SameText(ModuleName, 'BROKER') then
      Writer.WriteInteger(SBW_BROKER_MODULE_ID)
    else
    begin
      Module := (FBroker as TSBWBroker).GetModuleByName(ModuleName);
      if Module <> nil then
        Writer.WriteInteger(Module.ModuleID)
      else
        Writer.WriteInteger(SBW_NO_MODULE_ID);
    end;
    Result := Writer.ToBytes;
  finally
    Writer.Free;
  end;
end;

function TSBWSystemServiceHandler.GetVersion: TBytes;
var
  Writer: TSBWDataBlockWriter;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteString(VERSION);
    Result := Writer.ToBytes;
  finally
    Writer.Free;
  end;
end;

function TSBWSystemServiceHandler.GetModuleDescriptors(LocalOnly,
  IncludeRunning: Boolean): TBytes;
var
  Writer: TSBWDataBlockWriter;
  Module: TSBWModuleInfo;
  ModuleList: TArray<TSBWModuleInfo>;
  RegModules: TArray<TSBWRegisteredModule>;
  RegMod: TSBWRegisteredModule;
  Broker: TSBWBroker;
  I, TotalCount: Integer;
  ItemWriter: TSBWDataBlockWriter;
  ItemBytes: TBytes;
  AllItems: TList<TBytes>;
  RunningNames: TList<string>;
begin
  Broker := FBroker as TSBWBroker;

  if IncludeRunning then
    ModuleList := Broker.GetAllModules
  else
    SetLength(ModuleList, 0);

  AllItems := TList<TBytes>.Create;
  RunningNames := TList<string>.Create;
  try
    // First, add the broker itself
    ItemWriter := TSBWDataBlockWriter.Create;
    try
      WriteBrokerDescriptor(ItemWriter);
      AllItems.Add(ItemWriter.ToBytes);
    finally
      ItemWriter.Free;
    end;

    // Add connected modules and track their names
    for I := 0 to High(ModuleList) do
    begin
      Module := ModuleList[I];
      RunningNames.Add(LowerCase(Module.ModuleName));
      ItemWriter := TSBWDataBlockWriter.Create;
      try
        WriteModuleDescriptor(ItemWriter, Module);
        AllItems.Add(ItemWriter.ToBytes);
      finally
        ItemWriter.Free;
      end;
    end;

    // Add registered modules that aren't running
    RegModules := Broker.Registry.GetAllModules;
    for RegMod in RegModules do
    begin
      if RunningNames.IndexOf(LowerCase(RegMod.Name)) < 0 then
      begin
        ItemWriter := TSBWDataBlockWriter.Create;
        try
          // Write registered module descriptor
          ItemWriter.WriteListBegin(5);
          ItemWriter.WriteString(RegMod.Name);
          ItemWriter.WriteString(RegMod.DisplayName);
          ItemWriter.WriteInteger(Ord(RegMod.ModuleType));
          ItemWriter.WriteString(RegMod.CommandLine);
          ItemWriter.WriteString(RegMod.Help);
          AllItems.Add(ItemWriter.ToBytes);
        finally
          ItemWriter.Free;
        end;
      end;
    end;

    // Write as array of lists
    Writer := TSBWDataBlockWriter.Create;
    try
      var TypeByte: Byte := Ord(dbtArray);
      var ElemTypeByte: Byte := Ord(dbtList);
      var Dims: Int32 := 1;
      Writer.Stream.WriteBuffer(TypeByte, 1);
      Writer.Stream.WriteBuffer(ElemTypeByte, 1);
      Writer.Stream.WriteBuffer(Dims, SizeOf(Int32));
      TotalCount := AllItems.Count;
      Writer.Stream.WriteBuffer(TotalCount, SizeOf(Int32));

      for I := 0 to AllItems.Count - 1 do
      begin
        ItemBytes := AllItems[I];
        Writer.Stream.WriteBuffer(ItemBytes[0], Length(ItemBytes));
      end;

      Result := Writer.ToBytes;
    finally
      Writer.Free;
    end;
  finally
    RunningNames.Free;
    AllItems.Free;
  end;
end;

function TSBWSystemServiceHandler.GetModuleDescriptorByName(const ModuleName: string;
  IncludeRunning: Boolean): TBytes;
var
  Writer: TSBWDataBlockWriter;
  Module: TSBWModuleInfo;
  RegMod: TSBWRegisteredModule;
  Broker: TSBWBroker;
begin
  Broker := FBroker as TSBWBroker;

  Writer := TSBWDataBlockWriter.Create;
  try
    if SameText(ModuleName, 'BROKER') then
    begin
      WriteBrokerDescriptor(Writer);
      Result := Writer.ToBytes;
      Exit;
    end;

    // Check running modules first
    Module := nil;
    if IncludeRunning then
      Module := Broker.GetModuleByName(ModuleName);

    if Module <> nil then
      WriteModuleDescriptor(Writer, Module)
    else
    begin
      // Check registry for non-running module
      RegMod := Broker.Registry.FindModule(ModuleName);
      if RegMod <> nil then
      begin
        Writer.WriteListBegin(5);
        Writer.WriteString(RegMod.Name);
        Writer.WriteString(RegMod.DisplayName);
        Writer.WriteInteger(Ord(RegMod.ModuleType));
        Writer.WriteString(RegMod.CommandLine);
        Writer.WriteString(RegMod.Help);
      end
      else
        Writer.WriteListBegin(0);
    end;

    Result := Writer.ToBytes;
  finally
    Writer.Free;
  end;
end;

function TSBWSystemServiceHandler.GetModuleDescriptorById(ModuleID: SBWModuleID): TBytes;
var
  Writer: TSBWDataBlockWriter;
  Module: TSBWModuleInfo;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    if ModuleID = SBW_BROKER_MODULE_ID then
    begin
      WriteBrokerDescriptor(Writer);
      Result := Writer.ToBytes;
      Exit;
    end;

    Module := (FBroker as TSBWBroker).GetModule(ModuleID);
    if Module <> nil then
      WriteModuleDescriptor(Writer, Module)
    else
      Writer.WriteListBegin(0);

    Result := Writer.ToBytes;
  finally
    Writer.Free;
  end;
end;

{ New method implementations }

function TSBWSystemServiceHandler.GetModuleInstance(const ModuleName: string): TBytes;
var
  Writer: TSBWDataBlockWriter;
  Module: TSBWModuleInfo;
  Broker: TSBWBroker;
  WaitCount: Integer;
begin
  Broker := FBroker as TSBWBroker;

  Writer := TSBWDataBlockWriter.Create;
  try
    if SameText(ModuleName, 'BROKER') then
    begin
      Writer.WriteInteger(SBW_BROKER_MODULE_ID);
      Result := Writer.ToBytes;
      Exit;
    end;

    // Check if module is already running
    Module := Broker.GetModuleByName(ModuleName);
    if Module <> nil then
    begin
      Writer.WriteInteger(Module.ModuleID);
      Result := Writer.ToBytes;
      Exit;
    end;

    // Module not running - try to launch from registry
    if Broker.Registry.LaunchModule(ModuleName) then
    begin
      // Wait for module to connect (up to 10 seconds)
      WaitCount := 0;
      while WaitCount < 100 do  // 100 x 100ms = 10 seconds
      begin
        Sleep(100);
        Inc(WaitCount);
        Module := Broker.GetModuleByName(ModuleName);
        if Module <> nil then
        begin
          Writer.WriteInteger(Module.ModuleID);
          Result := Writer.ToBytes;
          Exit;
        end;
      end;
    end;

    // Module not found and could not be launched
    Writer.WriteInteger(SBW_NO_MODULE_ID);
    Result := Writer.ToBytes;
  finally
    Writer.Free;
  end;
end;

function TSBWSystemServiceHandler.FindServices(const Category: string;
  Recursive: Boolean): TBytes;
var
  Writer: TSBWDataBlockWriter;
  Module: TSBWModuleInfo;
  ModuleList: TArray<TSBWModuleInfo>;
  Svc: TSBWBrokerServiceInfo;
  I, TotalCount: Integer;
  ItemWriter: TSBWDataBlockWriter;
  ItemBytes: TBytes;
  AllItems: TList<TBytes>;
  CatLower, SvcCatLower: string;
begin
  // Returns array of service descriptors for services in the given category
  ModuleList := (FBroker as TSBWBroker).GetAllModules;
  CatLower := LowerCase(Category);

  AllItems := TList<TBytes>.Create;
  try
    for I := 0 to High(ModuleList) do
    begin
      Module := ModuleList[I];
      for Svc in Module.Services do
      begin
        SvcCatLower := LowerCase(Svc.Category);
        if (SvcCatLower = CatLower) or
           (Recursive and (Pos(CatLower + '/', SvcCatLower) = 1)) then
        begin
          ItemWriter := TSBWDataBlockWriter.Create;
          try
            WriteServiceDescriptor(ItemWriter, Module, Svc);
            AllItems.Add(ItemWriter.ToBytes);
          finally
            ItemWriter.Free;
          end;
        end;
      end;
    end;

    // Write as array of lists
    Writer := TSBWDataBlockWriter.Create;
    try
      var TypeByte: Byte := Ord(dbtArray);
      var ElemTypeByte: Byte := Ord(dbtList);
      var Dims: Int32 := 1;
      Writer.Stream.WriteBuffer(TypeByte, 1);
      Writer.Stream.WriteBuffer(ElemTypeByte, 1);
      Writer.Stream.WriteBuffer(Dims, SizeOf(Int32));
      TotalCount := AllItems.Count;
      Writer.Stream.WriteBuffer(TotalCount, SizeOf(Int32));

      for I := 0 to AllItems.Count - 1 do
      begin
        ItemBytes := AllItems[I];
        Writer.Stream.WriteBuffer(ItemBytes[0], Length(ItemBytes));
      end;

      Result := Writer.ToBytes;
    finally
      Writer.Free;
    end;
  finally
    AllItems.Free;
  end;
end;

function TSBWSystemServiceHandler.GetServiceCategories(const Category: string): TBytes;
var
  Writer: TSBWDataBlockWriter;
  Module: TSBWModuleInfo;
  ModuleList: TArray<TSBWModuleInfo>;
  Svc: TSBWBrokerServiceInfo;
  Categories: TList<string>;
  CatLower, SvcCat, SubCat: string;
  SlashPos: Integer;
begin
  // Returns immediate subcategories of the given category
  ModuleList := (FBroker as TSBWBroker).GetAllModules;
  CatLower := LowerCase(Category);
  Categories := TList<string>.Create;
  try
    for Module in ModuleList do
    begin
      for Svc in Module.Services do
      begin
        SvcCat := Svc.Category;
        // Check if this service's category starts with the given category
        if (Category = '') or (Pos(CatLower + '/', LowerCase(SvcCat)) = 1) then
        begin
          // Extract the immediate subcategory
          if Category = '' then
            SubCat := SvcCat
          else
            SubCat := Copy(SvcCat, Length(Category) + 2, MaxInt);

          SlashPos := Pos('/', SubCat);
          if SlashPos > 0 then
            SubCat := Copy(SubCat, 1, SlashPos - 1);

          if (SubCat <> '') and (Categories.IndexOf(SubCat) < 0) then
            Categories.Add(SubCat);
        end;
      end;
    end;

    Writer := TSBWDataBlockWriter.Create;
    try
      Writer.WriteStringArray(Categories.ToArray);
      Result := Writer.ToBytes;
    finally
      Writer.Free;
    end;
  finally
    Categories.Free;
  end;
end;

procedure TSBWSystemServiceHandler.Release(ModuleID: SBWModuleID);
begin
  // Disconnect broker from module
  if ModuleID <> SBW_BROKER_MODULE_ID then
    (FBroker as TSBWBroker).RemoveModule(ModuleID);
end;

function TSBWSystemServiceHandler.GetServiceDescriptorByName(ModuleID: SBWModuleID;
  const ServiceName: string): TBytes;
var
  Writer: TSBWDataBlockWriter;
  Module: TSBWModuleInfo;
  Svc: TSBWBrokerServiceInfo;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    if (ModuleID = SBW_BROKER_MODULE_ID) and SameText(ServiceName, 'SYSTEM') then
    begin
      // Broker's SYSTEM service descriptor
      Writer.WriteListBegin(5);
      Writer.WriteString('BROKER');
      Writer.WriteString('SYSTEM');
      Writer.WriteString(BROKER_SERVICE_DISPLAY);
      Writer.WriteString(BROKER_SERVICE_CATEGORY);
      Writer.WriteString(BROKER_SERVICE_HELP);
    end
    else
    begin
      Module := (FBroker as TSBWBroker).GetModule(ModuleID);
      if Module <> nil then
      begin
        Svc := Module.FindServiceByName(ServiceName);
        if Svc <> nil then
          WriteServiceDescriptor(Writer, Module, Svc)
        else
          Writer.WriteListBegin(0);
      end
      else
        Writer.WriteListBegin(0);
    end;
    Result := Writer.ToBytes;
  finally
    Writer.Free;
  end;
end;

function TSBWSystemServiceHandler.GetServiceDescriptorById(ModuleID: SBWModuleID;
  ServiceID: SBWServiceID): TBytes;
var
  Writer: TSBWDataBlockWriter;
  Module: TSBWModuleInfo;
  Svc: TSBWBrokerServiceInfo;
begin
  Writer := TSBWDataBlockWriter.Create;
  try
    if (ModuleID = SBW_BROKER_MODULE_ID) and (ServiceID = BROKER_SYSTEM_SERVICE_ID) then
    begin
      Writer.WriteListBegin(5);
      Writer.WriteString('BROKER');
      Writer.WriteString('SYSTEM');
      Writer.WriteString(BROKER_SERVICE_DISPLAY);
      Writer.WriteString(BROKER_SERVICE_CATEGORY);
      Writer.WriteString(BROKER_SERVICE_HELP);
    end
    else
    begin
      Module := (FBroker as TSBWBroker).GetModule(ModuleID);
      if Module <> nil then
      begin
        Svc := Module.FindServiceByID(ServiceID);
        if Svc <> nil then
          WriteServiceDescriptor(Writer, Module, Svc)
        else
          Writer.WriteListBegin(0);
      end
      else
        Writer.WriteListBegin(0);
    end;
    Result := Writer.ToBytes;
  finally
    Writer.Free;
  end;
end;

function TSBWSystemServiceHandler.GetServiceDescriptors(ModuleID: SBWModuleID): TBytes;
var
  Writer: TSBWDataBlockWriter;
  Module: TSBWModuleInfo;
  Svc: TSBWBrokerServiceInfo;
  I, TotalCount: Integer;
  ItemWriter: TSBWDataBlockWriter;
  ItemBytes: TBytes;
  AllItems: TList<TBytes>;
begin
  AllItems := TList<TBytes>.Create;
  try
    if ModuleID = SBW_BROKER_MODULE_ID then
    begin
      // Broker has one service: SYSTEM
      ItemWriter := TSBWDataBlockWriter.Create;
      try
        ItemWriter.WriteListBegin(5);
        ItemWriter.WriteString('BROKER');
        ItemWriter.WriteString('SYSTEM');
        ItemWriter.WriteString(BROKER_SERVICE_DISPLAY);
        ItemWriter.WriteString(BROKER_SERVICE_CATEGORY);
        ItemWriter.WriteString(BROKER_SERVICE_HELP);
        AllItems.Add(ItemWriter.ToBytes);
      finally
        ItemWriter.Free;
      end;
    end
    else
    begin
      Module := (FBroker as TSBWBroker).GetModule(ModuleID);
      if Module <> nil then
      begin
        for Svc in Module.Services do
        begin
          ItemWriter := TSBWDataBlockWriter.Create;
          try
            WriteServiceDescriptor(ItemWriter, Module, Svc);
            AllItems.Add(ItemWriter.ToBytes);
          finally
            ItemWriter.Free;
          end;
        end;
      end;
    end;

    Writer := TSBWDataBlockWriter.Create;
    try
      var TypeByte: Byte := Ord(dbtArray);
      var ElemTypeByte: Byte := Ord(dbtList);
      var Dims: Int32 := 1;
      Writer.Stream.WriteBuffer(TypeByte, 1);
      Writer.Stream.WriteBuffer(ElemTypeByte, 1);
      Writer.Stream.WriteBuffer(Dims, SizeOf(Int32));
      TotalCount := AllItems.Count;
      Writer.Stream.WriteBuffer(TotalCount, SizeOf(Int32));

      for I := 0 to AllItems.Count - 1 do
      begin
        ItemBytes := AllItems[I];
        Writer.Stream.WriteBuffer(ItemBytes[0], Length(ItemBytes));
      end;

      Result := Writer.ToBytes;
    finally
      Writer.Free;
    end;
  finally
    AllItems.Free;
  end;
end;

function TSBWSystemServiceHandler.GetExistingModuleInstanceIds: TBytes;
var
  Writer: TSBWDataBlockWriter;
  ModuleList: TArray<TSBWModuleInfo>;
  I: Integer;
begin
  ModuleList := (FBroker as TSBWBroker).GetAllModules;

  Writer := TSBWDataBlockWriter.Create;
  try
    // Write as array of integers
    Writer.WriteTypeByte(dbtArray);
    Writer.WriteRawByte(Ord(dbtInteger));  // Element type
    Writer.WriteRawInt32(1);                // 1 dimension
    Writer.WriteRawInt32(Length(ModuleList)); // Count

    for I := 0 to High(ModuleList) do
      Writer.WriteRawInt32(ModuleList[I].ModuleID);

    Result := Writer.ToBytes;
  finally
    Writer.Free;
  end;
end;

procedure TSBWSystemServiceHandler.UnregisterModule(const ModuleName: string);
begin
  (FBroker as TSBWBroker).Registry.UnregisterModule(ModuleName);
end;

procedure TSBWSystemServiceHandler.RegisterModule(const Name, DisplayName: string;
  ModuleType: Integer; const CommandLine, Help: string);
var
  MgmtType: TSBWModuleManagementType;
begin
  // Convert integer to management type
  case ModuleType of
    0: MgmtType := mmtUnique;
    1: MgmtType := mmtSelfManaged;
  else
    MgmtType := mmtSelfManaged;
  end;

  (FBroker as TSBWBroker).Registry.RegisterModule(
    Name, DisplayName, MgmtType, CommandLine, Help);
end;

procedure TSBWSystemServiceHandler.RegisterServiceInRegistry(const ModuleName,
  ServiceName, DisplayName, Category, Help: string);
begin
  (FBroker as TSBWBroker).Registry.RegisterService(
    ModuleName, ServiceName, DisplayName, Category, Help);
end;

procedure TSBWSystemServiceHandler.ShutdownBroker;
begin
  // Signal broker to stop - this will be handled asynchronously
  (FBroker as TSBWBroker).Stop;
end;

function TSBWSystemServiceHandler.GetServiceDescriptorsByName(const ModuleName: string): TBytes;
var
  Module: TSBWModuleInfo;
begin
  if SameText(ModuleName, 'BROKER') then
    Result := GetServiceDescriptors(SBW_BROKER_MODULE_ID)
  else
  begin
    Module := (FBroker as TSBWBroker).GetModuleByName(ModuleName);
    if Module <> nil then
      Result := GetServiceDescriptors(Module.ModuleID)
    else
    begin
      // Return empty array
      var Writer := TSBWDataBlockWriter.Create;
      try
        var TypeByte: Byte := Ord(dbtArray);
        var ElemTypeByte: Byte := Ord(dbtList);
        var Dims: Int32 := 1;
        var Count: Int32 := 0;
        Writer.Stream.WriteBuffer(TypeByte, 1);
        Writer.Stream.WriteBuffer(ElemTypeByte, 1);
        Writer.Stream.WriteBuffer(Dims, SizeOf(Int32));
        Writer.Stream.WriteBuffer(Count, SizeOf(Int32));
        Result := Writer.ToBytes;
      finally
        Writer.Free;
      end;
    end;
  end;
end;


procedure TSBWSystemServiceHandler.HandleRegisterServices(const CallMsg: TSBWCallMessage;
  SourceModule: TSBWModuleInfo);
var
  Reader: TSBWDataBlockReader;
  ServiceCount, MethodCount: Integer;
  I, J: Integer;
  SvcInfo: TSBWBrokerServiceInfo;
  MethInfo: TSBWBrokerMethodInfo;
  ReplyData: TBytes;
  Writer: TSBWDataBlockWriter;
  // For registry cache update
  RegServices: TArray<TSBWRegisteredService>;
  RegService: TSBWRegisteredService;
  ShouldCache: Boolean;
begin
  Reader := TSBWDataBlockReader.Create(CallMsg.Payload);
  try
    ServiceCount := Reader.ReadInteger;

    // Check if this module is already registered for auto-launch
    ShouldCache := (FBroker as TSBWBroker).Registry.IsRegistered(SourceModule.ModuleName);

    // Clear existing services and register new ones
    SourceModule.Services.Clear;

    // Prepare array for registry cache (only if registered)
    if ShouldCache then
      SetLength(RegServices, ServiceCount);

    for I := 0 to ServiceCount - 1 do
    begin
      SvcInfo := TSBWBrokerServiceInfo.Create;
      SvcInfo.ServiceID := Reader.ReadInteger;
      SvcInfo.Name := Reader.ReadString;
      SvcInfo.DisplayName := Reader.ReadString;
      SvcInfo.Category := Reader.ReadString;
      SvcInfo.Help := Reader.ReadString;

      // Create corresponding registry service (only if registered)
      if ShouldCache then
      begin
        RegService := TSBWRegisteredService.Create;
        RegService.ServiceName := SvcInfo.Name;
        RegService.DisplayName := SvcInfo.DisplayName;
        RegService.Category := SvcInfo.Category;
        RegService.Help := SvcInfo.Help;
        RegServices[I] := RegService;
      end;

      MethodCount := Reader.ReadInteger;
      for J := 0 to MethodCount - 1 do
      begin
        MethInfo := TSBWBrokerMethodInfo.Create;
        MethInfo.MethodID := Reader.ReadInteger;
        MethInfo.Name := Reader.ReadString;
        MethInfo.Signature := Reader.ReadString;
        MethInfo.Help := Reader.ReadString;
        SvcInfo.Methods.Add(MethInfo);

        // Add method to registry service (only if registered)
        if ShouldCache then
          RegService.AddMethod(MethInfo.Signature, MethInfo.Help);
      end;

      SourceModule.Services.Add(SvcInfo);
    end;

    // Update registry cache only for registered modules
    if ShouldCache then
    begin
      (FBroker as TSBWBroker).Registry.UpdateFromModuleRegistration(
        SourceModule.ModuleName, RegServices);

      // Free temporary registry service objects
      for I := 0 to High(RegServices) do
        RegServices[I].Free;
    end;

    Writer := TSBWDataBlockWriter.Create;
    try
      Writer.WriteBoolean(True);
      ReplyData := TSBWMessageWriter.BuildReplyMessage(
        SourceModule.ModuleID, CallMsg.UID, Writer.ToBytes);
    finally
      Writer.Free;
    end;
    SourceModule.Connection.SendMessage(ReplyData);

  finally
    Reader.Free;
  end;
end;

end.
