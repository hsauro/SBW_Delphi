unit SBW.Broker.Registry;

(******************************************************************************
 * SBW.Broker.Registry.pas
 *
 * Module Registry for the SBW Broker.
 *
 * Stores information about registered modules that can be auto-launched,
 * including their complete interface description (services and methods).
 *
 * The registry can be updated in two ways:
 * 1. Explicit registration via RegisterModule/RegisterService/RegisterMethod
 * 2. Auto-caching when a module connects and registers its services
 *
 * Registry structure:
 *   Module
 *     ├── Name, DisplayName, ModuleType, CommandLine, Help
 *     └── Services[]
 *           ├── ServiceName, DisplayName, Category, Help
 *           └── Methods[]
 *                 ├── Signature (e.g., "double sin(double)")
 *                 └── Help
 *
 * JSON format:
 * {
 *   "version": 2,
 *   "modules": [
 *     {
 *       "name": "edu.demo.mathserver",
 *       "displayName": "Math Server",
 *       "moduleType": 2,
 *       "commandLine": "C:\\Path\\To\\MathServer.exe",
 *       "help": "A math computation server",
 *       "services": [
 *         {
 *           "serviceName": "math",
 *           "displayName": "Math Service",
 *           "category": "Math",
 *           "help": "Basic math operations",
 *           "methods": [
 *             {
 *               "signature": "double sin(double)",
 *               "help": "Returns the sine of x"
 *             }
 *           ]
 *         }
 *       ]
 *     }
 *   ]
 * }
 *****************************************************************************)

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Generics.Collections,
  System.JSON, System.IOUtils,
  SBW.Types;

type
  /// <summary>
  /// Registered method information
  /// </summary>
  TSBWRegisteredMethod = class
  private
    FSignature: string;
    FHelp: string;
  public
    property Signature: string read FSignature write FSignature;
    property Help: string read FHelp write FHelp;

    /// <summary>
    /// Extract method name from signature (e.g., "sin" from "double sin(double)")
    /// </summary>
    function GetName: string;

    function ToJSON: TJSONObject;
    procedure FromJSON(const AJSON: TJSONObject);
  end;

  /// <summary>
  /// Registered service information
  /// </summary>
  TSBWRegisteredService = class
  private
    FServiceName: string;
    FDisplayName: string;
    FCategory: string;
    FHelp: string;
    FMethods: TObjectList<TSBWRegisteredMethod>;
  public
    constructor Create;
    destructor Destroy; override;

    function FindMethod(const MethodName: string): TSBWRegisteredMethod;
    function FindMethodBySignature(const Signature: string): TSBWRegisteredMethod;
    procedure AddMethod(const Signature, Help: string);
    procedure RemoveMethod(const Signature: string);
    procedure ClearMethods;

    property ServiceName: string read FServiceName write FServiceName;
    property DisplayName: string read FDisplayName write FDisplayName;
    property Category: string read FCategory write FCategory;
    property Help: string read FHelp write FHelp;
    property Methods: TObjectList<TSBWRegisteredMethod> read FMethods;

    function ToJSON: TJSONObject;
    procedure FromJSON(const AJSON: TJSONObject);
  end;

  /// <summary>
  /// Registered module information
  /// </summary>
  TSBWRegisteredModule = class
  private
    FName: string;
    FDisplayName: string;
    FModuleType: TSBWModuleManagementType;
    FCommandLine: string;
    FHelp: string;
    FServices: TObjectList<TSBWRegisteredService>;
  public
    constructor Create;
    destructor Destroy; override;

    function FindService(const ServiceName: string): TSBWRegisteredService;
    function AddService(const ServiceName, DisplayName, Category, Help: string): TSBWRegisteredService;
    procedure RemoveService(const ServiceName: string);
    procedure ClearServices;

    function ToJSON: TJSONObject;
    procedure FromJSON(const AJSON: TJSONObject);

    property Name: string read FName write FName;
    property DisplayName: string read FDisplayName write FDisplayName;
    property ModuleType: TSBWModuleManagementType read FModuleType write FModuleType;
    property CommandLine: string read FCommandLine write FCommandLine;
    property Help: string read FHelp write FHelp;
    property Services: TObjectList<TSBWRegisteredService> read FServices;
  end;

  /// <summary>
  /// Module registry - stores registered modules for discovery and auto-launch
  /// </summary>
  TSBWModuleRegistry = class
  private
    FModules: TObjectDictionary<string, TSBWRegisteredModule>;
    FLock: TCriticalSection;
    FFilePath: string;
    FAutoSave: Boolean;
    FDirty: Boolean;

    procedure MarkDirty;
    procedure AutoSaveIfNeeded;
  public
    constructor Create;
    destructor Destroy; override;

    //=========================================================================
    // Module Registration
    //=========================================================================

    /// <summary>
    /// Register a module for auto-launch.
    /// If the module already exists, updates the launch info but preserves services.
    /// </summary>
    procedure RegisterModule(const Name, DisplayName: string;
      ModuleType: TSBWModuleManagementType; const CommandLine, Help: string);

    /// <summary>
    /// Unregister a module completely (removes all services and methods)
    /// </summary>
    procedure UnregisterModule(const Name: string);

    /// <summary>
    /// Find a registered module by name
    /// </summary>
    function FindModule(const Name: string): TSBWRegisteredModule;

    /// <summary>
    /// Get all registered modules
    /// </summary>
    function GetAllModules: TArray<TSBWRegisteredModule>;

    /// <summary>
    /// Check if a module is registered
    /// </summary>
    function IsRegistered(const Name: string): Boolean;

    //=========================================================================
    // Service Registration
    //=========================================================================

    /// <summary>
    /// Register a service for a module.
    /// Creates the module if it doesn't exist (with empty launch info).
    /// Returns the service object for adding methods.
    /// </summary>
    function RegisterService(const ModuleName, ServiceName, DisplayName,
      Category, Help: string): TSBWRegisteredService;

    /// <summary>
    /// Find a service in a module
    /// </summary>
    function FindService(const ModuleName, ServiceName: string): TSBWRegisteredService;

    //=========================================================================
    // Method Registration
    //=========================================================================

    /// <summary>
    /// Register a method for a service.
    /// Creates the module and service if they don't exist.
    /// </summary>
    procedure RegisterMethod(const ModuleName, ServiceName, Signature, Help: string);

    /// <summary>
    /// Find a method in a service
    /// </summary>
    function FindMethod(const ModuleName, ServiceName, MethodName: string): TSBWRegisteredMethod;

    //=========================================================================
    // Cache Update (called when module connects and registers)
    //=========================================================================

    /// <summary>
    /// Update the registry cache with services/methods from a connected module.
    /// This is called by the broker when a module sends its service registration.
    /// Preserves the module's launch info while updating the service definitions.
    ///
    /// ServiceData format (matches RegisterServicesWithBroker):
    ///   - Each service: [ServiceID, Name, DisplayName, Category, Help, Methods[]]
    ///   - Each method: [MethodID, Name, Signature, Help]
    /// </summary>
    procedure UpdateFromModuleRegistration(const ModuleName: string;
      const Services: TArray<TSBWRegisteredService>);

    /// <summary>
    /// Clear all services for a module (before updating from live registration)
    /// </summary>
    procedure ClearModuleServices(const ModuleName: string);

    //=========================================================================
    // Module Launching
    //=========================================================================

    /// <summary>
    /// Launch a registered module.
    /// Returns True if launch was initiated successfully.
    /// </summary>
    function LaunchModule(const Name: string): Boolean;

    /// <summary>
    /// Get the command line for a registered module
    /// </summary>
    function GetCommandLine(const Name: string): string;

    //=========================================================================
    // Persistence Methods
    //=========================================================================

    /// <summary>
    /// Save the registry to a JSON file.
    /// Uses FilePath property if AFilePath is empty.
    /// </summary>
    procedure SaveToFile(const AFilePath: string = '');

    /// <summary>
    /// Load the registry from a JSON file.
    /// Uses FilePath property if AFilePath is empty.
    /// Clears existing registrations before loading.
    /// </summary>
    procedure LoadFromFile(const AFilePath: string = '');

    /// <summary>
    /// Load from file if it exists, otherwise start empty.
    /// Uses FilePath property.
    /// </summary>
    procedure LoadIfExists;

    /// <summary>
    /// Clear all registrations
    /// </summary>
    procedure Clear;

    /// <summary>
    /// Export registry to JSON string
    /// </summary>
    function ToJSON: string;

    /// <summary>
    /// Import registry from JSON string
    /// </summary>
    procedure FromJSON(const AJSONString: string);

    /// <summary>
    /// Get the default registry file path.
    /// Returns path in same directory as executable.
    /// </summary>
    class function GetDefaultFilePath: string;

    //=========================================================================
    // Properties
    //=========================================================================

    /// <summary>
    /// Path to the registry file for persistence.
    /// If empty, uses GetDefaultFilePath.
    /// </summary>
    property FilePath: string read FFilePath write FFilePath;

    /// <summary>
    /// If True, automatically saves to FilePath after each modification.
    /// Default is False.
    /// </summary>
    property AutoSave: Boolean read FAutoSave write FAutoSave;

    /// <summary>
    /// True if registry has unsaved changes
    /// </summary>
    property Dirty: Boolean read FDirty;

    /// <summary>
    /// Number of registered modules
    /// </summary>
    function Count: Integer;
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  Winapi.Windows, Winapi.ShellAPI;
{$ENDIF}

const
  REGISTRY_FORMAT_VERSION = 2;

//=============================================================================
// TSBWRegisteredMethod
//=============================================================================

function TSBWRegisteredMethod.GetName: string;
var
  OpenParen, SpacePos: Integer;
  BeforeParen: string;
begin
  // Signature format: "returnType methodName(args)"
  OpenParen := Pos('(', FSignature);
  if OpenParen = 0 then
    Exit(FSignature);

  BeforeParen := Trim(Copy(FSignature, 1, OpenParen - 1));
  SpacePos := LastDelimiter(' ', BeforeParen);
  if SpacePos > 0 then
    Result := Copy(BeforeParen, SpacePos + 1, Length(BeforeParen))
  else
    Result := BeforeParen;
end;

function TSBWRegisteredMethod.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('signature', FSignature);
  Result.AddPair('help', FHelp);
end;

procedure TSBWRegisteredMethod.FromJSON(const AJSON: TJSONObject);
begin
  FSignature := AJSON.GetValue<string>('signature', '');
  FHelp := AJSON.GetValue<string>('help', '');
end;

//=============================================================================
// TSBWRegisteredService
//=============================================================================

constructor TSBWRegisteredService.Create;
begin
  inherited Create;
  FMethods := TObjectList<TSBWRegisteredMethod>.Create(True);
end;

destructor TSBWRegisteredService.Destroy;
begin
  FMethods.Free;
  inherited;
end;

function TSBWRegisteredService.FindMethod(const MethodName: string): TSBWRegisteredMethod;
var
  Method: TSBWRegisteredMethod;
begin
  for Method in FMethods do
    if SameText(Method.GetName, MethodName) then
      Exit(Method);
  Result := nil;
end;

function TSBWRegisteredService.FindMethodBySignature(const Signature: string): TSBWRegisteredMethod;
var
  Method: TSBWRegisteredMethod;
begin
  for Method in FMethods do
    if SameText(Method.Signature, Signature) then
      Exit(Method);
  Result := nil;
end;

procedure TSBWRegisteredService.AddMethod(const Signature, Help: string);
var
  Method: TSBWRegisteredMethod;
begin
  // Remove existing if present (update)
  RemoveMethod(Signature);

  Method := TSBWRegisteredMethod.Create;
  Method.Signature := Signature;
  Method.Help := Help;
  FMethods.Add(Method);
end;

procedure TSBWRegisteredService.RemoveMethod(const Signature: string);
var
  I: Integer;
begin
  for I := FMethods.Count - 1 downto 0 do
    if SameText(FMethods[I].Signature, Signature) then
    begin
      FMethods.Delete(I);
      Exit;
    end;
end;

procedure TSBWRegisteredService.ClearMethods;
begin
  FMethods.Clear;
end;

function TSBWRegisteredService.ToJSON: TJSONObject;
var
  MethodsArray: TJSONArray;
  Method: TSBWRegisteredMethod;
begin
  Result := TJSONObject.Create;
  Result.AddPair('serviceName', FServiceName);
  Result.AddPair('displayName', FDisplayName);
  Result.AddPair('category', FCategory);
  Result.AddPair('help', FHelp);

  MethodsArray := TJSONArray.Create;
  for Method in FMethods do
    MethodsArray.AddElement(Method.ToJSON);
  Result.AddPair('methods', MethodsArray);
end;

procedure TSBWRegisteredService.FromJSON(const AJSON: TJSONObject);
var
  MethodsArray: TJSONArray;
  I: Integer;
  Method: TSBWRegisteredMethod;
  MethodJSON: TJSONObject;
begin
  FServiceName := AJSON.GetValue<string>('serviceName', '');
  FDisplayName := AJSON.GetValue<string>('displayName', '');
  FCategory := AJSON.GetValue<string>('category', '');
  FHelp := AJSON.GetValue<string>('help', '');

  FMethods.Clear;
  if AJSON.TryGetValue<TJSONArray>('methods', MethodsArray) then
  begin
    for I := 0 to MethodsArray.Count - 1 do
    begin
      MethodJSON := MethodsArray.Items[I] as TJSONObject;
      Method := TSBWRegisteredMethod.Create;
      Method.FromJSON(MethodJSON);
      FMethods.Add(Method);
    end;
  end;
end;

//=============================================================================
// TSBWRegisteredModule
//=============================================================================

constructor TSBWRegisteredModule.Create;
begin
  inherited Create;
  FServices := TObjectList<TSBWRegisteredService>.Create(True);
end;

destructor TSBWRegisteredModule.Destroy;
begin
  FServices.Free;
  inherited;
end;

function TSBWRegisteredModule.FindService(const ServiceName: string): TSBWRegisteredService;
var
  Svc: TSBWRegisteredService;
begin
  for Svc in FServices do
    if SameText(Svc.ServiceName, ServiceName) then
      Exit(Svc);
  Result := nil;
end;

function TSBWRegisteredModule.AddService(const ServiceName, DisplayName,
  Category, Help: string): TSBWRegisteredService;
begin
  // Remove existing if present
  RemoveService(ServiceName);

  Result := TSBWRegisteredService.Create;
  Result.ServiceName := ServiceName;
  Result.DisplayName := DisplayName;
  Result.Category := Category;
  Result.Help := Help;
  FServices.Add(Result);
end;

procedure TSBWRegisteredModule.RemoveService(const ServiceName: string);
var
  I: Integer;
begin
  for I := FServices.Count - 1 downto 0 do
    if SameText(FServices[I].ServiceName, ServiceName) then
    begin
      FServices.Delete(I);
      Exit;
    end;
end;

procedure TSBWRegisteredModule.ClearServices;
begin
  FServices.Clear;
end;

function TSBWRegisteredModule.ToJSON: TJSONObject;
var
  ServicesArray: TJSONArray;
  Svc: TSBWRegisteredService;
begin
  Result := TJSONObject.Create;
  Result.AddPair('name', FName);
  Result.AddPair('displayName', FDisplayName);
  Result.AddPair('moduleType', TJSONNumber.Create(Ord(FModuleType)));
  Result.AddPair('commandLine', FCommandLine);
  Result.AddPair('help', FHelp);

  ServicesArray := TJSONArray.Create;
  for Svc in FServices do
    ServicesArray.AddElement(Svc.ToJSON);
  Result.AddPair('services', ServicesArray);
end;

procedure TSBWRegisteredModule.FromJSON(const AJSON: TJSONObject);
var
  ServicesArray: TJSONArray;
  I: Integer;
  Svc: TSBWRegisteredService;
  SvcJSON: TJSONObject;
begin
  FName := AJSON.GetValue<string>('name', '');
  FDisplayName := AJSON.GetValue<string>('displayName', '');
  FModuleType := TSBWModuleManagementType(AJSON.GetValue<Integer>('moduleType', 0));
  FCommandLine := AJSON.GetValue<string>('commandLine', '');
  FHelp := AJSON.GetValue<string>('help', '');

  FServices.Clear;
  if AJSON.TryGetValue<TJSONArray>('services', ServicesArray) then
  begin
    for I := 0 to ServicesArray.Count - 1 do
    begin
      SvcJSON := ServicesArray.Items[I] as TJSONObject;
      Svc := TSBWRegisteredService.Create;
      Svc.FromJSON(SvcJSON);
      FServices.Add(Svc);
    end;
  end;
end;

//=============================================================================
// TSBWModuleRegistry
//=============================================================================

constructor TSBWModuleRegistry.Create;
begin
  inherited Create;
  FModules := TObjectDictionary<string, TSBWRegisteredModule>.Create([doOwnsValues]);
  FLock := TCriticalSection.Create;
  FFilePath := '';
  FAutoSave := False;
  FDirty := False;
end;

destructor TSBWModuleRegistry.Destroy;
begin
  FModules.Free;
  FLock.Free;
  inherited;
end;

procedure TSBWModuleRegistry.MarkDirty;
begin
  FDirty := True;
  AutoSaveIfNeeded;
end;

procedure TSBWModuleRegistry.AutoSaveIfNeeded;
begin
  if FAutoSave and FDirty then
    SaveToFile;
end;

class function TSBWModuleRegistry.GetDefaultFilePath: string;
begin
  Result := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'sbw_registry.json');
end;

function TSBWModuleRegistry.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FModules.Count;
  finally
    FLock.Leave;
  end;
end;

//=============================================================================
// Module Registration
//=============================================================================

procedure TSBWModuleRegistry.RegisterModule(const Name, DisplayName: string;
  ModuleType: TSBWModuleManagementType; const CommandLine, Help: string);
var
  Module: TSBWRegisteredModule;
  LowerName: string;
begin
  LowerName := LowerCase(Name);

  FLock.Enter;
  try
    // Check if module already exists
    if FModules.TryGetValue(LowerName, Module) then
    begin
      // Update launch info, preserve services
      Module.DisplayName := DisplayName;
      Module.ModuleType := ModuleType;
      Module.CommandLine := CommandLine;
      Module.Help := Help;
    end
    else
    begin
      // Create new module
      Module := TSBWRegisteredModule.Create;
      Module.Name := Name;
      Module.DisplayName := DisplayName;
      Module.ModuleType := ModuleType;
      Module.CommandLine := CommandLine;
      Module.Help := Help;
      FModules.Add(LowerName, Module);
    end;
    MarkDirty;
  finally
    FLock.Leave;
  end;
end;

procedure TSBWModuleRegistry.UnregisterModule(const Name: string);
var
  LowerName: string;
begin
  LowerName := LowerCase(Name);

  FLock.Enter;
  try
    if FModules.ContainsKey(LowerName) then
    begin
      FModules.Remove(LowerName);
      MarkDirty;
    end;
  finally
    FLock.Leave;
  end;
end;

function TSBWModuleRegistry.FindModule(const Name: string): TSBWRegisteredModule;
var
  LowerName: string;
begin
  LowerName := LowerCase(Name);

  FLock.Enter;
  try
    if not FModules.TryGetValue(LowerName, Result) then
      Result := nil;
  finally
    FLock.Leave;
  end;
end;

function TSBWModuleRegistry.GetAllModules: TArray<TSBWRegisteredModule>;
begin
  FLock.Enter;
  try
    Result := FModules.Values.ToArray;
  finally
    FLock.Leave;
  end;
end;

function TSBWModuleRegistry.IsRegistered(const Name: string): Boolean;
begin
  Result := FindModule(Name) <> nil;
end;

//=============================================================================
// Service Registration
//=============================================================================

function TSBWModuleRegistry.RegisterService(const ModuleName, ServiceName,
  DisplayName, Category, Help: string): TSBWRegisteredService;
var
  Module: TSBWRegisteredModule;
  LowerName: string;
begin
  LowerName := LowerCase(ModuleName);

  FLock.Enter;
  try
    // Find or create module
    if not FModules.TryGetValue(LowerName, Module) then
    begin
      Module := TSBWRegisteredModule.Create;
      Module.Name := ModuleName;
      FModules.Add(LowerName, Module);
    end;

    Result := Module.AddService(ServiceName, DisplayName, Category, Help);
    MarkDirty;
  finally
    FLock.Leave;
  end;
end;

function TSBWModuleRegistry.FindService(const ModuleName, ServiceName: string): TSBWRegisteredService;
var
  Module: TSBWRegisteredModule;
begin
  Module := FindModule(ModuleName);
  if Module <> nil then
    Result := Module.FindService(ServiceName)
  else
    Result := nil;
end;

//=============================================================================
// Method Registration
//=============================================================================

procedure TSBWModuleRegistry.RegisterMethod(const ModuleName, ServiceName,
  Signature, Help: string);
var
  Service: TSBWRegisteredService;
begin
  FLock.Enter;
  try
    // Find or create service (which also finds or creates module)
    Service := FindService(ModuleName, ServiceName);
    if Service = nil then
      Service := RegisterService(ModuleName, ServiceName, ServiceName, '', '');

    Service.AddMethod(Signature, Help);
    MarkDirty;
  finally
    FLock.Leave;
  end;
end;

function TSBWModuleRegistry.FindMethod(const ModuleName, ServiceName,
  MethodName: string): TSBWRegisteredMethod;
var
  Service: TSBWRegisteredService;
begin
  Service := FindService(ModuleName, ServiceName);
  if Service <> nil then
    Result := Service.FindMethod(MethodName)
  else
    Result := nil;
end;

//=============================================================================
// Cache Update
//=============================================================================

procedure TSBWModuleRegistry.ClearModuleServices(const ModuleName: string);
var
  Module: TSBWRegisteredModule;
begin
  FLock.Enter;
  try
    Module := FindModule(ModuleName);
    if Module <> nil then
    begin
      Module.ClearServices;
      MarkDirty;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TSBWModuleRegistry.UpdateFromModuleRegistration(const ModuleName: string;
  const Services: TArray<TSBWRegisteredService>);
var
  Module: TSBWRegisteredModule;
  LowerName: string;
  SrcService: TSBWRegisteredService;
  DstService: TSBWRegisteredService;
  SrcMethod: TSBWRegisteredMethod;
begin
  LowerName := LowerCase(ModuleName);

  FLock.Enter;
  try
    // Find or create module
    if not FModules.TryGetValue(LowerName, Module) then
    begin
      Module := TSBWRegisteredModule.Create;
      Module.Name := ModuleName;
      FModules.Add(LowerName, Module);
    end;

    // Clear existing services and replace with new ones
    Module.ClearServices;

    for SrcService in Services do
    begin
      DstService := Module.AddService(
        SrcService.ServiceName,
        SrcService.DisplayName,
        SrcService.Category,
        SrcService.Help);

      for SrcMethod in SrcService.Methods do
        DstService.AddMethod(SrcMethod.Signature, SrcMethod.Help);
    end;

    MarkDirty;
  finally
    FLock.Leave;
  end;
end;

//=============================================================================
// Module Launching
//=============================================================================

function TSBWModuleRegistry.GetCommandLine(const Name: string): string;
var
  Module: TSBWRegisteredModule;
begin
  Module := FindModule(Name);
  if Module <> nil then
    Result := Module.CommandLine
  else
    Result := '';
end;

function TSBWModuleRegistry.LaunchModule(const Name: string): Boolean;
var
  Module: TSBWRegisteredModule;
  CmdLine: string;
  {$IFDEF MSWINDOWS}
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  {$ENDIF}
begin
  Result := False;

  Module := FindModule(Name);
  if Module = nil then
    Exit;

  CmdLine := Module.CommandLine;
  if CmdLine = '' then
    Exit;

  {$IFDEF MSWINDOWS}
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_SHOWNORMAL;  // Show normally for GUI apps

  FillChar(ProcessInfo, SizeOf(ProcessInfo), 0);

  Result := CreateProcess(
    nil,
    PChar(CmdLine),
    nil,
    nil,
    False,
    CREATE_NEW_CONSOLE,
    nil,
    nil,
    StartupInfo,
    ProcessInfo
  );

  if Result then
  begin
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
  end;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

//=============================================================================
// Persistence Methods
//=============================================================================

function TSBWModuleRegistry.ToJSON: string;
var
  Root: TJSONObject;
  ModulesArray: TJSONArray;
  Module: TSBWRegisteredModule;
begin
  Root := TJSONObject.Create;
  try
    Root.AddPair('version', TJSONNumber.Create(REGISTRY_FORMAT_VERSION));

    ModulesArray := TJSONArray.Create;
    FLock.Enter;
    try
      for Module in FModules.Values do
        ModulesArray.AddElement(Module.ToJSON);
    finally
      FLock.Leave;
    end;

    Root.AddPair('modules', ModulesArray);
    Result := Root.Format(2);
  finally
    Root.Free;
  end;
end;

procedure TSBWModuleRegistry.FromJSON(const AJSONString: string);
var
  Root: TJSONObject;
  ModulesArray: TJSONArray;
  I: Integer;
  Module: TSBWRegisteredModule;
  ModuleJSON: TJSONObject;
  Version: Integer;
begin
  Root := TJSONObject.ParseJSONValue(AJSONString) as TJSONObject;
  if Root = nil then
    raise Exception.Create('Invalid JSON format');

  try
    Version := Root.GetValue<Integer>('version', 1);
    if Version > REGISTRY_FORMAT_VERSION then
      raise Exception.CreateFmt('Registry file version %d is newer than supported version %d',
        [Version, REGISTRY_FORMAT_VERSION]);

    FLock.Enter;
    try
      FModules.Clear;

      if Root.TryGetValue<TJSONArray>('modules', ModulesArray) then
      begin
        for I := 0 to ModulesArray.Count - 1 do
        begin
          ModuleJSON := ModulesArray.Items[I] as TJSONObject;
          Module := TSBWRegisteredModule.Create;
          try
            Module.FromJSON(ModuleJSON);
            FModules.Add(LowerCase(Module.Name), Module);
          except
            Module.Free;
            raise;
          end;
        end;
      end;

      FDirty := False;
    finally
      FLock.Leave;
    end;
  finally
    Root.Free;
  end;
end;

procedure TSBWModuleRegistry.SaveToFile(const AFilePath: string);
var
  Path: string;
  JSONContent: string;
begin
  if AFilePath <> '' then
    Path := AFilePath
  else if FFilePath <> '' then
    Path := FFilePath
  else
    Path := GetDefaultFilePath;

  JSONContent := ToJSON;
  TFile.WriteAllText(Path, JSONContent, TEncoding.UTF8);
  FDirty := False;
end;

procedure TSBWModuleRegistry.LoadFromFile(const AFilePath: string);
var
  Path: string;
  JSONContent: string;
begin
  if AFilePath <> '' then
    Path := AFilePath
  else if FFilePath <> '' then
    Path := FFilePath
  else
    Path := GetDefaultFilePath;

  if not TFile.Exists(Path) then
    raise Exception.CreateFmt('Registry file not found: %s', [Path]);

  JSONContent := TFile.ReadAllText(Path, TEncoding.UTF8);
  FromJSON(JSONContent);
end;

procedure TSBWModuleRegistry.LoadIfExists;
var
  Path: string;
begin
  if FFilePath <> '' then
    Path := FFilePath
  else
    Path := GetDefaultFilePath;

  if TFile.Exists(Path) then
    LoadFromFile(Path)
  else
    Clear;
end;

procedure TSBWModuleRegistry.Clear;
begin
  FLock.Enter;
  try
    FModules.Clear;
    FDirty := True;
  finally
    FLock.Leave;
  end;
end;

end.
