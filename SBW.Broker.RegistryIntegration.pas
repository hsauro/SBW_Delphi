unit SBW.Broker.RegistryIntegration;

{******************************************************************************
 * SBW.Broker.RegistryIntegration.pas
 *
 * Example code showing how to integrate the registry cache update
 * with the broker's service registration handler.
 *
 * When a module connects and calls SYS_METHOD_REGISTER_SERVICES (method 100),
 * the broker should:
 *   1. Store the services in the live TSBWModuleInfo (for runtime queries)
 *   2. Update the registry cache (for persistence and offline discovery)
 *
 * This file shows the pattern - integrate into your SBW.Broker.SystemService.pas
 *****************************************************************************}

interface

uses
  System.SysUtils, System.Generics.Collections,
  SBW.Types, SBW.DataBlock, SBW.Broker.Registry;

/// <summary>
/// Parse service registration data and update the registry cache.
/// Call this from your SYS_METHOD_REGISTER_SERVICES handler.
/// </summary>
procedure UpdateRegistryFromServiceData(
  Registry: TSBWModuleRegistry;
  const ModuleName: string;
  Reader: TSBWDataBlockReader);

/// <summary>
/// Build service array from registration data for UpdateFromModuleRegistration
/// </summary>
function ParseServiceRegistration(Reader: TSBWDataBlockReader): TArray<TSBWRegisteredService>;

implementation

function ParseServiceRegistration(Reader: TSBWDataBlockReader): TArray<TSBWRegisteredService>;
var
  ServiceCount, MethodCount: Integer;
  I, J: Integer;
  Service: TSBWRegisteredService;
  ServiceID, MethodID: Integer;
  ServiceName, ServiceDisplayName, ServiceCategory, ServiceHelp: string;
  MethodName, MethodSignature, MethodHelp: string;
begin
  // Read service count
  ServiceCount := Reader.ReadInteger;
  SetLength(Result, ServiceCount);

  for I := 0 to ServiceCount - 1 do
  begin
    // Read service info
    ServiceID := Reader.ReadInteger;        // We don't store the ID in registry
    ServiceName := Reader.ReadString;
    ServiceDisplayName := Reader.ReadString;
    ServiceCategory := Reader.ReadString;
    ServiceHelp := Reader.ReadString;

    Service := TSBWRegisteredService.Create;
    Service.ServiceName := ServiceName;
    Service.DisplayName := ServiceDisplayName;
    Service.Category := ServiceCategory;
    Service.Help := ServiceHelp;

    // Read methods
    MethodCount := Reader.ReadInteger;
    for J := 0 to MethodCount - 1 do
    begin
      MethodID := Reader.ReadInteger;       // We don't store the ID in registry
      MethodName := Reader.ReadString;      // This is just the name
      MethodSignature := Reader.ReadString; // Full signature like "double sin(double)"
      MethodHelp := Reader.ReadString;

      Service.AddMethod(MethodSignature, MethodHelp);
    end;

    Result[I] := Service;
  end;
end;

procedure UpdateRegistryFromServiceData(
  Registry: TSBWModuleRegistry;
  const ModuleName: string;
  Reader: TSBWDataBlockReader);
var
  Services: TArray<TSBWRegisteredService>;
  I: Integer;
begin
  Services := ParseServiceRegistration(Reader);
  try
    Registry.UpdateFromModuleRegistration(ModuleName, Services);
  finally
    // Free the temporary service objects
    for I := 0 to High(Services) do
      Services[I].Free;
  end;
end;

end.

{******************************************************************************
 * INTEGRATION EXAMPLE
 *
 * In your SBW.Broker.SystemService.pas, modify the handler for method 100:
 *
 * procedure TSBWSystemService.HandleRegisterServices(ClientThread: TSBWClientThread;
 *   const CallMsg: TSBWCallMessage);
 * var
 *   Reader: TSBWDataBlockReader;
 *   // ... existing variables ...
 * begin
 *   Reader := TSBWDataBlockReader.Create(CallMsg.Payload);
 *   try
 *     // === EXISTING CODE: Store in live module info ===
 *     // ... your existing service registration code ...
 *
 *     // === NEW: Update registry cache ===
 *     // Reset reader position to re-read the data
 *     Reader.Free;
 *     Reader := TSBWDataBlockReader.Create(CallMsg.Payload);
 *     UpdateRegistryFromServiceData(FBroker.Registry, ClientThread.ModuleName, Reader);
 *
 *   finally
 *     Reader.Free;
 *   end;
 *
 *   // Send success reply
 *   // ...
 * end;
 *
 *****************************************************************************}

{******************************************************************************
 * BROKER INITIALIZATION EXAMPLE
 *
 * In your TSBWBroker.Create:
 *
 *   FRegistry := TSBWModuleRegistry.Create;
 *   FRegistry.AutoSave := True;  // Auto-persist changes
 *   FRegistry.LoadIfExists;      // Load cached data from previous runs
 *
 * In your TSBWBroker.Destroy:
 *
 *   if FRegistry.Dirty then
 *     FRegistry.SaveToFile;
 *   FRegistry.Free;
 *
 *****************************************************************************}

{******************************************************************************
 * EXAMPLE JSON OUTPUT
 *
 * After a math server connects and registers, the registry will contain:
 *
 * {
 *   "version": 2,
 *   "modules": [
 *     {
 *       "name": "edu.demo.mathserver",
 *       "displayName": "Math Server",
 *       "moduleType": 2,
 *       "commandLine": "C:\\SBW\\Modules\\MathServer.exe",
 *       "help": "Provides basic math operations",
 *       "services": [
 *         {
 *           "serviceName": "math",
 *           "displayName": "Math Service",
 *           "category": "Math",
 *           "help": "Basic math operations",
 *           "methods": [
 *             {
 *               "signature": "double sin(double)",
 *               "help": "Returns the sine of x (in radians)"
 *             },
 *             {
 *               "signature": "double cos(double)",
 *               "help": "Returns the cosine of x (in radians)"
 *             },
 *             {
 *               "signature": "double add(double, double)",
 *               "help": "Returns the sum of two numbers"
 *             },
 *             {
 *               "signature": "int sum(int[])",
 *               "help": "Returns the sum of an array of integers"
 *             }
 *           ]
 *         }
 *       ]
 *     }
 *   ]
 * }
 *
 *****************************************************************************}
