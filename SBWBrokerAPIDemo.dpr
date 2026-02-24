program SBWBrokerAPIDemo;

{$APPTYPE CONSOLE}

{******************************************************************************
 * SBWBrokerAPIDemo.dpr
 *
 * Demonstrates the SBW Broker Services API and helper methods.
 *
 * This program shows:
 *   1. Module discovery (listing connected modules)
 *   2. Service discovery (listing services on a module)
 *   3. Method discovery (listing methods on a service)
 *   4. Descriptor retrieval (getting detailed module/service info)
 *   5. Using helper methods for simplified access
 *   6. Calling remote methods with different patterns
 *
 * Start the broker first, then optionally start SBWMathServer,
 * then run this demo.
 *****************************************************************************}

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  SBW.Connection in 'SBW.Connection.pas',
  SBW.Helpers in 'SBW.Helpers.pas',
  SBW.List in 'SBW.List.pas',
  SBW.Module in 'SBW.Module.pas',
  SBW.Service in 'SBW.Service.pas',
  SBW.Types in 'SBW.Types.pas',
  SBW.DataBlock in 'SBW.DataBlock.pas';

var
  Client: TSBWModuleImpl;

procedure PrintHeader(const Title: string);
begin
  WriteLn;
  WriteLn(StringOfChar('=', 60));
  WriteLn('  ', Title);
  WriteLn(StringOfChar('=', 60));
  WriteLn;
end;

procedure PrintSubHeader(const Title: string);
begin
  WriteLn;
  WriteLn('--- ', Title, ' ---');
  WriteLn;
end;

// =============================================================================
// Demo 1: Basic Broker Queries (Raw API)
// =============================================================================

procedure DemoBasicBrokerQueries;
var
  Version: string;
  ModuleIds: TArray<SBWModuleID>;
  I: Integer;
begin
  PrintHeader('Demo 1: Basic Broker Queries (Raw API)');

  // Get broker version
  Version := Client.GetVersion;
  WriteLn('Broker Version: ', Version);
  WriteLn;

  // Get existing module instance IDs
  ModuleIds := Client.GetExistingModuleInstanceIds;
  WriteLn('Connected Module IDs: ', Length(ModuleIds));
  for I := 0 to High(ModuleIds) do
    Write('  ', ModuleIds[I]);
  WriteLn;
end;

// =============================================================================
// Demo 2: Module Discovery (Raw API with TSBWList)
// =============================================================================

procedure DemoModuleDiscoveryRaw;
var
  Modules: TSBWList;
  Item: TSBWList;
  I: Integer;
  ModuleId: SBWModuleID;
  ModuleName: string;
begin
  PrintHeader('Demo 2: Module Discovery (Raw API with TSBWList)');

  // GetListOfModules returns a TSBWList where each item is [id, name]
  Modules := Client.GetListOfModules;
  try
    WriteLn('Connected Modules (', Modules.Count, '):');
    for I := 0 to Modules.Count - 1 do
    begin
      Item := Modules[I].GetList;
      ModuleId := Item[0].GetInteger;
      ModuleName := Item[1].GetString;
      WriteLn(Format('  [%d] %s', [ModuleId, ModuleName]));
    end;
  finally
    Modules.Free;
  end;
end;

// =============================================================================
// Demo 3: Module Discovery (Using Helpers)
// =============================================================================

procedure DemoModuleDiscoveryHelpers;
var
  Names: TArray<string>;
  Modules: TArray<TSBWModuleEntry>;
  I: Integer;
begin
  PrintHeader('Demo 3: Module Discovery (Using Helpers)');

  // Simple: Just get names
  PrintSubHeader('GetModuleNames');
  Names := TSBWHelpers.GetModuleNames(Client);
  WriteLn('Module Names:');
  for I := 0 to High(Names) do
    WriteLn('  ', Names[I]);

  // Get full info as records
  PrintSubHeader('GetModules');
  Modules := TSBWHelpers.GetModules(Client);
  WriteLn('Module Entries:');
  for I := 0 to High(Modules) do
    WriteLn(Format('  ID=%d, Name=%s', [Modules[I].ModuleID, Modules[I].ModuleName]));

  // Check if specific module exists
  PrintSubHeader('ModuleExists');
  WriteLn('edu.demo.mathserver exists: ',
    TSBWHelpers.ModuleExists(Client, 'edu.demo.mathserver'));
  WriteLn('nonexistent.module exists: ',
    TSBWHelpers.ModuleExists(Client, 'nonexistent.module'));
end;

// =============================================================================
// Demo 4: Service Discovery
// =============================================================================

procedure DemoServiceDiscovery;
var
  ModuleID: SBWModuleID;
  Services: TArray<TSBWServiceEntry>;
  Names: TArray<string>;
  I: Integer;
begin
  PrintHeader('Demo 4: Service Discovery');

  // First, try to find the math server
  ModuleID := Client.GetModuleIdByName('edu.demo.mathserver');
  if ModuleID = SBW_NO_MODULE_ID then
  begin
    WriteLn('Math server not running - using broker (module 0) instead');
    ModuleID := SBW_BROKER_MODULE_ID;
  end
  else
    WriteLn('Found math server at module ID ', ModuleID);

  // Get service names
  PrintSubHeader('GetServiceNames');
  Names := TSBWHelpers.GetServiceNames(Client, ModuleID);
  WriteLn('Services on module ', ModuleID, ':');
  for I := 0 to High(Names) do
    WriteLn('  ', Names[I]);

  // Get full service info
  PrintSubHeader('GetServices');
  Services := TSBWHelpers.GetServices(Client, ModuleID);
  WriteLn('Service Entries:');
  for I := 0 to High(Services) do
    WriteLn(Format('  ID=%d, Name=%s', [Services[I].ServiceID, Services[I].ServiceName]));
end;

// =============================================================================
// Demo 5: Method Discovery
// =============================================================================

procedure DemoMethodDiscovery;
var
  ModuleID: SBWModuleID;
  ServiceID: SBWServiceID;
  Signatures: TArray<string>;
  Names: TArray<string>;
  Methods: TArray<TSBWMethodEntry>;
  I: Integer;
begin
  PrintHeader('Demo 5: Method Discovery');

  // Try math server first, fall back to broker
  ModuleID := Client.GetModuleIdByName('edu.demo.mathserver');
  if ModuleID = SBW_NO_MODULE_ID then
  begin
    WriteLn('Math server not running - using broker SYSTEM service');
    ModuleID := SBW_BROKER_MODULE_ID;
    ServiceID := 0;  // SYSTEM service
  end
  else
  begin
    WriteLn('Found math server at module ID ', ModuleID);
    ServiceID := TSBWHelpers.RequireServiceId(Client, ModuleID, 'math');
    WriteLn('Math service ID: ', ServiceID);
  end;

  // Get method signatures
  PrintSubHeader('GetMethodSignatures');
  Signatures := TSBWHelpers.GetMethodSignatures(Client, ModuleID, ServiceID);
  WriteLn('Method Signatures:');
  for I := 0 to High(Signatures) do
    WriteLn('  ', Signatures[I]);

  // Get just method names
  PrintSubHeader('GetMethodNames');
  Names := TSBWHelpers.GetMethodNames(Client, ModuleID, ServiceID);
  WriteLn('Method Names:');
  for I := 0 to High(Names) do
    WriteLn('  ', Names[I]);

  // Get full method info
  PrintSubHeader('GetMethods');
  Methods := TSBWHelpers.GetMethods(Client, ModuleID, ServiceID);
  WriteLn('Method Entries:');
  for I := 0 to High(Methods) do
    WriteLn(Format('  ID=%d, Name=%-15s Sig=%s',
      [Methods[I].MethodID, Methods[I].Name, Methods[I].Signature]));
end;

// =============================================================================
// Demo 6: Module Descriptors
// =============================================================================

procedure DemoModuleDescriptors;
var
  Descs: TArray<TSBWModuleDescriptorRec>;
  Desc: TSBWModuleDescriptorRec;
  I: Integer;
begin
  PrintHeader('Demo 6: Module Descriptors');

  // Get all module descriptors
  PrintSubHeader('GetModuleDescriptorRecs (all running modules)');
  Descs := TSBWHelpers.GetModuleDescriptorRecs(Client, False, True);
  WriteLn('Module Descriptors (', Length(Descs), '):');
  for I := 0 to High(Descs) do
  begin
    WriteLn(Format('  Name: %s', [Descs[I].Name]));
    WriteLn(Format('    DisplayName: %s', [Descs[I].DisplayName]));
    WriteLn(Format('    Type: %d', [Descs[I].ManagementType]));
    if Descs[I].Help <> '' then
      WriteLn(Format('    Help: %s', [Descs[I].Help]));
    WriteLn;
  end;

  // Get specific module descriptor
  PrintSubHeader('GetModuleDescriptorRec (broker)');
  Desc := TSBWHelpers.GetModuleDescriptorRec(Client, SBW_BROKER_MODULE_ID);
  WriteLn('Broker Descriptor:');
  WriteLn('  Name: ', Desc.Name);
  WriteLn('  DisplayName: ', Desc.DisplayName);
end;

// =============================================================================
// Demo 7: Service Descriptors
// =============================================================================

procedure DemoServiceDescriptors;
var
  ModuleID: SBWModuleID;
  Descs: TArray<TSBWServiceDescriptorRec>;
  I: Integer;
begin
  PrintHeader('Demo 7: Service Descriptors');

  // Try math server first
  ModuleID := Client.GetModuleIdByName('edu.demo.mathserver');
  if ModuleID = SBW_NO_MODULE_ID then
  begin
    WriteLn('Math server not running - using broker');
    ModuleID := SBW_BROKER_MODULE_ID;
  end;

  Descs := TSBWHelpers.GetServiceDescriptorRecs(Client, ModuleID);
  WriteLn('Service Descriptors for module ', ModuleID, ':');
  for I := 0 to High(Descs) do
  begin
    WriteLn(Format('  Service: %s', [Descs[I].ServiceName]));
    WriteLn(Format('    DisplayName: %s', [Descs[I].DisplayName]));
    WriteLn(Format('    Category: %s', [Descs[I].Category]));
    if Descs[I].Help <> '' then
      WriteLn(Format('    Help: %s', [Descs[I].Help]));
    WriteLn;
  end;
end;

// =============================================================================
// Demo 8: Calling Methods (Multiple Patterns)
// =============================================================================

procedure DemoCallingMethods;
var
  ModuleID: SBWModuleID;
  ServiceID: SBWServiceID;
  SinMethodID: SBWMethodID;
  AddMethodID: SBWMethodID;
  SumMethodID: SBWMethodID;
  Args: TSBWDataBlockWriter;
  Result: TSBWDataBlockReader;
  R: Double;
  I: Integer;
begin
  PrintHeader('Demo 8: Calling Methods');

  ModuleID := Client.GetModuleIdByName('edu.demo.mathserver');
  if ModuleID = SBW_NO_MODULE_ID then
  begin
    WriteLn('Math server not running - skipping method call demos');
    WriteLn('Start SBWMathServer and run this demo again to see method calls');
    Exit;
  end;

  ServiceID := TSBWHelpers.RequireServiceId(Client, ModuleID, 'math');

  // Pattern 1: Manual ID resolution and call
  PrintSubHeader('Pattern 1: Manual (full control)');
  SinMethodID := TSBWHelpers.RequireMethodId(Client, ModuleID, ServiceID, 'sin');
  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteDouble(Pi / 2);
    Result := Client.CallMethod(ModuleID, ServiceID, SinMethodID, Args);
    try
      R := Result.ReadDouble;
      WriteLn(Format('sin(pi/2) = %g', [R]));
    finally
      Result.Free;
    end;
  finally
    Args.Free;
  end;

  // Pattern 2: Using call helper with IDs
  PrintSubHeader('Pattern 2: Call helper with IDs');
  AddMethodID := TSBWHelpers.RequireMethodId(Client, ModuleID, ServiceID, 'add');
  R := TSBWHelpers.CallDoubleDoubleToDouble(Client, ModuleID, ServiceID, AddMethodID, 3.5, 2.5);
  WriteLn(Format('add(3.5, 2.5) = %g', [R]));

  // Pattern 3: Using call helper with names (simplest)
  PrintSubHeader('Pattern 3: Call helper with names (simplest)');
  R := TSBWHelpers.CallDoubleToDouble(Client, 'edu.demo.mathserver', 'math', 'cos', 0);
  WriteLn(Format('cos(0) = %g', [R]));

  R := TSBWHelpers.CallDoubleToDouble(Client, 'edu.demo.mathserver', 'math', 'sqrt', 16);
  WriteLn(Format('sqrt(16) = %g', [R]));

  R := TSBWHelpers.CallDoubleDoubleToDouble(Client, 'edu.demo.mathserver', 'math', 'pow', 2, 10);
  WriteLn(Format('pow(2, 10) = %g', [R]));

  I := TSBWHelpers.CallIntArrayToInt(Client, 'edu.demo.mathserver', 'math', 'sum', [1, 2, 3, 4, 5]);
  WriteLn(Format('sum([1,2,3,4,5]) = %d', [I]));
end;

// =============================================================================
// Demo 9: Service Categories
// =============================================================================

procedure DemoServiceCategories;
var
  Categories: TArray<string>;
  Services: TArray<TSBWList>;
  I: Integer;
begin
  PrintHeader('Demo 9: Service Categories');

  // Get root categories
  PrintSubHeader('Root Categories');
  Categories := Client.GetServiceCategories('');
  WriteLn('Root Categories:');
  for I := 0 to High(Categories) do
    WriteLn('  ', Categories[I]);

  // Find services in a category
  PrintSubHeader('FindServices in "Math" category');
  Services := Client.FindServices('Math', True);
  try
    WriteLn('Services in Math category: ', Length(Services));
    for I := 0 to High(Services) do
    begin
      if Services[I].Count >= 2 then
        WriteLn(Format('  %s / %s',
          [Services[I][0].GetString, Services[I][1].GetString]));
    end;
  finally
    for I := 0 to High(Services) do
      Services[I].Free;
  end;
end;

// =============================================================================
// Demo 10: Error Handling
// =============================================================================

procedure DemoErrorHandling;
begin
  PrintHeader('Demo 10: Error Handling');

  // RequireXxx methods raise ESBWNotFound
  PrintSubHeader('Handling ESBWNotFound');
  try
    TSBWHelpers.RequireModuleId(Client, 'nonexistent.module');
  except
    on E: ESBWNotFound do
      WriteLn('Caught ESBWNotFound: ', E.Message);
  end;

  try
    TSBWHelpers.RequireServiceId(Client, SBW_BROKER_MODULE_ID, 'nonexistent');
  except
    on E: ESBWNotFound do
      WriteLn('Caught ESBWNotFound: ', E.Message);
  end;

  // Safe checking without exceptions
  PrintSubHeader('Safe Checking (no exceptions)');
  if TSBWHelpers.ModuleExists(Client, 'edu.demo.mathserver') then
    WriteLn('Math server is available')
  else
    WriteLn('Math server is not running');

  if Client.GetModuleIdByName('some.module') = SBW_NO_MODULE_ID then
    WriteLn('some.module not found (checked with GetModuleIdByName)');
end;

// =============================================================================
// Main
// =============================================================================

begin
  try
    WriteLn;
    WriteLn(StringOfChar('*', 60));
    WriteLn('*  SBW Broker API Demo                                     *');
    WriteLn(StringOfChar('*', 60));
    WriteLn;

    Client := TSBWModuleImpl.Create(
      'edu.demo.apidemo',
      'API Demo Client',
      mmtSelfManaged);
    try
      WriteLn('Connecting to broker...');
      Client.Connect('127.0.0.1', SBW_DEFAULT_PORT);
      WriteLn('Connected! Client Module ID = ', Client.GetModuleID);

      // Run all demos
      DemoBasicBrokerQueries;
      DemoModuleDiscoveryRaw;
      DemoModuleDiscoveryHelpers;
      DemoServiceDiscovery;
      DemoMethodDiscovery;
      DemoModuleDescriptors;
      DemoServiceDescriptors;
      DemoCallingMethods;
      DemoServiceCategories;
      DemoErrorHandling;

      PrintHeader('Demo Complete');
      WriteLn('All demos completed successfully!');

      Client.Disconnect;
    finally
      Client.Free;
    end;

    WriteLn;
    WriteLn('Press Enter to exit...');
    ReadLn;

  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end.

