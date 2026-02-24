program SBWRegistryDemo;

{$APPTYPE CONSOLE}

{******************************************************************************
 * SBWRegistryDemo.dpr
 *
 * Demonstrates the module registry persistence features.
 *
 * Shows how to:
 *   1. Register modules with the broker
 *   2. Save/load the registry to/from JSON
 *   3. Use auto-save feature
 *   4. Launch registered modules
 *****************************************************************************}

uses
  System.SysUtils,
  System.IOUtils,
  SBW.Types,
  SBW.Broker.Registry;

var
  Registry: TSBWModuleRegistry;

procedure PrintRegistry;
var
  Modules: TArray<TSBWRegisteredModule>;
  Module: TSBWRegisteredModule;
  Svc: TSBWRegisteredService;
begin
  WriteLn;
  WriteLn('Current Registry Contents:');
  WriteLn(StringOfChar('-', 50));

  Modules := Registry.GetAllModules;
  if Length(Modules) = 0 then
  begin
    WriteLn('  (empty)');
    Exit;
  end;

  for Module in Modules do
  begin
    WriteLn(Format('Module: %s', [Module.Name]));
    WriteLn(Format('  DisplayName: %s', [Module.DisplayName]));
    WriteLn(Format('  Type: %d', [Ord(Module.ModuleType)]));
    WriteLn(Format('  CommandLine: %s', [Module.CommandLine]));
    if Module.Help <> '' then
      WriteLn(Format('  Help: %s', [Module.Help]));

    if Module.Services.Count > 0 then
    begin
      WriteLn('  Services:');
      for Svc in Module.Services do
        WriteLn(Format('    - %s (%s)', [Svc.ServiceName, Svc.Category]));
    end;
    WriteLn;
  end;
end;

procedure DemoBasicUsage;
begin
  WriteLn;
  WriteLn('=== Demo 1: Basic Registration ===');

  // Register some modules
  Registry.RegisterModule(
    'edu.demo.mathserver',
    'Math Server',
    mmtSelfManaged,
    'C:\SBW\Modules\SBWMathServer.exe',
    'Provides basic math operations');

  Registry.RegisterModule(
    'edu.demo.canvasserver',
    'Canvas Server',
    mmtSelfManaged,
    'C:\SBW\Modules\CanvasServer.exe',
    'Drawing canvas for remote rendering');

  // Register services for a module
  Registry.RegisterService(
    'edu.demo.mathserver',
    'math',
    'Math Service',
    'Math',
    'Trigonometric and arithmetic functions');

  Registry.RegisterService(
    'edu.demo.canvasserver',
    'canvas',
    'Canvas Service',
    'Graphics',
    'Remote drawing operations');

  PrintRegistry;
end;

procedure DemoSaveLoad;
var
  FilePath: string;
  JSONContent: string;
begin
  WriteLn;
  WriteLn('=== Demo 2: Save/Load Registry ===');

  FilePath := TPath.Combine(TPath.GetTempPath, 'sbw_registry_demo.json');
  WriteLn('Saving to: ', FilePath);

  Registry.SaveToFile(FilePath);
  WriteLn('Saved!');

  // Show the JSON content
  WriteLn;
  WriteLn('JSON Content:');
  WriteLn(StringOfChar('-', 50));
  JSONContent := TFile.ReadAllText(FilePath);
  WriteLn(JSONContent);
  WriteLn(StringOfChar('-', 50));

  // Clear and reload
  WriteLn;
  WriteLn('Clearing registry...');
  Registry.Clear;
  WriteLn('Registry count: ', Registry.Count);

  WriteLn('Loading from file...');
  Registry.LoadFromFile(FilePath);
  WriteLn('Registry count: ', Registry.Count);

  PrintRegistry;

  // Cleanup
  TFile.Delete(FilePath);
end;

procedure DemoAutoSave;
var
  FilePath: string;
begin
  WriteLn;
  WriteLn('=== Demo 3: Auto-Save Feature ===');

  FilePath := TPath.Combine(TPath.GetTempPath, 'sbw_registry_autosave.json');

  Registry.Clear;
  Registry.FilePath := FilePath;
  Registry.AutoSave := True;

  WriteLn('AutoSave enabled, FilePath: ', FilePath);
  WriteLn;

  WriteLn('Registering module (will auto-save)...');
  Registry.RegisterModule(
    'edu.demo.autosave.test',
    'Auto-Save Test Module',
    mmtSelfManaged,
    'C:\Test\Module.exe',
    'Testing auto-save');

  WriteLn('File exists: ', TFile.Exists(FilePath));
  WriteLn('Dirty flag: ', Registry.Dirty);

  // Verify by loading into a new registry
  WriteLn;
  WriteLn('Verifying by loading into new registry...');
  var Registry2 := TSBWModuleRegistry.Create;
  try
    Registry2.LoadFromFile(FilePath);
    WriteLn('Loaded ', Registry2.Count, ' module(s) from auto-saved file');
    WriteLn('Module found: ', Registry2.IsRegistered('edu.demo.autosave.test'));
  finally
    Registry2.Free;
  end;

  // Cleanup
  Registry.AutoSave := False;
  TFile.Delete(FilePath);
end;

procedure DemoLoadIfExists;
begin
  WriteLn;
  WriteLn('=== Demo 4: LoadIfExists (Broker Startup Pattern) ===');

  Registry.Clear;
  Registry.FilePath := TSBWModuleRegistry.GetDefaultFilePath;

  WriteLn('Default registry path: ', Registry.FilePath);
  WriteLn('File exists: ', TFile.Exists(Registry.FilePath));

  // This is what the broker would do at startup:
  WriteLn;
  WriteLn('Calling LoadIfExists...');
  Registry.LoadIfExists;

  if Registry.Count > 0 then
    WriteLn('Loaded ', Registry.Count, ' registered module(s)')
  else
    WriteLn('No existing registry file, starting with empty registry');
end;

procedure DemoJSONImportExport;
var
  JSONString: string;
begin
  WriteLn;
  WriteLn('=== Demo 5: JSON Import/Export (String-based) ===');

  Registry.Clear;

  // Manually create JSON
  JSONString :=
    '{' + sLineBreak +
    '  "version": 1,' + sLineBreak +
    '  "modules": [' + sLineBreak +
    '    {' + sLineBreak +
    '      "name": "imported.module",' + sLineBreak +
    '      "displayName": "Imported Module",' + sLineBreak +
    '      "moduleType": 2,' + sLineBreak +
    '      "commandLine": "C:\\Imported\\Module.exe",' + sLineBreak +
    '      "help": "Imported from JSON string",' + sLineBreak +
    '      "services": []' + sLineBreak +
    '    }' + sLineBreak +
    '  ]' + sLineBreak +
    '}';

  WriteLn('Importing from JSON string...');
  Registry.FromJSON(JSONString);

  WriteLn('Imported ', Registry.Count, ' module(s)');
  WriteLn('Module exists: ', Registry.IsRegistered('imported.module'));

  WriteLn;
  WriteLn('Exporting to JSON string:');
  WriteLn(Registry.ToJSON);
end;

procedure DemoBrokerIntegration;
begin
  WriteLn;
  WriteLn('=== Demo 6: Broker Integration Pattern ===');
  WriteLn;
  WriteLn('// In TSBWBroker.Create:');
  WriteLn('FRegistry := TSBWModuleRegistry.Create;');
  WriteLn('FRegistry.FilePath := TSBWModuleRegistry.GetDefaultFilePath;');
  WriteLn('FRegistry.AutoSave := True;  // Optional: auto-save on changes');
  WriteLn('FRegistry.LoadIfExists;');
  WriteLn;
  WriteLn('// In TSBWBroker.Destroy:');
  WriteLn('if FRegistry.Dirty then');
  WriteLn('  FRegistry.SaveToFile;');
  WriteLn('FRegistry.Free;');
  WriteLn;
  WriteLn('// When handling SYS_METHOD_REGISTER_MODULE:');
  WriteLn('FRegistry.RegisterModule(Name, DisplayName, ModuleType, CmdLine, Help);');
  WriteLn('// With AutoSave=True, this automatically persists');
  WriteLn;
  WriteLn('// When handling SYS_METHOD_GET_MODULE_INSTANCE:');
  WriteLn('if not IsModuleConnected(ModuleName) then');
  WriteLn('  if FRegistry.IsRegistered(ModuleName) then');
  WriteLn('    FRegistry.LaunchModule(ModuleName);');
end;

begin
  try
    WriteLn('========================================');
    WriteLn('  SBW Module Registry Persistence Demo');
    WriteLn('========================================');

    Registry := TSBWModuleRegistry.Create;
    try
      DemoBasicUsage;
      DemoSaveLoad;
      DemoAutoSave;
      DemoLoadIfExists;
      DemoJSONImportExport;
      DemoBrokerIntegration;

      WriteLn;
      WriteLn('========================================');
      WriteLn('  All demos completed!');
      WriteLn('========================================');
    finally
      Registry.Free;
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
