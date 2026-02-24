program Project_Cmd_Browser;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Generics.Collections,
  uConsoleMenu in 'uConsoleMenu.pas',
  SBW.Module in '..\..\SBW.Module.pas',
  SBW.Connection in '..\..\SBW.Connection.pas',
  SBW.Types in '..\..\SBW.Types.pas',
  SBW_Array in '..\..\SBW_Array.pas',
  SBW.DataBlock in '..\..\SBW.DataBlock.pas',
  SBW.Message in '..\..\SBW.Message.pas',
  SBW.Signature in '..\..\SBW.Signature.pas',
  SBW.List in '..\..\SBW.List.pas',
  SBW.Service in '..\..\SBW.Service.pas',
  SBW.Utils in '..\..\SBW.Utils.pas',
  SBW.Helpers in '..\..\SBW.Helpers.pas';

var
  Client: TSBWModuleImpl;
  CurrentModuleName : String;
  CurrentServiceName : String;

{ --- Global SBW Helper Functions --- }

//function GetModuleNames: TArray<string>;
//var
//  Modules: TSBWList;
//  i: Integer;
//  Item: TSBWList;
//begin
//  Modules := Client.GetListOfModules;
//  try
//    SetLength(Result, Modules.Count);
//    for i := 0 to Modules.Count - 1 do
//    begin
//      Item := Modules[i].GetList;
//      Result[i] := Item[1].GetString; // Assuming index 1 is Name
//    end;
//  finally
//     Modules.Free;
//  end;
//end;

function GetServiceNames(const ModuleName: string): TArray<string>;
var
  ModuleId: Integer;
  ServiceList: TSBWList;
  i: Integer;
  InnerList: TSBWList;
begin
  // Use GetModuleInstanceByName to auto-launch if needed
  ModuleId := Client.GetModuleInstanceByName(ModuleName);
  if ModuleId = SBW_NO_MODULE_ID then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  ServiceList := Client.GetServiceIds(ModuleId);
  try
    SetLength(Result, ServiceList.Count);
    for i := 0 to ServiceList.Count - 1 do
    begin
      InnerList := ServiceList[i].GetList;
      Result[i] := InnerList[1].GetString;
    end;
  finally
    ServiceList.Free;
  end;
end;

function GetMethodNames(const ModuleName, ServiceName: string): TArray<string>;
var
  ModuleId, ServiceId: Integer;
  MethodList, InnerList: TSBWList;
  i : Integer;
begin
  MethodList := Client.GetMethodIds(Modulename, ServiceName);
  try
    SetLength (Result, MethodList.Count);
    for i := 0 to MethodList.Count - 1 do
        begin
        InnerList := MethodList[i].GetList;
        Result[i] := InnerList[1].GetString;
        end;
  finally
    MethodList.Free;
  end;
end;

{ --- Application Logic Classes --- }

type
  TModuleLogic = class
  private
    FCurrentModule: string;
    FCurrentService: string;

    // Callbacks for Dynamic Menus
    function FetchModules: TArray<string>;
    function FetchServices: TArray<string>;
    function FetchMethods: TArray<string>;

    // Selection Handlers
    procedure OnModuleSelected(const SelectedName: string);
    procedure OnServiceSelected(const SelectedName: string);
    procedure OnMethodSelected(const SelectedName: string);
  public
    procedure StartBrowser;
  end;

  TAppLogic = class
    procedure GetBrokerVersion;
    procedure SystemInfo;
  end;

{ TModuleLogic Implementation }

function TModuleLogic.FetchModules: TArray<string>;
var
  RunningNames: TArray<string>;
  Descriptors: TArray<TSBWModuleDescriptorRec>;
  ResultList: TList<string>;
  Desc: TSBWModuleDescriptorRec;
  I: Integer;
  IsRunning: Boolean;
begin
  ResultList := TList<string>.Create;
  try
    // Get running modules
    RunningNames := TSBWHelpers.GetModuleNames(Client);

    // Add running modules first (no prefix)
    for I := 0 to High(RunningNames) do
      ResultList.Add(RunningNames[I]);

    // Get all registered modules (including non-running)
    Descriptors := TSBWHelpers.GetModuleDescriptorRecs(Client, False, True);

    // Add registered-but-not-running modules with * prefix
    for Desc in Descriptors do
    begin
      // Check if already in running list
      IsRunning := False;
      for I := 0 to High(RunningNames) do
        if SameText(RunningNames[I], Desc.Name) then
        begin
          IsRunning := True;
          Break;
        end;

      if not IsRunning then
        ResultList.Add('* ' + Desc.Name);  // Star indicates "will auto-launch"
    end;

    Result := ResultList.ToArray;
  finally
    ResultList.Free;
  end;
end;

function TModuleLogic.FetchServices: TArray<string>;
begin
  Result := GetServiceNames(FCurrentModule);
end;

function TModuleLogic.FetchMethods: TArray<string>;
begin
  CurrentModuleName := FCurrentModule;
  CurrentServiceName := FCurrentService;
  Result := GetMethodNames(FCurrentModule, FCurrentService);
end;

procedure TModuleLogic.OnModuleSelected(const SelectedName: string);
var
  ServiceMenu: TDynamicNameMenu;
  ActualName: string;
begin
  // Strip the "* " prefix if present
  if SelectedName.StartsWith('* ') then
    ActualName := Copy(SelectedName, 3, Length(SelectedName))
  else
    ActualName := SelectedName;

  FCurrentModule := ActualName;
  ServiceMenu := TDynamicNameMenu.Create('Services for ' + ActualName,
                                         FetchServices,
                                         OnServiceSelected);
  try
    ServiceMenu.Execute;
  finally
    ServiceMenu.Free;
  end;
end;


procedure TModuleLogic.OnServiceSelected(const SelectedName: string);
var
  MethodMenu: TDynamicNameMenu;
begin
  FCurrentService := SelectedName;
  MethodMenu := TDynamicNameMenu.Create('Methods for ' + SelectedName,
                                        FetchMethods,
                                        OnMethodSelected);
  try
    MethodMenu.Execute;
  finally
    MethodMenu.Free;
  end;
end;

procedure TModuleLogic.OnMethodSelected(const SelectedName: string);
var SBWSignatureParser : TSBWSignatureParser;
    SBWSignature : TSBWSignature;
begin
  Writeln('Selected Method Signature:');
  Writeln('--------------------------');
  Writeln(SelectedName);
  try
  SBWSignatureParser := TSBWSignatureParser.Create;
  SBWSignature := SBWSignatureParser.Parse(SelectedName);
  Writeln(Client.GetMethodHelpByName(CurrentModuleName, CurrentServiceName, SBWSignature.Name));
  Writeln;
  Writeln('Press Enter to return to methods list...');
  finally
    SBWSignature.Free;
    SBWSignatureParser.Free;
  end;
  Readln;
end;

procedure TModuleLogic.StartBrowser;
var
  ModuleMenu: TDynamicNameMenu;
begin
  ModuleMenu := TDynamicNameMenu.Create('SBW Module Browser',
                                        FetchModules,
                                        OnModuleSelected);
  try
    ModuleMenu.Execute;
  finally
    ModuleMenu.Free;
  end;
end;

{ TAppLogic Implementation }

procedure TAppLogic.GetBrokerVersion;
begin
  Writeln('Broker Version: ', Client.GetVersion);
  Readln;
end;

procedure TAppLogic.SystemInfo;
begin
  Writeln('System Time: ', DateTimeToStr(Now));
  Readln;
end;

{ --- Main Program Execution --- }

var
  AppLogic: TAppLogic;
  ModLogic: TModuleLogic;
  MainMenu: TConsoleMenu;
  a : TArray<String>;
begin
  Client := TSBWModuleImpl.Create('cmd.browser', 'Browser', mmtSelfManaged);
  try
    Client.Connect('127.0.0.1', SBW_DEFAULT_PORT);

    // Initialize SBW Client
    a := TSBWHelpers.GetModuleNames(Client);

    AppLogic := TAppLogic.Create;
    ModLogic := TModuleLogic.Create;
    MainMenu := TConsoleMenu.Create('MAIN MENU', True);

    try
      MainMenu.Add(TMenuItem.Create('Browse Modules/Services/Methods', ModLogic.StartBrowser));
      MainMenu.Add(TMenuItem.Create('Get Broker Version', AppLogic.GetBrokerVersion));
      MainMenu.Add(TMenuItem.Create('System Info', AppLogic.SystemInfo));

      MainMenu.Execute;
    finally
      MainMenu.Free;
      ModLogic.Free;
      AppLogic.Free;
    end;

  finally
    Client.Disconnect;
    Client.Free;
  end;
end.

