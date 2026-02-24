program Project_Cmd_Browser;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  uConsoleMenu in 'uConsoleMenu.pas',
  SBW.Module in '..\..\SBW.Module.pas',
  SBW.Connection in '..\..\SBW.Connection.pas',
  SBW.Tests_Array in '..\..\SBW.Tests_Array.pas',
  SBW.Types in '..\..\SBW.Types.pas',
  SBW_Array in '..\..\SBW_Array.pas',
  SBW.DataBlock in '..\..\SBW.DataBlock.pas',
  SBW.Message in '..\..\SBW.Message.pas',
  SBW.Signature in '..\..\SBW.Signature.pas',
  SBW.List in '..\..\SBW.List.pas',
  SBW.Service in '..\..\SBW.Service.pas';

type
  TAppLogic = class
    procedure GetBrokerVersion;
    procedure ListModules;
    procedure SystemInfo;
  end;

  TModuleLogic = class
      function GetServices : TArray<String>;
      procedure OnUserPicked (const SelectedName : String);
  end;

var
  Logic: TAppLogic;
  MainMenu, SettingsMenu: TConsoleMenu;
  Host: string;
  Port: Word;
  Client: TSBWModuleImpl;
  ModuleMenu : TDynamicNameMenu;
  ModuleLogic : TModuleLogic;

function GetModuleNames (Client : TSBWModuleImpl) : TArray<string>;
var  Modules : TSBWList;
     i, SBWModuleId : Integer;
     InnerList : TSBWList;
     Item : TSBWList;
begin
   Modules := Client.GetListOfModules;
   SetLength (Result, Modules.Count);
   for i := 0 to Modules.Count - 1 do
       begin
       Item := Modules[i].GetList;
       Result[i] := Item[1].GetString;
       end;
end;


function GetServiceNames (Client : TSBWModuleImpl; ModuleId : Integer) : TArray<string>;
var  ServiceList : TSBWList;
     i, SBWModuleId : Integer;
     InnerList : TSBWList;
begin
  ServiceList := Client.GetServiceIds(ModuleId);
  SetLength (result, ServiceList.Count);
  for i := 0 to ServiceList.Count - 1 do
         begin
         InnerList := ServiceList[i].GetList;
         result[i] := InnerList[1].GetString;
         end;
end;


procedure TAppLogic.GetBrokerVersion;
begin
  Writeln ('Broker Version: ', Client.GetVersion);
  Readln;
end;


function TModuleLogic.GetServices : TArray<String>;
begin
end;


procedure TModuleLogic.OnUserPicked (const SelectedName : String);
begin
end;


procedure TAppLogic.ListModules;
var Modules : TArray<String>;
    i : integer;
begin
  ModuleMenu := TDynamicNameMenu.Create('Select a Module',
                                    ModuleLogic.GetServices,
                                    ModuleLogic.OnUserPicked);

  Modules := GetModuleNames(Client);
  for i := 0 to length (Modules) - 1 do
      Writeln (i, '. ', Modules[i]);
  Readln;
end;


procedure TAppLogic.SystemInfo;
begin
  Writeln('System Time: ', DateTimeToStr(Now));
  Readln;
end;


procedure StartClient;
begin
  Host := '127.0.0.1';
  Port := SBW_DEFAULT_PORT;
  InitializeSockets;

  Client := TSBWModuleImpl.Create('cmd.browser','Browser',  mmtSelfManaged);
  Client.Connect(Host, Port);
end;


begin
 StartClient;
 Logic := TAppLogic.Create;
 ModuleLogic := TModuleLogic.Create;

  // Create Menu Structure
  MainMenu := TConsoleMenu.Create('MAIN MENU', True);
  SettingsMenu := TConsoleMenu.Create('SETTINGS');

  // Add items to Sub-menu
  SettingsMenu.Add(TMenuItem.Create('Show System Info', Logic.SystemInfo));


  // Add items to Main menu
  MainMenu.Add(TMenuItem.Create('Broker Version', Logic.GetBrokerVersion));
  MainMenu.Add(TMenuItem.Create('List Modules', Logic.ListModules));
  MainMenu.Add(ModuleMenu);
  MainMenu.Execute;
  //MainMenu.Add(SettingsMenu); // Nesting the sub-menu

  try
    MainMenu.Execute;
  finally
    MainMenu.Free; // Freeing MainMenu frees all sub-elements automatically
    Logic.Free;
    Client.Disconnect;
    Client.Free;
    FinalizeSockets;
  end;
end.
