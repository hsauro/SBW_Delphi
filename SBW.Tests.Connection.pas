unit SBW.Tests.Connection;

{******************************************************************************
 * SBW.Tests.Connection.pas
 *
 * Unit tests for SBW connection and broker.
 *
 * Note: These tests require actual socket connections, so they test
 * the full stack including the broker.
 *****************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  SBW.Types, SBW.DataBlock, SBW.Message, SBW.Connection, SBW.Broker, SBW.BrokerTypes;

type
  TConnectionTests = class
  private
    FTestCount: Integer;
    FPassCount: Integer;
    FFailCount: Integer;
    FBroker: TSBWBroker;

    procedure Check(Condition: Boolean; const TestName: string);
    procedure CheckEquals(Expected, Actual: SBWInteger; const TestName: string); overload;
    procedure CheckEquals(const Expected, Actual: string; const TestName: string); overload;

    procedure SetUp;
    procedure TearDown;
  public
    constructor Create;

    procedure TestBrokerStartStop;
    procedure TestSingleClientConnect;
    procedure TestMultipleClientsConnect;
    procedure TestClientDisconnect;
    procedure TestModuleIDsUnique;

    procedure RunAllTests;

    property TestCount: Integer read FTestCount;
    property PassCount: Integer read FPassCount;
    property FailCount: Integer read FFailCount;
  end;

procedure RunConnectionTests;

implementation

const
  TEST_PORT = 10199; // Use a different port for testing

type
  TBrokerEventHandler = class
    procedure OnLog(Sender: TObject; const Msg: string);
    procedure OnModuleConnect(Sender: TObject; ModuleInfo: TSBWModuleInfo);
    procedure OnModuleDisconnect(Sender: TObject; ModuleID: SBWModuleID);
  end;

var
  EventHandler: TBrokerEventHandler;

procedure TBrokerEventHandler.OnLog(Sender: TObject; const Msg: string);
begin
  WriteLn(FormatDateTime('hh:nn:ss.zzz', Now), ' ', Msg);
end;

procedure TBrokerEventHandler.OnModuleConnect(Sender: TObject; ModuleInfo: TSBWModuleInfo);
begin
  WriteLn(Format('>>> Module connected: ID=%d, Name=%s',
    [ModuleInfo.ModuleID, ModuleInfo.ModuleName]));
end;

procedure TBrokerEventHandler.OnModuleDisconnect(Sender: TObject; ModuleID: SBWModuleID);
begin
  WriteLn(Format('<<< Module disconnected: ID=%d', [ModuleID]));
end;


procedure RunConnectionTests;
var
  Tests: TConnectionTests;
begin
  Tests := TConnectionTests.Create;
  try
    Tests.RunAllTests;
    WriteLn;
    WriteLn(Format('Connection Tests: %d tests, %d passed, %d failed',
      [Tests.TestCount, Tests.PassCount, Tests.FailCount]));
  finally
    Tests.Free;
  end;
end;

{ TConnectionTests }

constructor TConnectionTests.Create;
begin
  inherited Create;
  FTestCount := 0;
  FPassCount := 0;
  FFailCount := 0;
end;

procedure TConnectionTests.Check(Condition: Boolean; const TestName: string);
begin
  Inc(FTestCount);
  if Condition then
  begin
    Inc(FPassCount);
    WriteLn('  [PASS] ' + TestName);
  end
  else
  begin
    Inc(FFailCount);
    WriteLn('  [FAIL] ' + TestName);
  end;
end;

procedure TConnectionTests.CheckEquals(Expected, Actual: SBWInteger; const TestName: string);
begin
  Inc(FTestCount);
  if Expected = Actual then
  begin
    Inc(FPassCount);
    WriteLn('  [PASS] ' + TestName);
  end
  else
  begin
    Inc(FFailCount);
    WriteLn(Format('  [FAIL] %s: expected %d, got %d', [TestName, Expected, Actual]));
  end;
end;

procedure TConnectionTests.CheckEquals(const Expected, Actual: string; const TestName: string);
begin
  Inc(FTestCount);
  if Expected = Actual then
  begin
    Inc(FPassCount);
    WriteLn('  [PASS] ' + TestName);
  end
  else
  begin
    Inc(FFailCount);
    WriteLn(Format('  [FAIL] %s: expected "%s", got "%s"', [TestName, Expected, Actual]));
  end;
end;

procedure TConnectionTests.SetUp;
begin
  InitializeSockets;
  FBroker := TSBWBroker.Create;
  // Uncomment to get logs FBroker.OnLog := EventHandler.OnLog;
  FBroker.Start(TEST_PORT);
  Sleep(100); // Give broker time to start
end;

procedure TConnectionTests.TearDown;
begin
  FBroker.Stop;
  FBroker.Free;
  FBroker := nil;
  Sleep(100); // Give sockets time to close
end;

procedure TConnectionTests.TestBrokerStartStop;
begin
  SetUp;
  try
    Check(FBroker.Running, 'Broker is running');
    CheckEquals(TEST_PORT, FBroker.Port, 'Broker port');
  finally
    TearDown;
  end;
end;

procedure TConnectionTests.TestSingleClientConnect;
var
  Client: TSBWConnection;
begin
  WriteLn('TestSingleClientConnect');
  SetUp;
  try
    Client := TSBWConnection.Create;
    try
      Client.Connect('127.0.0.1', TEST_PORT, 'TestModule1');

      Check(Client.Connected, 'Client is connected');
      Check(Client.ModuleID > 0, 'Module ID assigned');
      CheckEquals('TestModule1', Client.ModuleName, 'Module name');

      // Verify broker sees the module
      Sleep(50);
      Check(Length(FBroker.GetAllModules) = 1, 'Broker has 1 module');

      Client.Disconnect;
      Check(not Client.Connected, 'Client disconnected');
    finally
      Client.Free;
    end;
  finally
    TearDown;
  end;
end;

procedure TConnectionTests.TestMultipleClientsConnect;
var
  Client1, Client2, Client3: TSBWConnection;
begin
  WriteLn('TestMultipleClientsConnect');
  SetUp;
  try
    Client1 := TSBWConnection.Create;
    Client2 := TSBWConnection.Create;
    Client3 := TSBWConnection.Create;
    try
      Client1.Connect('127.0.0.1', TEST_PORT, 'Module1');
      Client2.Connect('127.0.0.1', TEST_PORT, 'Module2');
      Client3.Connect('127.0.0.1', TEST_PORT, 'Module3');

      Check(Client1.Connected, 'Client1 connected');
      Check(Client2.Connected, 'Client2 connected');
      Check(Client3.Connected, 'Client3 connected');

      Sleep(50);
      Check(Length(FBroker.GetAllModules) = 3, 'Broker has 3 modules');

      Client1.Disconnect;
      Client2.Disconnect;
      Client3.Disconnect;
    finally
      Client3.Free;
      Client2.Free;
      Client1.Free;
    end;
  finally
    TearDown;
  end;
end;

procedure TConnectionTests.TestClientDisconnect;
var
  Client: TSBWConnection;
  ModuleID: SBWModuleID;
begin
  WriteLn('TestClientDisconnect');
  SetUp;
  try
    Client := TSBWConnection.Create;
    try
      Client.Connect('127.0.0.1', TEST_PORT, 'DisconnectTest');
      ModuleID := Client.ModuleID;

      Sleep(50);
      Check(FBroker.GetModule(ModuleID) <> nil, 'Module exists before disconnect');

      Client.Disconnect;
      Sleep(200); // Give broker time to detect disconnect

      Check(FBroker.GetModule(ModuleID) = nil, 'Module removed after disconnect');
    finally
      Client.Free;
    end;
  finally
    TearDown;
  end;
end;

procedure TConnectionTests.TestModuleIDsUnique;
var
  Client1, Client2: TSBWConnection;
begin
  WriteLn('TestModuleIDsUnique');
  SetUp;
  try
    Client1 := TSBWConnection.Create;
    Client2 := TSBWConnection.Create;
    try
      Client1.Connect('127.0.0.1', TEST_PORT, 'UniqueTest1');
      Client2.Connect('127.0.0.1', TEST_PORT, 'UniqueTest2');

      Check(Client1.ModuleID <> Client2.ModuleID, 'Module IDs are different');
      Check(Client1.ModuleID > 0, 'Module ID 1 is positive');
      Check(Client2.ModuleID > 0, 'Module ID 2 is positive');

      Client1.Disconnect;
      Client2.Disconnect;
    finally
      Client2.Free;
      Client1.Free;
    end;
  finally
    TearDown;
  end;
end;

procedure TConnectionTests.RunAllTests;
begin
  WriteLn('=== SBW Connection Tests ===');
  WriteLn;

  EventHandler := TBrokerEventHandler.Create;
  try
    TestBrokerStartStop;
    TestSingleClientConnect;
    TestMultipleClientsConnect;
    TestClientDisconnect;
    TestModuleIDsUnique;
  finally
    EventHandler.Free;
  end;


end;

end.

