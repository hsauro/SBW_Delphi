program SBWMathClient;

{$APPTYPE CONSOLE}

{******************************************************************************
 * SBWMathClient.dpr
 *
 * Client that calls the Math Server via SBW.
 *
 * Start the broker first, then the MathServer, then run this client.
 * This version uses broker discovery to find the math server.
 *****************************************************************************}

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  SBW.Types in 'SBW.Types.pas',
  SBW.DataBlock in 'SBW.DataBlock.pas',
  SBW.Message in 'SBW.Message.pas',
  SBW.Signature in 'SBW.Signature.pas',
  SBW.Service in 'SBW.Service.pas',
  SBW.Connection in 'SBW.Connection.pas',
  SBW.List in 'SBW.List.pas',
  SBW.Module in 'SBW.Module.pas',
  SBW.Utils in 'SBW.Utils.pas';

var
  Client: TSBWModuleImpl;
  Host: string;
  Port: Word;
  ServerModuleID: SBWModuleID;
  MathServiceID: SBWServiceID;
  SinMethodID, CosMethodID, AddMethodID, SumMethodID: SBWMethodID;

function CallSin(X: Double): Double;
var
  Args: TSBWDataBlockWriter;
  ResultReader: TSBWDataBlockReader;
begin
  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteDouble(X);
    ResultReader := Client.CallMethod(ServerModuleID, MathServiceID, SinMethodID, Args);
    try
      Result := ResultReader.ReadDouble;
    finally
      ResultReader.Free;
    end;
  finally
    Args.Free;
  end;
end;

function CallCos(X: Double): Double;
var
  Args: TSBWDataBlockWriter;
  ResultReader: TSBWDataBlockReader;
begin
  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteDouble(X);
    ResultReader := Client.CallMethod(ServerModuleID, MathServiceID, CosMethodID, Args);
    try
      Result := ResultReader.ReadDouble;
    finally
      ResultReader.Free;
    end;
  finally
    Args.Free;
  end;
end;

function CallAdd(A, B: Double): Double;
var
  Args: TSBWDataBlockWriter;
  ResultReader: TSBWDataBlockReader;
begin
  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteDouble(A);
    Args.WriteDouble(B);
    ResultReader := Client.CallMethod(ServerModuleID, MathServiceID, AddMethodID, Args);
    try
      Result := ResultReader.ReadDouble;
    finally
      ResultReader.Free;
    end;
  finally
    Args.Free;
  end;
end;

function CallSum(const Values: TArray<Integer>): Integer;
var
  Args: TSBWDataBlockWriter;
  ResultReader: TSBWDataBlockReader;
begin
  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteIntegerArray(Values);
    ResultReader := Client.CallMethod(ServerModuleID, MathServiceID, SumMethodID, Args);
    try
      Result := ResultReader.ReadInteger;
    finally
      ResultReader.Free;
    end;
  finally
    Args.Free;
  end;
end;

procedure DiscoverMathServer;
var
  Modules: TSBWList;
  Services: TSBWList;
  Methods: TSBWList;
  I: Integer;
  Item: TSBWList;
  ModuleId: SBWInteger;
  ModuleName: string;
  ServiceId: SBWServiceID;
  ServiceName: string;
  MethodId: SBWMethodID;
  Signature: string;
begin
  WriteLn('Discovering Math Server...');
  WriteLn;

  // List all modules - each item is a nested list [id, name]
  Modules := Client.GetListOfModules;
  try
    WriteLn(Format('Found %d connected modules:', [Modules.Count]));
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
  WriteLn;
  
  // Find the math server by name
  ServerModuleID := Client.GetModuleInstanceByName('edu.demo.mathserver');
  if ServerModuleID = SBW_NO_MODULE_ID then
  begin
    WriteLn('ERROR: Math server not found! Make sure it is running.');
    Halt(1);
  end;
  WriteLn(Format('Math Server found at module ID %d', [ServerModuleID]));
  
  // Get services - each item is a nested list [id, name]
  Services := Client.GetServiceIds(ServerModuleID);
  try
    WriteLn(Format('  Services: %d', [Services.Count]));
    MathServiceID := -1;
    for I := 0 to Services.Count - 1 do
    begin
      Item := Services[I].GetList;
      ServiceId := Item[0].GetInteger;
      ServiceName := Item[1].GetString;
      WriteLn(Format('    [%d] %s', [ServiceId, ServiceName]));
      if ServiceName = 'math' then
        MathServiceID := ServiceId;
    end;
  finally
    Services.Free;
  end;
  
  if MathServiceID < 0 then
  begin
    WriteLn('ERROR: Math service not found!');
    Halt(1);
  end;
  
  // Get methods - each item is a nested list [id, signature]
  Methods := Client.GetMethodIds(ServerModuleID, MathServiceID);
  try
    WriteLn(Format('  Methods: %d', [Methods.Count]));
    
    SinMethodID := -1;
    CosMethodID := -1;
    AddMethodID := -1;
    SumMethodID := -1;
    
    for I := 0 to Methods.Count - 1 do
    begin
      Item := Methods[I].GetList;
      MethodId := Item[0].GetInteger;
      Signature := Item[1].GetString;
      WriteLn(Format('    [%d] %s', [MethodId, Signature]));
      
      // Match by method name in signature
      if Pos('sin(', Signature) > 0 then
        SinMethodID := MethodId
      else if Pos('cos(', Signature) > 0 then
        CosMethodID := MethodId
      else if Pos('add(', Signature) > 0 then
        AddMethodID := MethodId
      else if Pos('sum(', Signature) > 0 then
        SumMethodID := MethodId;
    end;
  finally
    Methods.Free;
  end;
  
  WriteLn;
  WriteLn(Format('Resolved: sin=%d, cos=%d, add=%d, sum=%d', 
    [SinMethodID, CosMethodID, AddMethodID, SumMethodID]));
  WriteLn;
end;

procedure RunTests;
var
  R: Double;
  I: Integer;
begin
  WriteLn('=== Calling Math Server Methods ===');
  WriteLn;

  // Test sin
  Write('sin(pi/2) = ');
  R := CallSin(Pi / 2);
  WriteLn(Format('%g', [R]));

  Write('sin(0) = ');
  R := CallSin(0);
  WriteLn(Format('%g', [R]));

  Write('sin(pi) = ');
  R := CallSin(Pi);
  WriteLn(Format('%g', [R]));

  WriteLn;

  // Test cos
  Write('cos(0) = ');
  R := CallCos(0);
  WriteLn(Format('%g', [R]));

  Write('cos(pi/2) = ');
  R := CallCos(Pi / 2);
  WriteLn(Format('%g', [R]));

  Write('cos(pi) = ');
  R := CallCos(Pi);
  WriteLn(Format('%g', [R]));

  WriteLn;

  // Test add
  Write('add(3.5, 2.5) = ');
  R := CallAdd(3.5, 2.5);
  WriteLn(Format('%g', [R]));

  Write('add(-10, 10) = ');
  R := CallAdd(-10, 10);
  WriteLn(Format('%g', [R]));

  WriteLn;

  // Test sum
  Write('sum([1, 2, 3, 4, 5]) = ');
  I := CallSum([1, 2, 3, 4, 5]);
  WriteLn(I);

  Write('sum([1..10]) = ');
  I := CallSum([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
  WriteLn(I);

  WriteLn;
  WriteLn('=== All tests completed! ===');
end;

begin
  try
    WriteLn('=================================');
    WriteLn('  SBW Math Client (Discovery)');
    WriteLn('=================================');
    WriteLn;

    // Parse command line for host:port
    Host := '127.0.0.1';
    Port := SBW_DEFAULT_PORT;
    
    if ParamCount >= 1 then
      Host := ParamStr(1);
    if ParamCount >= 2 then
      Port := StrToIntDef(ParamStr(2), SBW_DEFAULT_PORT);

    try
      Client := TSBWModuleImpl.Create(
        'edu.demo.mathclient',
        'Math Client',
        mmtSelfManaged);
      try
        WriteLn(Format('Connecting to broker at %s:%d...', [Host, Port]));
        Client.Connect(Host, Port);
        WriteLn(Format('Connected! Client Module ID = %d', [Client.GetModuleID]));
        WriteLn;

        Writeln('Broker Version Number: ', Client.GetVersion);
        Writeln;
        // Discover the math server
        DiscoverMathServer;

        // Run the tests
        RunTests;

        WriteLn;
        WriteLn('Disconnecting...');
        Client.Disconnect;
      finally
        Client.Free;
      end;
    finally
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
