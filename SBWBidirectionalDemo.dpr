program SBWBidirectionalDemo;

{$APPTYPE CONSOLE}

{******************************************************************************
 * SBWBidirectionalDemo.dpr
 *
 * Demonstrates bidirectional communication in SBW.
 * This module both:
 *   - Serves a "greeter" service with a greet(name) method
 *   - Calls the MathServer's methods
 *
 * This shows how a single module can be both a server AND a client.
 *
 * Usage:
 *   1. Start the Broker
 *   2. Start the MathServer
 *   3. Start this BidirectionalDemo
 *   4. From another client, call this module's greet method
 *      OR watch this module call the MathServer
 *****************************************************************************}

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  SBW.Types in '..\SBW.Types.pas',
  SBW.DataBlock in '..\SBW.DataBlock.pas',
  SBW.Message in '..\SBW.Message.pas',
  SBW.Signature in '..\SBW.Signature.pas',
  SBW.Service in '..\SBW.Service.pas',
  SBW.Connection in '..\SBW.Connection.pas',
  SBW.List in '..\SBW.List.pas',
  SBW.Module in '..\SBW.Module.pas';

var
  Module: TSBWModuleImpl;
  Host: string;
  Port: Word;
  MathServerID: SBWModuleID;

// Our service handler for greet(string name) -> string
function HandleGreet(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;
var
  Name: string;
begin
  Name := Args.ReadString;
  WriteLn(Format('[Greeter] Received greet call from module %d with name: %s', [FromModuleID, Name]));

  Result := TSBWDataBlockWriter.Create;
  Result.WriteString(Format('Hello, %s! Greetings from the bidirectional module.', [Name]));
end;

// Call the MathServer's sin method
function CallMathSin(X: Double): Double;
var
  Args: TSBWDataBlockWriter;
  ResultReader: TSBWDataBlockReader;
begin
  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteDouble(X);
    // MathServer: service 0, method 0 = sin
    ResultReader := Module.CallMethod(MathServerID, 0, 0, Args);
    try
      Result := ResultReader.ReadDouble;
    finally
      ResultReader.Free;
    end;
  finally
    Args.Free;
  end;
end;

// Call the MathServer's add method
function CallMathAdd(A, B: Double): Double;
var
  Args: TSBWDataBlockWriter;
  ResultReader: TSBWDataBlockReader;
begin
  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteDouble(A);
    Args.WriteDouble(B);
    // MathServer: service 0, method 2 = add
    ResultReader := Module.CallMethod(MathServerID, 0, 2, Args);
    try
      Result := ResultReader.ReadDouble;
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
  I: Integer;
  Item: TSBWList;
begin
  WriteLn('Looking for MathServer...');

  // Find the math server
  MathServerID := Module.GetModuleIdByName('edu.demo.mathserver');
  if MathServerID = SBW_NO_MODULE_ID then
  begin
    WriteLn('MathServer not found. Math calls will fail.');
    WriteLn('Start the MathServer to enable math operations.');
  end
  else
    WriteLn(Format('Found MathServer at module ID %d', [MathServerID]));

  WriteLn;
end;

procedure TestMathCalls;
var
  R: Double;
begin
  if MathServerID = SBW_NO_MODULE_ID then
  begin
    WriteLn('Cannot test math calls - MathServer not available.');
    Exit;
  end;

  WriteLn('=== Testing outgoing calls to MathServer ===');
  WriteLn;

  Write('Calling sin(pi/2) = ');
  R := CallMathSin(Pi / 2);
  WriteLn(Format('%g', [R]));

  Write('Calling sin(0) = ');
  R := CallMathSin(0);
  WriteLn(Format('%g', [R]));

  Write('Calling add(10, 25) = ');
  R := CallMathAdd(10, 25);
  WriteLn(Format('%g', [R]));

  Write('Calling add(3.14, 2.86) = ');
  R := CallMathAdd(3.14, 2.86);
  WriteLn(Format('%g', [R]));

  WriteLn;
  WriteLn('Math calls completed successfully!');
  WriteLn;
end;

begin
  try
    WriteLn('==========================================');
    WriteLn('  SBW Bidirectional Demo');
    WriteLn('==========================================');
    WriteLn;
    WriteLn('This module:');
    WriteLn('  - SERVES a "greeter" service (can be called by others)');
    WriteLn('  - CALLS the MathServer (acts as a client)');
    WriteLn;

    // Parse command line
    Host := '127.0.0.1';
    Port := SBW_DEFAULT_PORT;

    if ParamCount >= 1 then
      Host := ParamStr(1);
    if ParamCount >= 2 then
      Port := StrToIntDef(ParamStr(2), SBW_DEFAULT_PORT);

    InitializeSockets;
    try
      Module := TSBWModuleImpl.Create(
        'edu.demo.bidirectional',
        'Bidirectional Demo',
        mmtSelfManaged);
      try
        // Add our greeter service
        with Module.AddService('greeter', 'Greeter Service', 'Demo') do
        begin
          AddMethod('string greet(string name)', HandleGreet,
            'Returns a greeting for the given name');
        end;

        WriteLn(Format('Connecting to broker at %s:%d...', [Host, Port]));

        // Connect in BIDIRECTIONAL mode - this starts the receive thread
        Module.Connect(Host, Port, True);  // <-- True = bidirectional

        WriteLn(Format('Connected! Module ID = %d', [Module.GetModuleID]));
        WriteLn;

        // Discover other modules
        DiscoverMathServer;

        // Test calling the MathServer (while we can still receive calls!)
        TestMathCalls;

        WriteLn('==========================================');
        WriteLn('Module is now running in bidirectional mode.');
        WriteLn('- Other modules can call our greet(name) method');
        WriteLn('- We can call other modules at any time');
        WriteLn;
        WriteLn('Press Enter to exit...');
        WriteLn('==========================================');

        // We could do more work here - the receive thread handles incoming calls
        // in the background. For this demo, we just wait for user input.
        ReadLn;

        WriteLn;
        WriteLn('Disconnecting...');
        Module.Disconnect;
      finally
        Module.Free;
      end;
    finally
      FinalizeSockets;
    end;

  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end.

