program SBWModuleDemo;

{$APPTYPE CONSOLE}

{******************************************************************************
 * SBWModuleDemo.dpr
 *
 * Demonstration of SBW module-to-module communication.
 *
 * This demo:
 * 1. Starts a broker
 * 2. Creates a "math server" module with sin, cos, add methods
 * 3. Creates a "client" module that calls the math server
 *
 * All in one process for simplicity, but they communicate through the broker.
 *****************************************************************************}

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.SyncObjs,
  SBW.Types in '..\SBW.Types.pas',
  SBW.DataBlock in '..\SBW.DataBlock.pas',
  SBW.Message in '..\SBW.Message.pas',
  SBW.Signature in '..\SBW.Signature.pas',
  SBW.Service in '..\SBW.Service.pas',
  SBW.Connection in '..\SBW.Connection.pas',
  SBW.Broker in '..\SBW.Broker.pas',
  SBW.Module in '..\SBW.Module.pas';

type
  TDemoEventHandler = class
    procedure OnLog(Sender: TObject; const Msg: string);
  end;

procedure TDemoEventHandler.OnLog(Sender: TObject; const Msg: string);
begin
  WriteLn('[Broker] ', Msg);
end;

var
  Broker: TSBWBroker;
  MathServer: TSBWModuleImpl;
  Client: TSBWModuleImpl;
  EventHandler: TDemoEventHandler;
  MathService: TSBWService;
  ServerThread: TThread;

procedure SetupMathServer;
begin
  WriteLn('Setting up Math Server module...');

  MathServer := TSBWModuleImpl.Create(
    'edu.demo.mathserver',
    'Math Server',
    mmtUnique);

  // Add the math service
  MathService := MathServer.AddService('math', 'Math Functions', 'Math');

  // Add methods using the method builder helpers
  MathService.AddMethod('double sin(double x)',
    TSBWMethodBuilder.DoubleToDouble(
      function(X: Double): Double
      begin
        Result := System.Sin(X);
        WriteLn(Format('[MathServer] sin(%g) = %g', [X, Result]));
      end),
    'Calculate sine of x');

  MathService.AddMethod('double cos(double x)',
    TSBWMethodBuilder.DoubleToDouble(
      function(X: Double): Double
      begin
        Result := System.Cos(X);
        WriteLn(Format('[MathServer] cos(%g) = %g', [X, Result]));
      end),
    'Calculate cosine of x');

  MathService.AddMethod('double add(double a, double b)',
    TSBWMethodBuilder.DoubleDoubleToDouble(
      function(A, B: Double): Double
      begin
        Result := A + B;
        WriteLn(Format('[MathServer] add(%g, %g) = %g', [A, B, Result]));
      end),
    'Add two numbers');

  MathService.AddMethod('int sum(int[] values)',
    TSBWMethodBuilder.IntArrayToInt(
      function(Values: TArray<Integer>): Integer
      var
        I, S: Integer;
      begin
        S := 0;
        for I in Values do
          S := S + I;
        Result := S;
        WriteLn(Format('[MathServer] sum(array of %d elements) = %d', [Length(Values), Result]));
      end),
    'Sum an array of integers');

  WriteLn(Format('Math Server has %d methods', [MathService.Methods.Count]));
end;

procedure RunMathServer;
begin
  MathServer.Connect('127.0.0.1', SBW_DEFAULT_PORT);
  WriteLn(Format('[MathServer] Connected with module ID %d', [MathServer.GetModuleID]));

  // Run the receive loop (this blocks)
  MathServer.Run;
end;

procedure RunClient;
var
  Args: TSBWDataBlockWriter;
  ResultReader: TSBWDataBlockReader;
  ResultValue: Double;
  IntResult: Integer;
  ServerModuleID: SBWModuleID;
begin
  WriteLn;
  WriteLn('Setting up Client module...');

  Client := TSBWModuleImpl.Create(
    'edu.demo.client',
    'Demo Client',
    mmtSelfManaged);

  Client.Connect('127.0.0.1', SBW_DEFAULT_PORT);
  WriteLn(Format('[Client] Connected with module ID %d', [Client.GetModuleID]));

  // The math server should be module ID 1
  ServerModuleID := 1;

  WriteLn;
  WriteLn('=== Calling Math Server Methods ===');
  WriteLn;

  // Call sin(pi/2)
  Write('[Client] Calling sin(pi/2)... ');
  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteDouble(Pi / 2);
    ResultReader := Client.CallMethod(ServerModuleID, 0, 0, Args);
    try
      ResultValue := ResultReader.ReadDouble;
      WriteLn(Format('Result: %g', [ResultValue]));
    finally
      ResultReader.Free;
    end;
  finally
    Args.Free;
  end;

  // Call cos(0)
  Write('[Client] Calling cos(0)... ');
  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteDouble(0);
    ResultReader := Client.CallMethod(ServerModuleID, 0, 1, Args);
    try
      ResultValue := ResultReader.ReadDouble;
      WriteLn(Format('Result: %g', [ResultValue]));
    finally
      ResultReader.Free;
    end;
  finally
    Args.Free;
  end;

  // Call add(3.5, 2.5)
  Write('[Client] Calling add(3.5, 2.5)... ');
  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteDouble(3.5);
    Args.WriteDouble(2.5);
    ResultReader := Client.CallMethod(ServerModuleID, 0, 2, Args);
    try
      ResultValue := ResultReader.ReadDouble;
      WriteLn(Format('Result: %g', [ResultValue]));
    finally
      ResultReader.Free;
    end;
  finally
    Args.Free;
  end;

  // Call sum([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  Write('[Client] Calling sum([1..10])... ');
  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteIntegerArray([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    ResultReader := Client.CallMethod(ServerModuleID, 0, 3, Args);
    try
      IntResult := ResultReader.ReadInteger;
      WriteLn(Format('Result: %d', [IntResult]));
    finally
      ResultReader.Free;
    end;
  finally
    Args.Free;
  end;

  WriteLn;
  WriteLn('=== All calls completed successfully! ===');

  Client.Disconnect;
end;

begin
  try
    WriteLn('SBW Module Communication Demo');
    WriteLn('==============================');
    WriteLn;

    InitializeSockets;
    try
      // Start the broker
      WriteLn('Starting broker...');
      EventHandler := TDemoEventHandler.Create;
      Broker := TSBWBroker.Create;
      try
        Broker.OnLog := EventHandler.OnLog;
        Broker.Start(SBW_DEFAULT_PORT);
        WriteLn(Format('Broker started on port %d', [Broker.Port]));
        WriteLn;

        // Setup the math server
        SetupMathServer;

        // Run the math server in a background thread
        ServerThread := TThread.CreateAnonymousThread(RunMathServer);
        ServerThread.FreeOnTerminate := False;
        ServerThread.Start;

        // Give server time to connect
        Sleep(500);

        // Run the client (this makes the calls)
        RunClient;

        // Cleanup
        WriteLn;
        WriteLn('Shutting down...');

        MathServer.SignalDisconnect;
        ServerThread.WaitFor;
        ServerThread.Free;
        MathServer.Free;

        Broker.Stop;
      finally
        Broker.Free;
        EventHandler.Free;
      end;
    finally
      FinalizeSockets;
    end;

    WriteLn;
    WriteLn('Demo completed. Press Enter to exit...');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end.

