program SBWMathServer;

{$APPTYPE CONSOLE}

{******************************************************************************
 * SBWMathServer.dpr
 *
 * Math Server module for SBW.
 *
 * Provides mathematical functions: sin, cos, add, sum
 *
 * Start the broker first, then run this server.
 * It will connect and wait for incoming method calls.
 *****************************************************************************}

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  SBW.Module in 'SBW.Module.pas',
  SBW.Service in 'SBW.Service.pas',
  SBW.Tests.DataBlock in 'SBW.Tests.DataBlock.pas',
  SBW.Tests.Message in 'SBW.Tests.Message.pas',
  SBW.Tests.Service in 'SBW.Tests.Service.pas',
  SBW.Tests.Signature in 'SBW.Tests.Signature.pas',
  SBW.Types in 'SBW.Types.pas',
  SBW.Connection in 'SBW.Connection.pas';

var
  MathServer: TSBWModuleImpl;
  MathService: TSBWService;
  Host: string;
  Port: Word;

procedure SetupMathServer;
begin
  WriteLn('Registering math service methods...');

  MathServer := TSBWModuleImpl.Create(
    'edu.demo.mathserver',
    'Math Server',
    mmtUnique);

  // Add the math service
  MathService := MathServer.AddService('math', 'Math Functions', 'Math');

  // sin(x)
  MathService.AddMethod('double sin(double x)',
    TSBWMethodBuilder.DoubleToDouble(
      function(X: Double): Double
      begin
        Result := System.Sin(X);
        WriteLn(Format('  sin(%g) = %g', [X, Result]));
      end),
    'Calculate sine of x');

  // cos(x)
  MathService.AddMethod('double cos(double x)',
    TSBWMethodBuilder.DoubleToDouble(
      function(X: Double): Double
      begin
        Result := System.Cos(X);
        WriteLn(Format('  cos(%g) = %g', [X, Result]));
      end),
    'Calculate cosine of x');

  // add(a, b)
  MathService.AddMethod('double add(double a, double b)',
    TSBWMethodBuilder.DoubleDoubleToDouble(
      function(A, B: Double): Double
      begin
        Result := A + B;
        WriteLn(Format('  add(%g, %g) = %g', [A, B, Result]));
      end),
    'Add two numbers');

  // sum(values[])
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
        WriteLn(Format('  sum(array of %d elements) = %d', [Length(Values), Result]));
      end),
    'Sum an array of integers');

  WriteLn(Format('Registered %d methods on service "%s"',
    [MathService.Methods.Count, MathService.Name]));
end;

begin
  try
    WriteLn('=================================');
    WriteLn('  SBW Math Server');
    WriteLn('=================================');
    WriteLn;

    // Parse command line for host:port (default 127.0.0.1:10102)
    Host := '127.0.0.1';
    Port := SBW_DEFAULT_PORT;

    if ParamCount >= 1 then
      Host := ParamStr(1);
    if ParamCount >= 2 then
      Port := StrToIntDef(ParamStr(2), SBW_DEFAULT_PORT);

    InitializeSockets;
    try
      SetupMathServer;

      WriteLn;
      WriteLn(Format('Connecting to broker at %s:%d...', [Host, Port]));
      MathServer.Connect(Host, Port);
      WriteLn(Format('Connected! Module ID = %d', [MathServer.GetModuleID]));
      WriteLn;
      WriteLn('Waiting for incoming calls... (Ctrl+C to stop)');
      WriteLn;

      // Run the receive loop (blocks until disconnected)
      MathServer.Run;

      WriteLn('Disconnected from broker.');
      MathServer.Free;
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

