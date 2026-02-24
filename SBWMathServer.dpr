program SBWMathServer;

{$APPTYPE CONSOLE}

{******************************************************************************
 * SBWMathServer.dpr
 *
 * A simple math server module for SBW.
 *
 * Provides a "math" service with the following methods:
 *   - double sin(double x)
 *   - double cos(double x)
 *   - double add(double a, double b)
 *   - int sum(int[] values)
 *
 * Start the broker first, then run this server.
 * Clients can then discover and call these methods.
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
  SBW.Module in 'SBW.Module.pas';

var
  Server: TSBWModuleImpl;
  MathService: TSBWService;
  Host: string;
  Port: Word;

// =============================================================================
// Math method implementations
// =============================================================================

function DoSin(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;
var
  X, Y: Double;
begin
  X := Args.ReadDouble;
  Y := Sin(X);
  WriteLn(Format('  sin(%g) = %g', [X, Y]));
  Result := TSBWDataBlockWriter.Create;
  Result.WriteDouble(Y);
end;

function DoCos(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;
var
  X, Y: Double;
begin
  X := Args.ReadDouble;
  Y := Cos(X);
  WriteLn(Format('  cos(%g) = %g', [X, Y]));
  Result := TSBWDataBlockWriter.Create;
  Result.WriteDouble(Y);
end;

function DoAdd(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;
var
  A, B, Y: Double;
begin
  A := Args.ReadDouble;
  B := Args.ReadDouble;
  Y := A + B;
  WriteLn(Format('  add(%g, %g) = %g', [A, B, Y]));
  Result := TSBWDataBlockWriter.Create;
  Result.WriteDouble(Y);
end;

function DoSum(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;
var
  Values: TArray<SBWInteger>;
  Total: Integer;
  I: Integer;
begin
  Values := Args.ReadIntegerArray;
  Total := 0;
  for I := 0 to High(Values) do
    Total := Total + Values[I];
  WriteLn(Format('  sum([%d values]) = %d', [Length(Values), Total]));
  Result := TSBWDataBlockWriter.Create;
  Result.WriteInteger(Total);
end;

function DoTan(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;
var
  X, Y: Double;
begin
  X := Args.ReadDouble;
  Y := Tan(X);
  WriteLn(Format('  tan(%g) = %g', [X, Y]));
  Result := TSBWDataBlockWriter.Create;
  Result.WriteDouble(Y);
end;

function DoSqrt(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;
var
  X, Y: Double;
begin
  X := Args.ReadDouble;
  Y := Sqrt(X);
  WriteLn(Format('  sqrt(%g) = %g', [X, Y]));
  Result := TSBWDataBlockWriter.Create;
  Result.WriteDouble(Y);
end;

function DoPow(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;
var
  Base, Exponent, Y: Double;
begin
  Base := Args.ReadDouble;
  Exponent := Args.ReadDouble;
  Y := Power(Base, Exponent);
  WriteLn(Format('  pow(%g, %g) = %g', [Base, Exponent, Y]));
  Result := TSBWDataBlockWriter.Create;
  Result.WriteDouble(Y);
end;

function DoAbs(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;
var
  X, Y: Double;
begin
  X := Args.ReadDouble;
  Y := Abs(X);
  WriteLn(Format('  abs(%g) = %g', [X, Y]));
  Result := TSBWDataBlockWriter.Create;
  Result.WriteDouble(Y);
end;

// =============================================================================
// Main program
// =============================================================================

begin
  try
    WriteLn('=================================');
    WriteLn('  SBW Math Server');
    WriteLn('=================================');
    WriteLn;

    // Parse command line for host:port
    Host := '127.0.0.1';
    Port := SBW_DEFAULT_PORT;

    if ParamCount >= 1 then
      Host := ParamStr(1);
    if ParamCount >= 2 then
      Port := StrToIntDef(ParamStr(2), SBW_DEFAULT_PORT);

    // Create the module
    Server := TSBWModuleImpl.Create(
      'edu.demo.mathserver',
      'Math Server',
      mmtSelfManaged,
      'A simple math server providing trigonometric and arithmetic functions');
    try
      // Add the math service
      MathService := Server.AddService(
        'math',
        'Math Service',
        'Math',
        'Provides basic mathematical operations');

      // Register methods
      MathService.AddMethod('double sin(double)', DoSin,
        'Returns the sine of x (in radians)');
      MathService.AddMethod('double cos(double)', DoCos,
        'Returns the cosine of x (in radians)');
      MathService.AddMethod('double tan(double)', DoTan,
        'Returns the tangent of x (in radians)');
      MathService.AddMethod('double add(double, double)', DoAdd,
        'Returns the sum of two numbers');
      MathService.AddMethod('int sum(int[])', DoSum,
        'Returns the sum of an array of integers');
      MathService.AddMethod('double sqrt(double)', DoSqrt,
        'Returns the square root of x');
      MathService.AddMethod('double pow(double, double)', DoPow,
        'Returns base raised to exponent power');
      MathService.AddMethod('double abs(double)', DoAbs,
        'Returns the absolute value of x');

      WriteLn(Format('Connecting to broker at %s:%d...', [Host, Port]));
      Server.Connect(Host, Port);
      WriteLn(Format('Connected! Server Module ID = %d', [Server.GetModuleID]));
      WriteLn;
      WriteLn('Math service registered with methods:');
      WriteLn('  [0] double sin(double)');
      WriteLn('  [1] double cos(double)');
      WriteLn('  [2] double tan(double)');
      WriteLn('  [3] double add(double, double)');
      WriteLn('  [4] int sum(int[])');
      WriteLn('  [5] double sqrt(double)');
      WriteLn('  [6] double pow(double, double)');
      WriteLn('  [7] double abs(double)');
      WriteLn;
      WriteLn('Server is running. Press Ctrl+C to stop.');
      WriteLn('Waiting for calls...');
      WriteLn;

      // Check for registration flag
      if FindCmdLineSwitch('register') then
      begin
        Server.RegisterModule(
          'edu.demo.mathserver',
          'Math Server',
          Ord(mmtSelfManaged),
          ParamStr(0),
          'A simple math server...');
        WriteLn('Registered for auto-launch');
      end;

      // Run the message loop
      Server.Run;

    finally
      Server.Free;
    end;

    WriteLn;
    WriteLn('Server stopped.');

  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end.
