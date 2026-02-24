unit SBW.Tests.Service;

{******************************************************************************
 * SBW.Tests.Service.pas
 *
 * Unit tests for SBW service and method infrastructure.
 *****************************************************************************}

interface

uses
  System.SysUtils, System.Math,
  SBW.Types, SBW.DataBlock, SBW.Signature, SBW.Service;

type
  TServiceTests = class
  private
    FTestCount: Integer;
    FPassCount: Integer;
    FFailCount: Integer;

    procedure Check(Condition: Boolean; const TestName: string);
    procedure CheckEquals(Expected, Actual: Integer; const TestName: string); overload;
    procedure CheckEquals(const Expected, Actual: string; const TestName: string); overload;
    procedure CheckEquals(Expected, Actual: Double; const TestName: string; Epsilon: Double = 1E-10); overload;
  public
    constructor Create;

    procedure TestCreateService;
    procedure TestAddMethod;
    procedure TestFindMethod;
    procedure TestFindMethodByID;
    procedure TestServiceRegistry;
    procedure TestFindServicesByCategory;
    procedure TestMethodInvocation;
    procedure TestMethodBuilderDoubleToDouble;
    procedure TestMethodBuilderDoubleDoubleToDouble;
    procedure TestMethodBuilderVoidToInteger;
    procedure TestMethodBuilderArrayToScalar;

    procedure RunAllTests;

    property TestCount: Integer read FTestCount;
    property PassCount: Integer read FPassCount;
    property FailCount: Integer read FFailCount;
  end;

procedure RunServiceTests;

implementation

procedure RunServiceTests;
var
  Tests: TServiceTests;
begin
  Tests := TServiceTests.Create;
  try
    Tests.RunAllTests;
    WriteLn;
    WriteLn(Format('Service Tests: %d tests, %d passed, %d failed',
      [Tests.TestCount, Tests.PassCount, Tests.FailCount]));
  finally
    Tests.Free;
  end;
end;

{ TServiceTests }

constructor TServiceTests.Create;
begin
  inherited Create;
  FTestCount := 0;
  FPassCount := 0;
  FFailCount := 0;
end;

procedure TServiceTests.Check(Condition: Boolean; const TestName: string);
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

procedure TServiceTests.CheckEquals(Expected, Actual: Integer; const TestName: string);
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

procedure TServiceTests.CheckEquals(const Expected, Actual: string; const TestName: string);
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

procedure TServiceTests.CheckEquals(Expected, Actual: Double; const TestName: string; Epsilon: Double);
begin
  Inc(FTestCount);
  if Abs(Expected - Actual) < Epsilon then
  begin
    Inc(FPassCount);
    WriteLn('  [PASS] ' + TestName);
  end
  else
  begin
    Inc(FFailCount);
    WriteLn(Format('  [FAIL] %s: expected %g, got %g', [TestName, Expected, Actual]));
  end;
end;

procedure TServiceTests.TestCreateService;
var
  Service: TSBWService;
begin
  WriteLn('TestCreateService');

  Service := TSBWService.Create('TestService', 'Test Service', 'Testing');
  try
    CheckEquals('TestService', Service.Name, 'Service name');
    CheckEquals('Test Service', Service.DisplayName, 'Display name');
    CheckEquals('Testing', Service.Category, 'Category');
    CheckEquals(0, Service.Methods.Count, 'No methods initially');
  finally
    Service.Free;
  end;
end;

procedure TServiceTests.TestAddMethod;
var
  Service: TSBWService;
  Method: TSBWServiceMethod;
begin
  WriteLn('TestAddMethod');

  Service := TSBWService.Create('MathService', 'Math Service', 'Math');
  try
    Method := Service.AddMethod('double sin(double x)',
      function(FromID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter
      begin
        Result := TSBWDataBlockWriter.Create;
      end);

    Check(Method <> nil, 'Method created');
    CheckEquals('sin', Method.Name, 'Method name');
    CheckEquals('double sin(double x)', Method.SignatureString, 'Signature string');
    CheckEquals(1, Service.Methods.Count, 'One method');
  finally
    Service.Free;
  end;
end;

procedure TServiceTests.TestFindMethod;
var
  Service: TSBWService;
  Method: TSBWServiceMethod;
begin
  WriteLn('TestFindMethod');

  Service := TSBWService.Create('MathService', 'Math Service', 'Math');
  try
    Service.AddMethod('double sin(double x)', nil);
    Service.AddMethod('double cos(double x)', nil);
    Service.AddMethod('double add(double a, double b)', nil);

    Method := Service.FindMethod('sin');
    Check(Method <> nil, 'Found sin');
    CheckEquals('sin', Method.Name, 'sin name');

    Method := Service.FindMethod('cos');
    Check(Method <> nil, 'Found cos');

    Method := Service.FindMethod('add');
    Check(Method <> nil, 'Found add');

    Method := Service.FindMethod('tan');
    Check(Method = nil, 'tan not found');
  finally
    Service.Free;
  end;
end;

procedure TServiceTests.TestFindMethodByID;
var
  Service: TSBWService;
  Method1, Method2, Found: TSBWServiceMethod;
begin
  WriteLn('TestFindMethodByID');

  Service := TSBWService.Create('TestService', 'Test', 'Test');
  try
    Method1 := Service.AddMethod('void method1()', nil);
    Method2 := Service.AddMethod('void method2()', nil);

    Found := Service.FindMethodByID(Method1.MethodID);
    Check(Found = Method1, 'Found method1 by ID');

    Found := Service.FindMethodByID(Method2.MethodID);
    Check(Found = Method2, 'Found method2 by ID');

    Found := Service.FindMethodByID(999);
    Check(Found = nil, 'Invalid ID returns nil');
  finally
    Service.Free;
  end;
end;

procedure TServiceTests.TestServiceRegistry;
var
  Registry: TSBWServiceRegistry;
  Service1, Service2, Found: TSBWService;
begin
  WriteLn('TestServiceRegistry');

  Registry := TSBWServiceRegistry.Create;
  try
    Service1 := Registry.AddService('Service1', 'Service One', 'Category1');
    Service2 := Registry.AddService('Service2', 'Service Two', 'Category2');

    CheckEquals(2, Registry.Services.Count, 'Two services');

    Found := Registry.FindService('Service1');
    Check(Found = Service1, 'Found Service1');

    Found := Registry.FindService('Service2');
    Check(Found = Service2, 'Found Service2');

    Found := Registry.FindService('Service3');
    Check(Found = nil, 'Service3 not found');

    Found := Registry.FindServiceByID(Service1.ServiceID);
    Check(Found = Service1, 'Found Service1 by ID');
  finally
    Registry.Free;
  end;
end;

procedure TServiceTests.TestFindServicesByCategory;
var
  Registry: TSBWServiceRegistry;
  Services: TArray<TSBWService>;
begin
  WriteLn('TestFindServicesByCategory');

  Registry := TSBWServiceRegistry.Create;
  try
    Registry.AddService('Sim1', 'Simulator 1', 'Analysis/Simulation');
    Registry.AddService('Sim2', 'Simulator 2', 'Analysis/Simulation');
    Registry.AddService('Opt1', 'Optimizer 1', 'Analysis/Optimization');
    Registry.AddService('Vis1', 'Visualizer 1', 'Visualization');

    // Exact category match
    Services := Registry.FindServicesByCategory('Visualization', False);
    CheckEquals(1, Length(Services), 'One visualization service');

    // Recursive search
    Services := Registry.FindServicesByCategory('Analysis', True);
    CheckEquals(3, Length(Services), 'Three analysis services (recursive)');

    // Exact subcategory
    Services := Registry.FindServicesByCategory('Analysis/Simulation', False);
    CheckEquals(2, Length(Services), 'Two simulation services');
  finally
    Registry.Free;
  end;
end;

procedure TServiceTests.TestMethodInvocation;
var
  Service: TSBWService;
  Method: TSBWServiceMethod;
  Args: TSBWDataBlockWriter;
  ArgsReader: TSBWDataBlockReader;
  ResultWriter: TSBWDataBlockWriter;
  ResultReader: TSBWDataBlockReader;
  ResultValue: Double;
begin
  WriteLn('TestMethodInvocation');

  Service := TSBWService.Create('MathService', 'Math', 'Math');
  try
    Method := Service.AddMethod('double square(double x)',
      function(FromID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter
      var
        X: Double;
      begin
        X := Args.ReadDouble;
        Result := TSBWDataBlockWriter.Create;
        Result.WriteDouble(X * X);
      end);

    // Create arguments
    Args := TSBWDataBlockWriter.Create;
    try
      Args.WriteDouble(5.0);

      ArgsReader := TSBWDataBlockReader.Create(Args.ToBytes);
      try
        ResultWriter := Method.Invoke(1, ArgsReader);
        try
          ResultReader := TSBWDataBlockReader.Create(ResultWriter.ToBytes);
          try
            ResultValue := ResultReader.ReadDouble;
            CheckEquals(25.0, ResultValue, 'square(5.0) = 25.0');
          finally
            ResultReader.Free;
          end;
        finally
          ResultWriter.Free;
        end;
      finally
        ArgsReader.Free;
      end;
    finally
      Args.Free;
    end;
  finally
    Service.Free;
  end;
end;

procedure TServiceTests.TestMethodBuilderDoubleToDouble;
var
  Handler: TSBWMethodHandler;
  Args: TSBWDataBlockWriter;
  ArgsReader: TSBWDataBlockReader;
  ResultWriter: TSBWDataBlockWriter;
  ResultReader: TSBWDataBlockReader;
  ResultValue: Double;
begin
  WriteLn('TestMethodBuilderDoubleToDouble');

  Handler := TSBWMethodBuilder.DoubleToDouble(
    function(X: Double): Double
    begin
      Result := Sin(X);
    end);

  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteDouble(Pi / 2);

    ArgsReader := TSBWDataBlockReader.Create(Args.ToBytes);
    try
      ResultWriter := Handler(1, ArgsReader);
      try
        ResultReader := TSBWDataBlockReader.Create(ResultWriter.ToBytes);
        try
          ResultValue := ResultReader.ReadDouble;
          CheckEquals(1.0, ResultValue, 'sin(pi/2) = 1.0', 1E-10);
        finally
          ResultReader.Free;
        end;
      finally
        ResultWriter.Free;
      end;
    finally
      ArgsReader.Free;
    end;
  finally
    Args.Free;
  end;
end;

procedure TServiceTests.TestMethodBuilderDoubleDoubleToDouble;
var
  Handler: TSBWMethodHandler;
  Args: TSBWDataBlockWriter;
  ArgsReader: TSBWDataBlockReader;
  ResultWriter: TSBWDataBlockWriter;
  ResultReader: TSBWDataBlockReader;
  ResultValue: Double;
begin
  WriteLn('TestMethodBuilderDoubleDoubleToDouble');

  Handler := TSBWMethodBuilder.DoubleDoubleToDouble(
    function(A, B: Double): Double
    begin
      Result := A + B;
    end);

  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteDouble(3.0);
    Args.WriteDouble(4.0);

    ArgsReader := TSBWDataBlockReader.Create(Args.ToBytes);
    try
      ResultWriter := Handler(1, ArgsReader);
      try
        ResultReader := TSBWDataBlockReader.Create(ResultWriter.ToBytes);
        try
          ResultValue := ResultReader.ReadDouble;
          CheckEquals(7.0, ResultValue, 'add(3, 4) = 7');
        finally
          ResultReader.Free;
        end;
      finally
        ResultWriter.Free;
      end;
    finally
      ArgsReader.Free;
    end;
  finally
    Args.Free;
  end;
end;

procedure TServiceTests.TestMethodBuilderVoidToInteger;
var
  Handler: TSBWMethodHandler;
  Counter: Integer;
  Args: TSBWDataBlockWriter;
  ArgsReader: TSBWDataBlockReader;
  ResultWriter: TSBWDataBlockWriter;
  ResultReader: TSBWDataBlockReader;
  ResultValue: Integer;
begin
  WriteLn('TestMethodBuilderVoidToInteger');

  Counter := 42;

  Handler := TSBWMethodBuilder.VoidToInteger(
    function: Integer
    begin
      Result := Counter;
    end);

  Args := TSBWDataBlockWriter.Create;
  try
    ArgsReader := TSBWDataBlockReader.Create(Args.ToBytes);
    try
      ResultWriter := Handler(1, ArgsReader);
      try
        ResultReader := TSBWDataBlockReader.Create(ResultWriter.ToBytes);
        try
          ResultValue := ResultReader.ReadInteger;
          CheckEquals(42, ResultValue, 'getCounter() = 42');
        finally
          ResultReader.Free;
        end;
      finally
        ResultWriter.Free;
      end;
    finally
      ArgsReader.Free;
    end;
  finally
    Args.Free;
  end;
end;

procedure TServiceTests.TestMethodBuilderArrayToScalar;
var
  Handler: TSBWMethodHandler;
  Args: TSBWDataBlockWriter;
  ArgsReader: TSBWDataBlockReader;
  ResultWriter: TSBWDataBlockWriter;
  ResultReader: TSBWDataBlockReader;
  ResultValue: Integer;
begin
  WriteLn('TestMethodBuilderArrayToScalar');

  Handler := TSBWMethodBuilder.IntArrayToInt(
    function(Arr: TArray<Integer>): Integer
    var
      I, Sum: Integer;
    begin
      Sum := 0;
      for I in Arr do
        Sum := Sum + I;
      Result := Sum;
    end);

  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteIntegerArray([1, 2, 3, 4, 5]);

    ArgsReader := TSBWDataBlockReader.Create(Args.ToBytes);
    try
      ResultWriter := Handler(1, ArgsReader);
      try
        ResultReader := TSBWDataBlockReader.Create(ResultWriter.ToBytes);
        try
          ResultValue := ResultReader.ReadInteger;
          CheckEquals(15, ResultValue, 'sum([1,2,3,4,5]) = 15');
        finally
          ResultReader.Free;
        end;
      finally
        ResultWriter.Free;
      end;
    finally
      ArgsReader.Free;
    end;
  finally
    Args.Free;
  end;
end;

procedure TServiceTests.RunAllTests;
begin
  WriteLn('=== SBW Service Tests ===');
  WriteLn;

  TestCreateService;
  TestAddMethod;
  TestFindMethod;
  TestFindMethodByID;
  TestServiceRegistry;
  TestFindServicesByCategory;
  TestMethodInvocation;
  TestMethodBuilderDoubleToDouble;
  TestMethodBuilderDoubleDoubleToDouble;
  TestMethodBuilderVoidToInteger;
  TestMethodBuilderArrayToScalar;
end;

end.

