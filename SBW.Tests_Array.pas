unit SBW.Tests_Array;

{******************************************************************************
 * SBW.Tests.Array.pas
 *
 * Unit tests for TSBWArray - homogeneous multi-dimensional array type.
 * Tests creation, indexing, serialization round-trips, and edge cases.
 *****************************************************************************}

interface

uses
  System.SysUtils, System.Math,
  SBW.Types, SBW.DataBlock, SBW_Array;

type
  TArrayTests = class
  private
    FTestCount: Integer;
    FPassCount: Integer;
    FFailCount: Integer;

    procedure Check(Condition: Boolean; const TestName: string);
    procedure CheckEquals(Expected, Actual: Integer; const TestName: string); overload;
    procedure CheckEquals(Expected, Actual: SBWDouble; const TestName: string;
      Epsilon: Double = 1E-10); overload;
    procedure CheckEquals(const Expected, Actual: string; const TestName: string); overload;
    procedure CheckEquals(Expected, Actual: Boolean; const TestName: string); overload;
  public
    constructor Create;

    procedure TestCreate1D;
    procedure TestCreate2D;
    procedure TestCreate3D;
    procedure TestCreateWithFactoryFunctions;
    procedure TestDoubleArray1DAccess;
    procedure TestIntegerArray1DAccess;
    procedure TestStringArray1DAccess;
    procedure TestDoubleArray2DAccess;
    procedure TestSetFromDoubleArray;
    procedure TestSetFromDoubleArray2D;
    procedure TestAsDoubleArray2D;
    procedure TestReshape;
    procedure TestDoubleArrayRoundTrip;
    procedure TestIntegerArrayRoundTrip;
    procedure TestStringArrayRoundTrip;
    procedure TestDoubleArray2DRoundTrip;
    procedure TestArray3DRoundTrip;
    procedure TestEmptyArrayRoundTrip;
    procedure TestClone;
    procedure TestClear;
    procedure TestFill;
    procedure TestIdentityMatrix;
    procedure TestZeroMatrix;
    procedure TestTypeMismatchException;
    procedure TestIndexOutOfRangeException;

    procedure RunAllTests;

    property TestCount: Integer read FTestCount;
    property PassCount: Integer read FPassCount;
    property FailCount: Integer read FFailCount;
  end;

procedure RunAllArrayTests;

implementation

procedure RunAllArrayTests;
var
  Tests: TArrayTests;
begin
  Tests := TArrayTests.Create;
  try
    Tests.RunAllTests;
    WriteLn;
    WriteLn(Format('Results: %d tests, %d passed, %d failed',
      [Tests.TestCount, Tests.PassCount, Tests.FailCount]));
    if Tests.FailCount = 0 then
      WriteLn('All tests passed!')
    else
      WriteLn('Some tests FAILED!');
  finally
    Tests.Free;
  end;
end;

{ TArrayTests }

constructor TArrayTests.Create;
begin
  inherited Create;
  FTestCount := 0;
  FPassCount := 0;
  FFailCount := 0;
end;

procedure TArrayTests.Check(Condition: Boolean; const TestName: string);
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

procedure TArrayTests.CheckEquals(Expected, Actual: Integer; const TestName: string);
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

procedure TArrayTests.CheckEquals(Expected, Actual: SBWDouble; 
  const TestName: string; Epsilon: Double);
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

procedure TArrayTests.CheckEquals(const Expected, Actual: string; 
  const TestName: string);
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

procedure TArrayTests.CheckEquals(Expected, Actual: Boolean; const TestName: string);
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
    WriteLn(Format('  [FAIL] %s: expected %s, got %s',
      [TestName, BoolToStr(Expected, True), BoolToStr(Actual, True)]));
  end;
end;

procedure TArrayTests.TestCreate1D;
var
  Arr: TSBWArray;
begin
  WriteLn('TestCreate1D');
  Arr := TSBWArray.Create(dbtDouble, 5);
  try
    CheckEquals(1, Arr.DimensionCount, '1D dimension count');
    CheckEquals(5, Arr.Size(0), '1D size');
    CheckEquals(5, Arr.TotalElements, '1D total elements');
    Check(Arr.ElementType = dbtDouble, '1D element type is Double');
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.TestCreate2D;
var
  Arr: TSBWArray;
begin
  WriteLn('TestCreate2D');
  Arr := TSBWArray.Create(dbtInteger, [3, 4]);
  try
    CheckEquals(2, Arr.DimensionCount, '2D dimension count');
    CheckEquals(3, Arr.Rows, '2D rows');
    CheckEquals(4, Arr.Cols, '2D cols');
    CheckEquals(12, Arr.TotalElements, '2D total elements');
    Check(Arr.ElementType = dbtInteger, '2D element type is Integer');
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.TestCreate3D;
var
  Arr: TSBWArray;
begin
  WriteLn('TestCreate3D');
  Arr := TSBWArray.Create(dbtDouble, [2, 3, 4]);
  try
    CheckEquals(3, Arr.DimensionCount, '3D dimension count');
    CheckEquals(2, Arr.Size(0), '3D dim 0');
    CheckEquals(3, Arr.Size(1), '3D dim 1');
    CheckEquals(4, Arr.Size(2), '3D dim 2');
    CheckEquals(24, Arr.TotalElements, '3D total elements');
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.TestCreateWithFactoryFunctions;
var
  DoubleArr, IntArr, StrArr, DoubleMat: TSBWArray;
begin
  WriteLn('TestCreateWithFactoryFunctions');
  
  DoubleArr := SBWDoubleArray([1.0, 2.0, 3.0]);
  try
    CheckEquals(3, DoubleArr.TotalElements, 'SBWDoubleArray size');
    CheckEquals(2.0, DoubleArr.GetDouble(1), 'SBWDoubleArray element');
  finally
    DoubleArr.Free;
  end;
  
  IntArr := SBWIntegerArray([10, 20, 30, 40]);
  try
    CheckEquals(4, IntArr.TotalElements, 'SBWIntegerArray size');
    CheckEquals(30, IntArr.GetInteger(2), 'SBWIntegerArray element');
  finally
    IntArr.Free;
  end;
  
  StrArr := SBWStringArray(['a', 'b', 'c']);
  try
    CheckEquals(3, StrArr.TotalElements, 'SBWStringArray size');
    CheckEquals('b', StrArr.GetString(1), 'SBWStringArray element');
  finally
    StrArr.Free;
  end;
  
  DoubleMat := SBWDoubleMatrix([[1.0, 2.0], [3.0, 4.0]]);
  try
    CheckEquals(4, DoubleMat.TotalElements, 'SBWDoubleMatrix size');
    CheckEquals(3.0, DoubleMat.GetDouble2D(1, 0), 'SBWDoubleMatrix element');
  finally
    DoubleMat.Free;
  end;
end;

procedure TArrayTests.TestDoubleArray1DAccess;
var
  Arr: TSBWArray;
begin
  WriteLn('TestDoubleArray1DAccess');
  Arr := TSBWArray.Create(dbtDouble, 3);
  try
    Arr.SetDouble(0, 1.5);
    Arr.SetDouble(1, 2.5);
    Arr.SetDouble(2, 3.5);
    
    CheckEquals(1.5, Arr.GetDouble(0), 'GetDouble(0)');
    CheckEquals(2.5, Arr.GetDouble(1), 'GetDouble(1)');
    CheckEquals(3.5, Arr.GetDouble(2), 'GetDouble(2)');
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.TestIntegerArray1DAccess;
var
  Arr: TSBWArray;
begin
  WriteLn('TestIntegerArray1DAccess');
  Arr := TSBWArray.Create(dbtInteger, 3);
  try
    Arr.SetInteger(0, -100);
    Arr.SetInteger(1, 0);
    Arr.SetInteger(2, 100);
    
    CheckEquals(-100, Arr.GetInteger(0), 'GetInteger(0)');
    CheckEquals(0, Arr.GetInteger(1), 'GetInteger(1)');
    CheckEquals(100, Arr.GetInteger(2), 'GetInteger(2)');
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.TestStringArray1DAccess;
var
  Arr: TSBWArray;
begin
  WriteLn('TestStringArray1DAccess');
  Arr := TSBWArray.Create(dbtString, 3);
  try
    Arr.SetString(0, 'Hello');
    Arr.SetString(1, 'World');
    Arr.SetString(2, '');
    
    CheckEquals('Hello', Arr.GetString(0), 'GetString(0)');
    CheckEquals('World', Arr.GetString(1), 'GetString(1)');
    CheckEquals('', Arr.GetString(2), 'GetString(2)');
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.TestDoubleArray2DAccess;
var
  Arr: TSBWArray;
begin
  WriteLn('TestDoubleArray2DAccess');
  Arr := TSBWArray.Create(dbtDouble, [3, 4]);
  try
    Arr.SetDouble2D(0, 0, 1.0);
    Arr.SetDouble2D(0, 1, 2.0);
    Arr.SetDouble2D(1, 2, 5.0);
    Arr.SetDouble2D(2, 3, 9.0);
    
    CheckEquals(1.0, Arr.GetDouble2D(0, 0), 'GetDouble2D(0,0)');
    CheckEquals(2.0, Arr.GetDouble2D(0, 1), 'GetDouble2D(0,1)');
    CheckEquals(5.0, Arr.GetDouble2D(1, 2), 'GetDouble2D(1,2)');
    CheckEquals(9.0, Arr.GetDouble2D(2, 3), 'GetDouble2D(2,3)');
    
    // Test generic multi-dimensional access
    CheckEquals(5.0, Arr.GetDoubleAt([1, 2]), 'GetDoubleAt([1,2])');
    
    Arr.SetDoubleAt([2, 0], 7.0);
    CheckEquals(7.0, Arr.GetDouble2D(2, 0), 'After SetDoubleAt');
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.TestSetFromDoubleArray;
var
  Arr: TSBWArray;
  Data: TArray<SBWDouble>;
begin
  WriteLn('TestSetFromDoubleArray');
  Data := [1.1, 2.2, 3.3, 4.4, 5.5];
  
  Arr := TSBWArray.Create(dbtDouble, 5);
  try
    Arr.SetFromDoubleArray(Data);
    
    CheckEquals(1.1, Arr.GetDouble(0), 'Element 0');
    CheckEquals(3.3, Arr.GetDouble(2), 'Element 2');
    CheckEquals(5.5, Arr.GetDouble(4), 'Element 4');
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.TestSetFromDoubleArray2D;
var
  Arr: TSBWArray;
  Data: TArray<TArray<SBWDouble>>;
begin
  WriteLn('TestSetFromDoubleArray2D');
  Data := [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]];
  
  Arr := TSBWArray.Create(dbtDouble);
  try
    Arr.SetFromDoubleArray2D(Data);
    
    CheckEquals(2, Arr.Rows, 'Rows');
    CheckEquals(3, Arr.Cols, 'Cols');
    CheckEquals(1.0, Arr.GetDouble2D(0, 0), 'Element [0,0]');
    CheckEquals(5.0, Arr.GetDouble2D(1, 1), 'Element [1,1]');
    CheckEquals(6.0, Arr.GetDouble2D(1, 2), 'Element [1,2]');
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.TestAsDoubleArray2D;
var
  Arr: TSBWArray;
  Data: TArray<TArray<SBWDouble>>;
begin
  WriteLn('TestAsDoubleArray2D');
  Arr := TSBWArray.Create(dbtDouble, [2, 3]);
  try
    Arr.SetDouble2D(0, 0, 1.0);
    Arr.SetDouble2D(0, 1, 2.0);
    Arr.SetDouble2D(0, 2, 3.0);
    Arr.SetDouble2D(1, 0, 4.0);
    Arr.SetDouble2D(1, 1, 5.0);
    Arr.SetDouble2D(1, 2, 6.0);
    
    Data := Arr.AsDoubleArray2D;
    
    CheckEquals(2, Length(Data), 'Row count');
    CheckEquals(3, Length(Data[0]), 'Col count');
    CheckEquals(1.0, Data[0][0], 'Data[0,0]');
    CheckEquals(5.0, Data[1][1], 'Data[1,1]');
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.TestReshape;
var
  Arr: TSBWArray;
begin
  WriteLn('TestReshape');
  Arr := TSBWArray.Create(dbtDouble, 6);
  try
    Arr.SetDouble(0, 1.0);
    Arr.SetDouble(5, 6.0);
    
    CheckEquals(1, Arr.DimensionCount, 'Initial dims');
    CheckEquals(6, Arr.TotalElements, 'Initial total');
    
    Arr.Reshape([2, 3]);
    
    CheckEquals(2, Arr.DimensionCount, 'After reshape dims');
    CheckEquals(6, Arr.TotalElements, 'After reshape total');
    CheckEquals(2, Arr.Rows, 'After reshape rows');
    CheckEquals(3, Arr.Cols, 'After reshape cols');
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.TestDoubleArrayRoundTrip;
var
  Arr, Arr2: TSBWArray;
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
begin
  WriteLn('TestDoubleArrayRoundTrip');
  
  Arr := SBWDoubleArray([1.5, 2.5, 3.5, 4.5]);
  Writer := TSBWDataBlockWriter.Create;
  try
    Arr.WriteTo(Writer);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
    Arr.Free;
  end;
  
  Reader := TSBWDataBlockReader.Create(Data);
  try
    Arr2 := TSBWArray.ReadFrom(Reader);
    try
      Check(Arr2.ElementType = dbtDouble, 'Element type is Double');
      CheckEquals(4, Arr2.TotalElements, 'Total elements');
      CheckEquals(1.5, Arr2.GetDouble(0), 'Element 0');
      CheckEquals(4.5, Arr2.GetDouble(3), 'Element 3');
    finally
      Arr2.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TArrayTests.TestIntegerArrayRoundTrip;
var
  Arr, Arr2: TSBWArray;
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
begin
  WriteLn('TestIntegerArrayRoundTrip');
  
  Arr := SBWIntegerArray([10, -20, 30, -40, 50]);
  Writer := TSBWDataBlockWriter.Create;
  try
    Arr.WriteTo(Writer);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
    Arr.Free;
  end;
  
  Reader := TSBWDataBlockReader.Create(Data);
  try
    Arr2 := TSBWArray.ReadFrom(Reader);
    try
      Check(Arr2.ElementType = dbtInteger, 'Element type is Integer');
      CheckEquals(5, Arr2.TotalElements, 'Total elements');
      CheckEquals(10, Arr2.GetInteger(0), 'Element 0');
      CheckEquals(-40, Arr2.GetInteger(3), 'Element 3');
    finally
      Arr2.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TArrayTests.TestStringArrayRoundTrip;
var
  Arr, Arr2: TSBWArray;
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
begin
  WriteLn('TestStringArrayRoundTrip');
  
  Arr := SBWStringArray(['Hello', 'World', '']);
  Writer := TSBWDataBlockWriter.Create;
  try
    Arr.WriteTo(Writer);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
    Arr.Free;
  end;
  
  Reader := TSBWDataBlockReader.Create(Data);
  try
    Arr2 := TSBWArray.ReadFrom(Reader);
    try
      Check(Arr2.ElementType = dbtString, 'Element type is String');
      CheckEquals(3, Arr2.TotalElements, 'Total elements');
      CheckEquals('Hello', Arr2.GetString(0), 'Element 0');
      CheckEquals('', Arr2.GetString(2), 'Element 2 (empty)');
    finally
      Arr2.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TArrayTests.TestDoubleArray2DRoundTrip;
var
  Arr, Arr2: TSBWArray;
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
begin
  WriteLn('TestDoubleArray2DRoundTrip');
  
  Arr := SBWDoubleMatrix([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]);
  Writer := TSBWDataBlockWriter.Create;
  try
    Arr.WriteTo(Writer);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
    Arr.Free;
  end;
  
  Reader := TSBWDataBlockReader.Create(Data);
  try
    Arr2 := TSBWArray.ReadFrom(Reader);
    try
      CheckEquals(2, Arr2.DimensionCount, 'Dimension count');
      CheckEquals(2, Arr2.Rows, 'Rows');
      CheckEquals(3, Arr2.Cols, 'Cols');
      CheckEquals(1.0, Arr2.GetDouble2D(0, 0), 'Element [0,0]');
      CheckEquals(6.0, Arr2.GetDouble2D(1, 2), 'Element [1,2]');
    finally
      Arr2.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TArrayTests.TestArray3DRoundTrip;
var
  Arr, Arr2: TSBWArray;
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
begin
  WriteLn('TestArray3DRoundTrip');
  
  Arr := TSBWArray.Create(dbtDouble, [2, 3, 4]);
  try
    Arr.SetDoubleAt([0, 0, 0], 1.0);
    Arr.SetDoubleAt([1, 2, 3], 24.0);
    Arr.SetDoubleAt([0, 1, 2], 7.0);
    
    Writer := TSBWDataBlockWriter.Create;
    try
      Arr.WriteTo(Writer);
      Data := Writer.ToBytes;
    finally
      Writer.Free;
    end;
  finally
    Arr.Free;
  end;
  
  Reader := TSBWDataBlockReader.Create(Data);
  try
    Arr2 := TSBWArray.ReadFrom(Reader);
    try
      CheckEquals(3, Arr2.DimensionCount, 'Dimension count');
      CheckEquals(2, Arr2.Size(0), 'Dim 0');
      CheckEquals(3, Arr2.Size(1), 'Dim 1');
      CheckEquals(4, Arr2.Size(2), 'Dim 2');
      CheckEquals(1.0, Arr2.GetDoubleAt([0, 0, 0]), 'Element [0,0,0]');
      CheckEquals(24.0, Arr2.GetDoubleAt([1, 2, 3]), 'Element [1,2,3]');
      CheckEquals(7.0, Arr2.GetDoubleAt([0, 1, 2]), 'Element [0,1,2]');
    finally
      Arr2.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TArrayTests.TestEmptyArrayRoundTrip;
var
  Arr, Arr2: TSBWArray;
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
begin
  WriteLn('TestEmptyArrayRoundTrip');
  
  Arr := TSBWArray.Create(dbtDouble, 0);
  Writer := TSBWDataBlockWriter.Create;
  try
    Arr.WriteTo(Writer);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
    Arr.Free;
  end;
  
  Reader := TSBWDataBlockReader.Create(Data);
  try
    Arr2 := TSBWArray.ReadFrom(Reader);
    try
      Check(Arr2.ElementType = dbtDouble, 'Element type is Double');
      CheckEquals(0, Arr2.TotalElements, 'Total elements is 0');
    finally
      Arr2.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TArrayTests.TestClone;
var
  Arr, Arr2: TSBWArray;
begin
  WriteLn('TestClone');
  
  Arr := SBWDoubleArray([1.0, 2.0, 3.0]);
  try
    Arr2 := Arr.Clone;
    try
      CheckEquals(3, Arr2.TotalElements, 'Clone size');
      CheckEquals(1.0, Arr2.GetDouble(0), 'Clone element 0');
      CheckEquals(3.0, Arr2.GetDouble(2), 'Clone element 2');
      
      // Modify original, clone should be unchanged
      Arr.SetDouble(0, 99.0);
      CheckEquals(1.0, Arr2.GetDouble(0), 'Clone unchanged after original modified');
    finally
      Arr2.Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.TestClear;
var
  Arr: TSBWArray;
begin
  WriteLn('TestClear');
  
  Arr := SBWDoubleArray([1.0, 2.0, 3.0]);
  try
    Arr.Clear;
    CheckEquals(0.0, Arr.GetDouble(0), 'After clear element 0');
    CheckEquals(0.0, Arr.GetDouble(1), 'After clear element 1');
    CheckEquals(0.0, Arr.GetDouble(2), 'After clear element 2');
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.TestFill;
var
  Arr: TSBWArray;
begin
  WriteLn('TestFill');
  
  Arr := TSBWArray.Create(dbtDouble, 5);
  try
    Arr.Fill(3.14);
    CheckEquals(3.14, Arr.GetDouble(0), 'Fill element 0');
    CheckEquals(3.14, Arr.GetDouble(2), 'Fill element 2');
    CheckEquals(3.14, Arr.GetDouble(4), 'Fill element 4');
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.TestIdentityMatrix;
var
  Arr: TSBWArray;
begin
  WriteLn('TestIdentityMatrix');
  
  Arr := SBWIdentityMatrix(3);
  try
    CheckEquals(2, Arr.DimensionCount, 'Dimension count');
    CheckEquals(3, Arr.Rows, 'Rows');
    CheckEquals(3, Arr.Cols, 'Cols');
    
    // Diagonal should be 1
    CheckEquals(1.0, Arr.GetDouble2D(0, 0), 'Diagonal [0,0]');
    CheckEquals(1.0, Arr.GetDouble2D(1, 1), 'Diagonal [1,1]');
    CheckEquals(1.0, Arr.GetDouble2D(2, 2), 'Diagonal [2,2]');
    
    // Off-diagonal should be 0
    CheckEquals(0.0, Arr.GetDouble2D(0, 1), 'Off-diagonal [0,1]');
    CheckEquals(0.0, Arr.GetDouble2D(1, 0), 'Off-diagonal [1,0]');
    CheckEquals(0.0, Arr.GetDouble2D(2, 0), 'Off-diagonal [2,0]');
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.TestZeroMatrix;
var
  Arr: TSBWArray;
begin
  WriteLn('TestZeroMatrix');
  
  Arr := SBWZeroMatrix(2, 3);
  try
    CheckEquals(2, Arr.Rows, 'Rows');
    CheckEquals(3, Arr.Cols, 'Cols');
    
    CheckEquals(0.0, Arr.GetDouble2D(0, 0), 'Element [0,0]');
    CheckEquals(0.0, Arr.GetDouble2D(1, 2), 'Element [1,2]');
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.TestTypeMismatchException;
var
  Arr: TSBWArray;
  ExceptionRaised: Boolean;
begin
  WriteLn('TestTypeMismatchException');
  
  Arr := TSBWArray.Create(dbtDouble, 3);
  try
    ExceptionRaised := False;
    try
      Arr.GetInteger(0); // Should fail - it's a double array
    except
      on E: ESBWArrayError do
        ExceptionRaised := True;
    end;
    Check(ExceptionRaised, 'TypeMismatch exception raised');
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.TestIndexOutOfRangeException;
var
  Arr: TSBWArray;
  ExceptionRaised: Boolean;
begin
  WriteLn('TestIndexOutOfRangeException');
  
  Arr := TSBWArray.Create(dbtDouble, 3);
  try
    ExceptionRaised := False;
    try
      Arr.GetDouble(5); // Should fail - index out of range
    except
      on E: ESBWArrayError do
        ExceptionRaised := True;
    end;
    Check(ExceptionRaised, 'IndexOutOfRange exception raised');
  finally
    Arr.Free;
  end;
end;

procedure TArrayTests.RunAllTests;
begin
  WriteLn('=== SBW Array Tests ===');
  WriteLn;

  TestCreate1D;
  TestCreate2D;
  TestCreate3D;
  TestCreateWithFactoryFunctions;
  TestDoubleArray1DAccess;
  TestIntegerArray1DAccess;
  TestStringArray1DAccess;
  TestDoubleArray2DAccess;
  TestSetFromDoubleArray;
  TestSetFromDoubleArray2D;
  TestAsDoubleArray2D;
  TestReshape;
  TestDoubleArrayRoundTrip;
  TestIntegerArrayRoundTrip;
  TestStringArrayRoundTrip;
  TestDoubleArray2DRoundTrip;
  TestArray3DRoundTrip;
  TestEmptyArrayRoundTrip;
  TestClone;
  TestClear;
  TestFill;
  TestIdentityMatrix;
  TestZeroMatrix;
  TestTypeMismatchException;
  TestIndexOutOfRangeException;
end;

end.
