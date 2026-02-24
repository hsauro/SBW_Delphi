unit SBW.Tests.DataBlock;

{******************************************************************************
 * SBW.Tests.DataBlock.pas
 *
 * Unit tests for TSBWDataBlockWriter and TSBWDataBlockReader.
 * Tests round-trip serialization for all supported data types.
 *
 * To run these tests, include this unit in a DUnit test project or
 * call RunAllTests from a console application.
 *****************************************************************************}

interface

uses
  System.SysUtils, System.Math,
  SBW.Types, SBW.DataBlock;

type
  TDataBlockTests = class
  private
    FTestCount: Integer;
    FPassCount: Integer;
    FFailCount: Integer;

    procedure Check(Condition: Boolean; const TestName: string);
    procedure CheckEquals(Expected, Actual: SBWInteger; const TestName: string); overload;
    procedure CheckEquals(Expected, Actual: SBWDouble; const TestName: string; Epsilon: Double = 1E-10); overload;
    procedure CheckEquals(const Expected, Actual: string; const TestName: string); overload;
    procedure CheckEquals(Expected, Actual: Boolean; const TestName: string); overload;
  public
    constructor Create;

    // Individual test methods
    procedure TestByteRoundTrip;
    procedure TestBooleanRoundTrip;
    procedure TestIntegerRoundTrip;
    procedure TestDoubleRoundTrip;
    procedure TestStringRoundTrip;
    procedure TestComplexRoundTrip;
    procedure TestIntegerArrayRoundTrip;
    procedure TestDoubleArrayRoundTrip;
    procedure TestStringArrayRoundTrip;
    procedure TestByteArrayRoundTrip;
    procedure TestDoubleArray2DRoundTrip;
    procedure TestIntegerArray2DRoundTrip;
    procedure TestMixedDataRoundTrip;
    procedure TestListRoundTrip;
    procedure TestEmptyArrays;
    procedure TestLargeArray;
    procedure TestUnicodeStrings;
    procedure TestPeekNextType;
    procedure TestSkip;
    procedure TestTypeMismatchException;
    procedure TestReadPastEndException;

    // Run all tests
    procedure RunAllTests;

    // Results
    property TestCount: Integer read FTestCount;
    property PassCount: Integer read FPassCount;
    property FailCount: Integer read FFailCount;
  end;

/// <summary>
/// Convenience function to run all tests and report results
/// </summary>
procedure RunAllTests;

implementation

procedure RunAllTests;
var
  Tests: TDataBlockTests;
begin
  Tests := TDataBlockTests.Create;
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

{ TDataBlockTests }

constructor TDataBlockTests.Create;
begin
  inherited Create;
  FTestCount := 0;
  FPassCount := 0;
  FFailCount := 0;
end;

procedure TDataBlockTests.Check(Condition: Boolean; const TestName: string);
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

procedure TDataBlockTests.CheckEquals(Expected, Actual: SBWInteger; const TestName: string);
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

procedure TDataBlockTests.CheckEquals(Expected, Actual: SBWDouble; const TestName: string; Epsilon: Double);
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

procedure TDataBlockTests.CheckEquals(const Expected, Actual: string; const TestName: string);
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

procedure TDataBlockTests.CheckEquals(Expected, Actual: Boolean; const TestName: string);
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

procedure TDataBlockTests.TestByteRoundTrip;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
begin
  WriteLn('TestByteRoundTrip');
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteByte(0);
    Writer.WriteByte(127);
    Writer.WriteByte(255);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    CheckEquals(0, Reader.ReadByte, 'Byte 0');
    CheckEquals(127, Reader.ReadByte, 'Byte 127');
    CheckEquals(255, Reader.ReadByte, 'Byte 255');
    Check(not Reader.HasMore, 'No more data');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestBooleanRoundTrip;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
begin
  WriteLn('TestBooleanRoundTrip');
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteBoolean(True);
    Writer.WriteBoolean(False);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    CheckEquals(True, Reader.ReadBoolean, 'Boolean True');
    CheckEquals(False, Reader.ReadBoolean, 'Boolean False');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestIntegerRoundTrip;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
begin
  WriteLn('TestIntegerRoundTrip');
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteInteger(0);
    Writer.WriteInteger(42);
    Writer.WriteInteger(-1);
    Writer.WriteInteger(MaxInt);
    Writer.WriteInteger(-MaxInt - 1);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    CheckEquals(0, Reader.ReadInteger, 'Integer 0');
    CheckEquals(42, Reader.ReadInteger, 'Integer 42');
    CheckEquals(-1, Reader.ReadInteger, 'Integer -1');
    CheckEquals(MaxInt, Reader.ReadInteger, 'Integer MaxInt');
    CheckEquals(-MaxInt - 1, Reader.ReadInteger, 'Integer MinInt');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestDoubleRoundTrip;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
begin
  WriteLn('TestDoubleRoundTrip');
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteDouble(0.0);
    Writer.WriteDouble(3.14159265358979);
    Writer.WriteDouble(-1.0E-100);
    Writer.WriteDouble(1.7976931348623157E+308); // Near max double
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    CheckEquals(0.0, Reader.ReadDouble, 'Double 0');
    CheckEquals(3.14159265358979, Reader.ReadDouble, 'Double Pi');
    CheckEquals(-1.0E-100, Reader.ReadDouble, 'Double small negative');
    CheckEquals(1.7976931348623157E+308, Reader.ReadDouble, 'Double large', 1E+295);
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestStringRoundTrip;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
begin
  WriteLn('TestStringRoundTrip');
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteString('');
    Writer.WriteString('Hello, World!');
    Writer.WriteString('A');
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    CheckEquals('', Reader.ReadString, 'Empty string');
    CheckEquals('Hello, World!', Reader.ReadString, 'Hello World');
    CheckEquals('A', Reader.ReadString, 'Single char');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestComplexRoundTrip;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
  C1, C2: TSBWComplex;
  R1, R2: TSBWComplex;
begin
  WriteLn('TestComplexRoundTrip');

  C1.Real := 3.0;
  C1.Imag := 4.0;
  C2.Real := -1.5;
  C2.Imag := 0.0;

  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteComplex(C1);
    Writer.WriteComplex(C2);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    R1 := Reader.ReadComplex;
    R2 := Reader.ReadComplex;
    CheckEquals(3.0, R1.Real, 'Complex1 Real');
    CheckEquals(4.0, R1.Imag, 'Complex1 Imag');
    CheckEquals(-1.5, R2.Real, 'Complex2 Real');
    CheckEquals(0.0, R2.Imag, 'Complex2 Imag');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestIntegerArrayRoundTrip;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
  InArr, OutArr: TArray<SBWInteger>;
begin
  WriteLn('TestIntegerArrayRoundTrip');
  InArr := [1, 2, 3, 4, 5, -100, 0, MaxInt];

  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteIntegerArray(InArr);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    OutArr := Reader.ReadIntegerArray;
    CheckEquals(Length(InArr), Length(OutArr), 'Array length');
    CheckEquals(InArr[0], OutArr[0], 'Element 0');
    CheckEquals(InArr[5], OutArr[5], 'Element 5 (-100)');
    CheckEquals(InArr[7], OutArr[7], 'Element 7 (MaxInt)');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestDoubleArrayRoundTrip;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
  InArr, OutArr: TArray<SBWDouble>;
begin
  WriteLn('TestDoubleArrayRoundTrip');
  InArr := [1.1, 2.2, 3.3, -4.4, 0.0, Pi];

  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteDoubleArray(InArr);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    OutArr := Reader.ReadDoubleArray;
    CheckEquals(Length(InArr), Length(OutArr), 'Array length');
    CheckEquals(InArr[0], OutArr[0], 'Element 0');
    CheckEquals(InArr[3], OutArr[3], 'Element 3 (-4.4)');
    CheckEquals(Pi, OutArr[5], 'Element 5 (Pi)');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestStringArrayRoundTrip;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
  InArr, OutArr: TArray<string>;
begin
  WriteLn('TestStringArrayRoundTrip');
  InArr := ['alpha', 'beta', 'gamma', '', 'delta'];

  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteStringArray(InArr);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    OutArr := Reader.ReadStringArray;
    CheckEquals(Length(InArr), Length(OutArr), 'Array length');
    CheckEquals('alpha', OutArr[0], 'Element 0');
    CheckEquals('', OutArr[3], 'Element 3 (empty)');
    CheckEquals('delta', OutArr[4], 'Element 4');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestByteArrayRoundTrip;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
  InArr, OutArr: TArray<SBWByte>;
begin
  WriteLn('TestByteArrayRoundTrip');
  InArr := [0, 1, 127, 128, 255];

  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteByteArray(InArr);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    OutArr := Reader.ReadByteArray;
    CheckEquals(Length(InArr), Length(OutArr), 'Array length');
    CheckEquals(0, OutArr[0], 'Element 0');
    CheckEquals(128, OutArr[3], 'Element 3 (128)');
    CheckEquals(255, OutArr[4], 'Element 4 (255)');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestDoubleArray2DRoundTrip;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
  InArr, OutArr: TArray<TArray<SBWDouble>>;
begin
  WriteLn('TestDoubleArray2DRoundTrip');
  SetLength(InArr, 2);
  InArr[0] := [1.0, 2.0, 3.0];
  InArr[1] := [4.0, 5.0, 6.0];

  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteDoubleArray2D(InArr);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    OutArr := Reader.ReadDoubleArray2D;
    CheckEquals(2, Length(OutArr), 'Row count');
    CheckEquals(3, Length(OutArr[0]), 'Column count');
    CheckEquals(1.0, OutArr[0][0], 'Element [0,0]');
    CheckEquals(6.0, OutArr[1][2], 'Element [1,2]');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestIntegerArray2DRoundTrip;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
  InArr, OutArr: TArray<TArray<SBWInteger>>;
begin
  WriteLn('TestIntegerArray2DRoundTrip');
  SetLength(InArr, 3);
  InArr[0] := [1, 2];
  InArr[1] := [3, 4];
  InArr[2] := [5, 6];

  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteIntegerArray2D(InArr);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    OutArr := Reader.ReadIntegerArray2D;
    CheckEquals(3, Length(OutArr), 'Row count');
    CheckEquals(2, Length(OutArr[0]), 'Column count');
    CheckEquals(1, OutArr[0][0], 'Element [0,0]');
    CheckEquals(6, OutArr[2][1], 'Element [2,1]');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestMixedDataRoundTrip;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
begin
  WriteLn('TestMixedDataRoundTrip');
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteInteger(42);
    Writer.WriteString('test');
    Writer.WriteDouble(3.14);
    Writer.WriteBoolean(True);
    Writer.WriteByte(255);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    CheckEquals(42, Reader.ReadInteger, 'Integer');
    CheckEquals('test', Reader.ReadString, 'String');
    CheckEquals(3.14, Reader.ReadDouble, 'Double');
    CheckEquals(True, Reader.ReadBoolean, 'Boolean');
    CheckEquals(255, Reader.ReadByte, 'Byte');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestListRoundTrip;
var
  Writer, ItemWriter: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
  ListCount: SBWInteger;
begin
  WriteLn('TestListRoundTrip');

  Writer := TSBWDataBlockWriter.Create;
  try
    // Write a list with 3 items
    Writer.WriteListBegin(3);
    Writer.WriteInteger(100);
    Writer.WriteString('item2');
    Writer.WriteDouble(2.5);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    ListCount := Reader.ReadListBegin;
    CheckEquals(3, ListCount, 'List count');
    CheckEquals(100, Reader.ReadInteger, 'List item 0');
    CheckEquals('item2', Reader.ReadString, 'List item 1');
    CheckEquals(2.5, Reader.ReadDouble, 'List item 2');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestEmptyArrays;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
  EmptyInts: TArray<SBWInteger>;
  EmptyDoubles: TArray<SBWDouble>;
  EmptyStrings: TArray<string>;
begin
  WriteLn('TestEmptyArrays');

  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteIntegerArray([]);
    Writer.WriteDoubleArray([]);
    Writer.WriteStringArray([]);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    EmptyInts := Reader.ReadIntegerArray;
    EmptyDoubles := Reader.ReadDoubleArray;
    EmptyStrings := Reader.ReadStringArray;
    CheckEquals(0, Length(EmptyInts), 'Empty int array');
    CheckEquals(0, Length(EmptyDoubles), 'Empty double array');
    CheckEquals(0, Length(EmptyStrings), 'Empty string array');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestLargeArray;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
  InArr, OutArr: TArray<SBWDouble>;
  I: Integer;
const
  SIZE = 10000;
begin
  WriteLn('TestLargeArray');
  SetLength(InArr, SIZE);
  for I := 0 to SIZE - 1 do
    InArr[I] := I * 0.001;

  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteDoubleArray(InArr);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    OutArr := Reader.ReadDoubleArray;
    CheckEquals(SIZE, Length(OutArr), 'Large array length');
    CheckEquals(0.0, OutArr[0], 'First element');
    CheckEquals((SIZE - 1) * 0.001, OutArr[SIZE - 1], 'Last element');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestUnicodeStrings;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
begin
  WriteLn('TestUnicodeStrings');
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteString('Héllo Wörld');     // Latin extended
    Writer.WriteString('こんにちは');       // Japanese
    Writer.WriteString('Привет');           // Russian
    Writer.WriteString('🎉🚀💡');           // Emoji
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    CheckEquals('Héllo Wörld', Reader.ReadString, 'Latin extended');
    CheckEquals('こんにちは', Reader.ReadString, 'Japanese');
    CheckEquals('Привет', Reader.ReadString, 'Russian');
    CheckEquals('🎉🚀💡', Reader.ReadString, 'Emoji');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestPeekNextType;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
begin
  WriteLn('TestPeekNextType');
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteInteger(42);
    Writer.WriteString('test');
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    Check(Reader.PeekNextType = dbtInteger, 'Peek sees Integer');
    Check(Reader.PeekNextType = dbtInteger, 'Peek again still sees Integer');
    Reader.ReadInteger; // Consume it
    Check(Reader.PeekNextType = dbtString, 'Peek now sees String');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestSkip;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
begin
  WriteLn('TestSkip');
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteInteger(1);
    Writer.WriteString('skip me');
    Writer.WriteDoubleArray([1.0, 2.0, 3.0]);
    Writer.WriteInteger(999);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    CheckEquals(1, Reader.ReadInteger, 'First integer');
    Reader.Skip; // Skip the string
    Reader.Skip; // Skip the array
    CheckEquals(999, Reader.ReadInteger, 'Last integer after skips');
    Check(not Reader.HasMore, 'No more data after skips');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestTypeMismatchException;
var
  Writer: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
  Data: TBytes;
  ExceptionRaised: Boolean;
begin
  WriteLn('TestTypeMismatchException');
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteInteger(42);
    Data := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Reader := TSBWDataBlockReader.Create(Data);
  try
    ExceptionRaised := False;
    try
      Reader.ReadString; // Should fail - it's an integer
    except
      on E: ESBWTypeMismatch do
        ExceptionRaised := True;
    end;
    Check(ExceptionRaised, 'TypeMismatch exception raised');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.TestReadPastEndException;
var
  Reader: TSBWDataBlockReader;
  ExceptionRaised: Boolean;
begin
  WriteLn('TestReadPastEndException');
  Reader := TSBWDataBlockReader.Create([]);
  try
    ExceptionRaised := False;
    try
      Reader.ReadInteger;
    except
      on E: ESBWReadPastEnd do
        ExceptionRaised := True;
    end;
    Check(ExceptionRaised, 'ReadPastEnd exception raised');
  finally
    Reader.Free;
  end;
end;

procedure TDataBlockTests.RunAllTests;
begin
  WriteLn('=== SBW DataBlock Tests ===');
  WriteLn;

  TestByteRoundTrip;
  TestBooleanRoundTrip;
  TestIntegerRoundTrip;
  TestDoubleRoundTrip;
  TestStringRoundTrip;
  TestComplexRoundTrip;
  TestIntegerArrayRoundTrip;
  TestDoubleArrayRoundTrip;
  TestStringArrayRoundTrip;
  TestByteArrayRoundTrip;
  TestDoubleArray2DRoundTrip;
  TestIntegerArray2DRoundTrip;
  TestMixedDataRoundTrip;
  TestListRoundTrip;
  TestEmptyArrays;
  TestLargeArray;
  TestUnicodeStrings;
  TestPeekNextType;
  TestSkip;
  TestTypeMismatchException;
  TestReadPastEndException;
end;

end.

