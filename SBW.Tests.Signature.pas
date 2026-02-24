unit SBW.Tests.Signature;

{******************************************************************************
 * SBW.Tests.Signature.pas
 *
 * Unit tests for SBW signature parsing.
 *****************************************************************************}

interface

uses
  System.SysUtils, System.Generics.Collections,
  SBW.Types, SBW.Signature;

type
  TSignatureTests = class
  private
    FTestCount: Integer;
    FPassCount: Integer;
    FFailCount: Integer;

    procedure Check(Condition: Boolean; const TestName: string);
    procedure CheckEquals(Expected, Actual: Integer; const TestName: string); overload;
    procedure CheckEquals(const Expected, Actual: string; const TestName: string); overload;
    procedure CheckEquals(Expected, Actual: TSBWDataBlockType; const TestName: string); overload;
  public
    constructor Create;

    // Basic type parsing
    procedure TestParseSimpleTypes;
    procedure TestParseVoidReturn;
    procedure TestParseArrayTypes;
    procedure TestParse2DArrayTypes;
    procedure TestParseListTypes;

    // Complete signatures
    procedure TestParseSimpleSignature;
    procedure TestParseNoArgs;
    procedure TestParseMultipleArgs;
    procedure TestParseArrayArg;
    procedure TestParseListReturn;
    procedure TestParseVarArgs;
    procedure TestParseComplexSignature;
    procedure TestParseNamedArgs;
    procedure TestParseUnnamedArgs;
    procedure TestParseMixedNamedArgs;

    // Round-trip (parse then ToString)
    procedure TestRoundTrip;

    // Error cases
    procedure TestInvalidSignature;

    procedure RunAllTests;

    property TestCount: Integer read FTestCount;
    property PassCount: Integer read FPassCount;
    property FailCount: Integer read FFailCount;
  end;

procedure RunSignatureTests;

implementation

procedure RunSignatureTests;
var
  Tests: TSignatureTests;
begin
  Tests := TSignatureTests.Create;
  try
    Tests.RunAllTests;
    WriteLn;
    WriteLn(Format('Signature Tests: %d tests, %d passed, %d failed',
      [Tests.TestCount, Tests.PassCount, Tests.FailCount]));
  finally
    Tests.Free;
  end;
end;

{ TSignatureTests }

constructor TSignatureTests.Create;
begin
  inherited Create;
  FTestCount := 0;
  FPassCount := 0;
  FFailCount := 0;
end;

procedure TSignatureTests.Check(Condition: Boolean; const TestName: string);
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

procedure TSignatureTests.CheckEquals(Expected, Actual: Integer; const TestName: string);
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

procedure TSignatureTests.CheckEquals(const Expected, Actual: string; const TestName: string);
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

procedure TSignatureTests.CheckEquals(Expected, Actual: TSBWDataBlockType; const TestName: string);
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
    WriteLn(Format('  [FAIL] %s: expected %d, got %d', [TestName, Ord(Expected), Ord(Actual)]));
  end;
end;

procedure TSignatureTests.TestParseSimpleTypes;
var
  Parser: TSBWSignatureParser;
  T: TSBWSignatureType;
begin
  WriteLn('TestParseSimpleTypes');
  Parser := TSBWSignatureParser.Create;
  try
    T := Parser.ParseTypeOnly('int');
    try
      CheckEquals(dbtInteger, T.BaseType, 'int type');
    finally
      T.Free;
    end;

    T := Parser.ParseTypeOnly('double');
    try
      CheckEquals(dbtDouble, T.BaseType, 'double type');
    finally
      T.Free;
    end;

    T := Parser.ParseTypeOnly('string');
    try
      CheckEquals(dbtString, T.BaseType, 'string type');
    finally
      T.Free;
    end;

    T := Parser.ParseTypeOnly('boolean');
    try
      CheckEquals(dbtBoolean, T.BaseType, 'boolean type');
    finally
      T.Free;
    end;

    T := Parser.ParseTypeOnly('byte');
    try
      CheckEquals(dbtByte, T.BaseType, 'byte type');
    finally
      T.Free;
    end;

    T := Parser.ParseTypeOnly('complex');
    try
      CheckEquals(dbtComplex, T.BaseType, 'complex type');
    finally
      T.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSignatureTests.TestParseVoidReturn;
var
  Parser: TSBWSignatureParser;
  Sig: TSBWSignature;
begin
  WriteLn('TestParseVoidReturn');
  Parser := TSBWSignatureParser.Create;
  try
    Sig := Parser.Parse('void doSomething()');
    try
      Check(Sig.ReturnType.IsVoid, 'Return type is void');
      CheckEquals('doSomething', Sig.Name, 'Method name');
      CheckEquals(0, Sig.Arguments.Count, 'No arguments');
    finally
      Sig.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSignatureTests.TestParseArrayTypes;
var
  Parser: TSBWSignatureParser;
  T: TSBWSignatureType;
begin
  WriteLn('TestParseArrayTypes');
  Parser := TSBWSignatureParser.Create;
  try
    T := Parser.ParseTypeOnly('int[]');
    try
      Check(T.IsArray, 'Is array');
      CheckEquals(1, T.ArrayDimensions, '1D array');
      CheckEquals(dbtInteger, T.ArrayInnerType.BaseType, 'Inner type is int');
    finally
      T.Free;
    end;

    T := Parser.ParseTypeOnly('double[]');
    try
      Check(T.IsArray, 'double[] is array');
      CheckEquals(dbtDouble, T.ArrayInnerType.BaseType, 'Inner type is double');
    finally
      T.Free;
    end;

    T := Parser.ParseTypeOnly('string[]');
    try
      Check(T.IsArray, 'string[] is array');
      CheckEquals(dbtString, T.ArrayInnerType.BaseType, 'Inner type is string');
    finally
      T.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSignatureTests.TestParse2DArrayTypes;
var
  Parser: TSBWSignatureParser;
  T: TSBWSignatureType;
begin
  WriteLn('TestParse2DArrayTypes');
  Parser := TSBWSignatureParser.Create;
  try
    T := Parser.ParseTypeOnly('double[][]');
    try
      Check(T.IsArray, 'Is array');
      CheckEquals(2, T.ArrayDimensions, '2D array');
      CheckEquals(dbtDouble, T.ArrayInnerType.BaseType, 'Inner type is double');
    finally
      T.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSignatureTests.TestParseListTypes;
var
  Parser: TSBWSignatureParser;
  T: TSBWSignatureType;
begin
  WriteLn('TestParseListTypes');
  Parser := TSBWSignatureParser.Create;
  try
    // Empty list
    T := Parser.ParseTypeOnly('{}');
    try
      Check(T.IsList, 'Is list');
      CheckEquals(0, T.ListContents.Count, 'Empty list');
    finally
      T.Free;
    end;

    // List with one element
    T := Parser.ParseTypeOnly('{int}');
    try
      Check(T.IsList, 'Is list');
      CheckEquals(1, T.ListContents.Count, 'One element');
      CheckEquals(dbtInteger, T.ListContents[0].DataType.BaseType, 'Element is int');
    finally
      T.Free;
    end;

    // List with named elements
    T := Parser.ParseTypeOnly('{string name, double value}');
    try
      Check(T.IsList, 'Is list');
      CheckEquals(2, T.ListContents.Count, 'Two elements');
      CheckEquals('name', T.ListContents[0].Name, 'First element name');
      CheckEquals(dbtString, T.ListContents[0].DataType.BaseType, 'First element type');
      CheckEquals('value', T.ListContents[1].Name, 'Second element name');
      CheckEquals(dbtDouble, T.ListContents[1].DataType.BaseType, 'Second element type');
    finally
      T.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSignatureTests.TestParseSimpleSignature;
var
  Parser: TSBWSignatureParser;
  Sig: TSBWSignature;
begin
  WriteLn('TestParseSimpleSignature');
  Parser := TSBWSignatureParser.Create;
  try
    Sig := Parser.Parse('double sin(double x)');
    try
      CheckEquals('sin', Sig.Name, 'Method name');
      CheckEquals(dbtDouble, Sig.ReturnType.BaseType, 'Return type');
      CheckEquals(1, Sig.Arguments.Count, 'Argument count');
      CheckEquals(dbtDouble, Sig.Arguments[0].DataType.BaseType, 'Arg type');
      CheckEquals('x', Sig.Arguments[0].Name, 'Arg name');
    finally
      Sig.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSignatureTests.TestParseNoArgs;
var
  Parser: TSBWSignatureParser;
  Sig: TSBWSignature;
begin
  WriteLn('TestParseNoArgs');
  Parser := TSBWSignatureParser.Create;
  try
    Sig := Parser.Parse('int getCount()');
    try
      CheckEquals('getCount', Sig.Name, 'Method name');
      CheckEquals(dbtInteger, Sig.ReturnType.BaseType, 'Return type');
      CheckEquals(0, Sig.Arguments.Count, 'No arguments');
    finally
      Sig.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSignatureTests.TestParseMultipleArgs;
var
  Parser: TSBWSignatureParser;
  Sig: TSBWSignature;
begin
  WriteLn('TestParseMultipleArgs');
  Parser := TSBWSignatureParser.Create;
  try
    Sig := Parser.Parse('double add(double a, double b)');
    try
      CheckEquals('add', Sig.Name, 'Method name');
      CheckEquals(2, Sig.Arguments.Count, 'Two arguments');
      CheckEquals('a', Sig.Arguments[0].Name, 'First arg name');
      CheckEquals('b', Sig.Arguments[1].Name, 'Second arg name');
    finally
      Sig.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSignatureTests.TestParseArrayArg;
var
  Parser: TSBWSignatureParser;
  Sig: TSBWSignature;
begin
  WriteLn('TestParseArrayArg');
  Parser := TSBWSignatureParser.Create;
  try
    Sig := Parser.Parse('int sum(int[] values)');
    try
      CheckEquals('sum', Sig.Name, 'Method name');
      CheckEquals(dbtInteger, Sig.ReturnType.BaseType, 'Return type');
      CheckEquals(1, Sig.Arguments.Count, 'One argument');
      Check(Sig.Arguments[0].DataType.IsArray, 'Arg is array');
      CheckEquals(dbtInteger, Sig.Arguments[0].DataType.ArrayInnerType.BaseType, 'Array element type');
    finally
      Sig.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSignatureTests.TestParseListReturn;
var
  Parser: TSBWSignatureParser;
  Sig: TSBWSignature;
begin
  WriteLn('TestParseListReturn');
  Parser := TSBWSignatureParser.Create;
  try
    Sig := Parser.Parse('{string name, double value} getPerson(int id)');
    try
      CheckEquals('getPerson', Sig.Name, 'Method name');
      Check(Sig.ReturnType.IsList, 'Return type is list');
      CheckEquals(2, Sig.ReturnType.ListContents.Count, 'List has 2 elements');
      CheckEquals(1, Sig.Arguments.Count, 'One argument');
    finally
      Sig.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSignatureTests.TestParseVarArgs;
var
  Parser: TSBWSignatureParser;
  Sig: TSBWSignature;
begin
  WriteLn('TestParseVarArgs');
  Parser := TSBWSignatureParser.Create;
  try
    Sig := Parser.Parse('void f(...)');
    try
      CheckEquals('f', Sig.Name, 'Method name');
      Check(Sig.HasVarArgs, 'Has varargs');
      CheckEquals(0, Sig.Arguments.Count, 'No fixed arguments');
    finally
      Sig.Free;
    end;

    Sig := Parser.Parse('void g(int x, ...)');
    try
      CheckEquals('g', Sig.Name, 'Method name');
      Check(Sig.HasVarArgs, 'Has varargs');
      CheckEquals(1, Sig.Arguments.Count, 'One fixed argument');
    finally
      Sig.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSignatureTests.TestParseComplexSignature;
var
  Parser: TSBWSignatureParser;
  Sig: TSBWSignature;
begin
  WriteLn('TestParseComplexSignature');
  Parser := TSBWSignatureParser.Create;
  try
    Sig := Parser.Parse('{}[] getRecords()');
    try
      CheckEquals('getRecords', Sig.Name, 'Method name');
      Check(Sig.ReturnType.IsArray, 'Return is array');
      Check(Sig.ReturnType.ArrayInnerType.IsList, 'Array of lists');
    finally
      Sig.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSignatureTests.TestParseNamedArgs;
var
  Parser: TSBWSignatureParser;
  Sig: TSBWSignature;
begin
  WriteLn('TestParseNamedArgs');
  Parser := TSBWSignatureParser.Create;
  try
    Sig := Parser.Parse('double compute(double x, double y, double z)');
    try
      CheckEquals(3, Sig.Arguments.Count, 'Three arguments');
      CheckEquals('x', Sig.Arguments[0].Name, 'First arg name');
      CheckEquals('y', Sig.Arguments[1].Name, 'Second arg name');
      CheckEquals('z', Sig.Arguments[2].Name, 'Third arg name');
    finally
      Sig.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSignatureTests.TestParseUnnamedArgs;
var
  Parser: TSBWSignatureParser;
  Sig: TSBWSignature;
begin
  WriteLn('TestParseUnnamedArgs');
  Parser := TSBWSignatureParser.Create;
  try
    Sig := Parser.Parse('double compute(double, double, double)');
    try
      CheckEquals(3, Sig.Arguments.Count, 'Three arguments');
      CheckEquals('', Sig.Arguments[0].Name, 'First arg unnamed');
      CheckEquals('', Sig.Arguments[1].Name, 'Second arg unnamed');
      CheckEquals('', Sig.Arguments[2].Name, 'Third arg unnamed');
    finally
      Sig.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSignatureTests.TestParseMixedNamedArgs;
var
  Parser: TSBWSignatureParser;
  Sig: TSBWSignature;
begin
  WriteLn('TestParseMixedNamedArgs');
  Parser := TSBWSignatureParser.Create;
  try
    Sig := Parser.Parse('void doAnalysis(string sbml)');
    try
      CheckEquals(1, Sig.Arguments.Count, 'One argument');
      CheckEquals(dbtString, Sig.Arguments[0].DataType.BaseType, 'Arg is string');
      CheckEquals('sbml', Sig.Arguments[0].Name, 'Arg name');
    finally
      Sig.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSignatureTests.TestRoundTrip;
var
  Parser: TSBWSignatureParser;
  Sig: TSBWSignature;
  Original, RoundTripped: string;
begin
  WriteLn('TestRoundTrip');
  Parser := TSBWSignatureParser.Create;
  try
    // Simple signature
    Original := 'double sin(double x)';
    Sig := Parser.Parse(Original);
    try
      RoundTripped := Sig.ToString;
      CheckEquals(Original, RoundTripped, 'Simple round trip');
    finally
      Sig.Free;
    end;

    // No args
    Original := 'int getCount()';
    Sig := Parser.Parse(Original);
    try
      RoundTripped := Sig.ToString;
      CheckEquals(Original, RoundTripped, 'No args round trip');
    finally
      Sig.Free;
    end;

    // Array return
    Original := 'double[] getValues()';
    Sig := Parser.Parse(Original);
    try
      RoundTripped := Sig.ToString;
      CheckEquals(Original, RoundTripped, 'Array return round trip');
    finally
      Sig.Free;
    end;

    // Varargs
    Original := 'void f(...)';
    Sig := Parser.Parse(Original);
    try
      RoundTripped := Sig.ToString;
      CheckEquals(Original, RoundTripped, 'Varargs round trip');
    finally
      Sig.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSignatureTests.TestInvalidSignature;
var
  Parser: TSBWSignatureParser;
  ExceptionRaised: Boolean;
begin
  WriteLn('TestInvalidSignature');
  Parser := TSBWSignatureParser.Create;
  try
    // Missing parentheses
    ExceptionRaised := False;
    try
      Parser.Parse('int getValue').Free;
    except
      on E: ESBWSignatureError do
        ExceptionRaised := True;
    end;
    Check(ExceptionRaised, 'Missing parens raises exception');

    // Invalid type
    ExceptionRaised := False;
    try
      Parser.Parse('foobar getValue()').Free;
    except
      on E: ESBWSignatureError do
        ExceptionRaised := True;
    end;
    Check(ExceptionRaised, 'Invalid type raises exception');
  finally
    Parser.Free;
  end;
end;

procedure TSignatureTests.RunAllTests;
begin
  WriteLn('=== SBW Signature Tests ===');
  WriteLn;

  TestParseSimpleTypes;
  TestParseVoidReturn;
  TestParseArrayTypes;
  TestParse2DArrayTypes;
  TestParseListTypes;
  TestParseSimpleSignature;
  TestParseNoArgs;
  TestParseMultipleArgs;
  TestParseArrayArg;
  TestParseListReturn;
  TestParseVarArgs;
  TestParseComplexSignature;
  TestParseNamedArgs;
  TestParseUnnamedArgs;
  TestParseMixedNamedArgs;
  TestRoundTrip;
  TestInvalidSignature;
end;

end.

