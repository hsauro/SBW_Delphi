unit SBW.Tests.Message;

{******************************************************************************
 * SBW.Tests.Message.pas
 *
 * Unit tests for TSBWMessage classes and message framing.
 *****************************************************************************}

interface

uses
  System.SysUtils,
  SBW.Types, SBW.DataBlock, SBW.Message;

type
  TMessageTests = class
  private
    FTestCount: Integer;
    FPassCount: Integer;
    FFailCount: Integer;

    procedure Check(Condition: Boolean; const TestName: string);
    procedure CheckEquals(Expected, Actual: SBWInteger; const TestName: string); overload;
    procedure CheckEquals(const Expected, Actual: string; const TestName: string); overload;
  public
    constructor Create;

    procedure TestCallMessageRoundTrip;
    procedure TestSendMessageRoundTrip;
    procedure TestReplyMessageRoundTrip;
    procedure TestErrorMessageRoundTrip;
    procedure TestCallMessageWithPayload;
    procedure TestReplyMessageWithPayload;
    procedure TestMessageLength;
    procedure TestUIDGenerator;
    procedure TestParseCallMessage;
    procedure TestParseReplyMessage;
    procedure TestParseErrorMessage;

    procedure RunAllTests;

    property TestCount: Integer read FTestCount;
    property PassCount: Integer read FPassCount;
    property FailCount: Integer read FFailCount;
  end;

procedure RunMessageTests;

implementation

procedure RunMessageTests;
var
  Tests: TMessageTests;
begin
  Tests := TMessageTests.Create;
  try
    Tests.RunAllTests;
    WriteLn;
    WriteLn(Format('Message Tests: %d tests, %d passed, %d failed',
      [Tests.TestCount, Tests.PassCount, Tests.FailCount]));
  finally
    Tests.Free;
  end;
end;

{ TMessageTests }

constructor TMessageTests.Create;
begin
  inherited Create;
  FTestCount := 0;
  FPassCount := 0;
  FFailCount := 0;
end;

procedure TMessageTests.Check(Condition: Boolean; const TestName: string);
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

procedure TMessageTests.CheckEquals(Expected, Actual: SBWInteger; const TestName: string);
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

procedure TMessageTests.CheckEquals(const Expected, Actual: string; const TestName: string);
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

procedure TMessageTests.TestCallMessageRoundTrip;
var
  Msg: TSBWCallMessage;
  Data: TBytes;
  Parsed: TSBWCallMessage;
begin
  WriteLn('TestCallMessageRoundTrip');

  Msg := TSBWCallMessage.Create;
  try
    Msg.DestinationID := 100;
    Msg.SourceID := 200;
    Msg.UID := 12345;
    Msg.ServiceID := 1;
    Msg.MethodID := 2;
    Msg.MessageType := mtCall;
    Msg.Payload := [];

    Data := Msg.ToBytes;
    CheckEquals(SBW_CALL_HEADER_SIZE, Length(Data), 'Empty payload message size');
  finally
    Msg.Free;
  end;

  Parsed := TSBWCallMessage.FromBytes(Data);
  try
    CheckEquals(100, Parsed.DestinationID, 'DestinationID');
    CheckEquals(200, Parsed.SourceID, 'SourceID');
    CheckEquals(12345, Parsed.UID, 'UID');
    CheckEquals(1, Parsed.ServiceID, 'ServiceID');
    CheckEquals(2, Parsed.MethodID, 'MethodID');
    Check(Parsed.MessageType = mtCall, 'MessageType is Call');
    CheckEquals(0, Length(Parsed.Payload), 'Empty payload');
  finally
    Parsed.Free;
  end;
end;

procedure TMessageTests.TestSendMessageRoundTrip;
var
  Msg: TSBWCallMessage;
  Data: TBytes;
  Parsed: TSBWCallMessage;
begin
  WriteLn('TestSendMessageRoundTrip');

  Msg := TSBWCallMessage.Create;
  try
    Msg.DestinationID := 50;
    Msg.SourceID := 60;
    Msg.UID := 999;
    Msg.ServiceID := 3;
    Msg.MethodID := 4;
    Msg.MessageType := mtSend;
    Msg.Payload := [];
    Data := Msg.ToBytes;
  finally
    Msg.Free;
  end;

  Parsed := TSBWCallMessage.FromBytes(Data);
  try
    Check(Parsed.MessageType = mtSend, 'MessageType is Send');
    CheckEquals(50, Parsed.DestinationID, 'DestinationID');
  finally
    Parsed.Free;
  end;
end;

procedure TMessageTests.TestReplyMessageRoundTrip;
var
  Msg: TSBWReplyMessage;
  Data: TBytes;
  Parsed: TSBWReplyMessage;
begin
  WriteLn('TestReplyMessageRoundTrip');

  Msg := TSBWReplyMessage.Create;
  try
    Msg.DestinationID := 42;
    Msg.UID := 7777;
    Msg.Payload := [];
    Data := Msg.ToBytes;
    CheckEquals(SBW_REPLY_HEADER_SIZE, Length(Data), 'Empty reply message size');
  finally
    Msg.Free;
  end;

  Parsed := TSBWReplyMessage.FromBytes(Data);
  try
    CheckEquals(42, Parsed.DestinationID, 'DestinationID');
    CheckEquals(7777, Parsed.UID, 'UID');
    Check(Parsed.MessageType = mtReply, 'MessageType is Reply');
    CheckEquals(0, Length(Parsed.Payload), 'Empty payload');
  finally
    Parsed.Free;
  end;
end;

procedure TMessageTests.TestErrorMessageRoundTrip;
var
  Msg: TSBWErrorMessage;
  Data: TBytes;
  Parsed: TSBWErrorMessage;
begin
  WriteLn('TestErrorMessageRoundTrip');

  Msg := TSBWErrorMessage.Create;
  try
    Msg.DestinationID := 10;
    Msg.UID := 5555;
    Msg.ErrorCode := ecServiceNotFound;
    Msg.ErrorMessage := 'Service not found';
    Msg.DetailedMessage := 'The service "foo" does not exist on module 10';
    Data := Msg.ToBytes;
  finally
    Msg.Free;
  end;

  Parsed := TSBWErrorMessage.FromBytes(Data);
  try
    CheckEquals(10, Parsed.DestinationID, 'DestinationID');
    CheckEquals(5555, Parsed.UID, 'UID');
    Check(Parsed.MessageType = mtError, 'MessageType is Error');
    Check(Parsed.ErrorCode = ecServiceNotFound, 'ErrorCode');
    CheckEquals('Service not found', Parsed.ErrorMessage, 'ErrorMessage');
    CheckEquals('The service "foo" does not exist on module 10', Parsed.DetailedMessage, 'DetailedMessage');
  finally
    Parsed.Free;
  end;
end;

procedure TMessageTests.TestCallMessageWithPayload;
var
  Writer: TSBWDataBlockWriter;
  Payload: TBytes;
  Data: TBytes;
  Parsed: TSBWCallMessage;
  Reader: TSBWDataBlockReader;
begin
  WriteLn('TestCallMessageWithPayload');

  // Create a payload with some data
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteInteger(42);
    Writer.WriteString('hello');
    Writer.WriteDouble(3.14);
    Payload := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Data := TSBWMessageWriter.BuildCallMessage(1, 2, 1000, 3, 4, Payload, False);

  Parsed := TSBWCallMessage.FromBytes(Data);
  try
    CheckEquals(1, Parsed.DestinationID, 'DestinationID');
    CheckEquals(2, Parsed.SourceID, 'SourceID');
    CheckEquals(1000, Parsed.UID, 'UID');
    CheckEquals(3, Parsed.ServiceID, 'ServiceID');
    CheckEquals(4, Parsed.MethodID, 'MethodID');
    CheckEquals(Length(Payload), Length(Parsed.Payload), 'Payload length');

    // Verify payload contents
    Reader := TSBWDataBlockReader.Create(Parsed.Payload);
    try
      CheckEquals(42, Reader.ReadInteger, 'Payload integer');
      CheckEquals('hello', Reader.ReadString, 'Payload string');
      Check(Abs(Reader.ReadDouble - 3.14) < 0.001, 'Payload double');
    finally
      Reader.Free;
    end;
  finally
    Parsed.Free;
  end;
end;

procedure TMessageTests.TestReplyMessageWithPayload;
var
  Writer: TSBWDataBlockWriter;
  Payload: TBytes;
  Data: TBytes;
  Parsed: TSBWReplyMessage;
  Reader: TSBWDataBlockReader;
begin
  WriteLn('TestReplyMessageWithPayload');

  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteDoubleArray([1.0, 2.0, 3.0, 4.0, 5.0]);
    Payload := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Data := TSBWMessageWriter.BuildReplyMessage(99, 8888, Payload);

  Parsed := TSBWReplyMessage.FromBytes(Data);
  try
    CheckEquals(99, Parsed.DestinationID, 'DestinationID');
    CheckEquals(8888, Parsed.UID, 'UID');

    Reader := TSBWDataBlockReader.Create(Parsed.Payload);
    try
      var Arr := Reader.ReadDoubleArray;
      CheckEquals(5, Length(Arr), 'Array length');
      Check(Abs(Arr[4] - 5.0) < 0.001, 'Array element 4');
    finally
      Reader.Free;
    end;
  finally
    Parsed.Free;
  end;
end;

procedure TMessageTests.TestMessageLength;
var
  Msg: TSBWCallMessage;
  Data: TBytes;
  ReadLength: SBWInteger;
begin
  WriteLn('TestMessageLength');

  Msg := TSBWCallMessage.Create;
  try
    Msg.DestinationID := 1;
    Msg.SourceID := 2;
    Msg.UID := 3;
    Msg.ServiceID := 4;
    Msg.MethodID := 5;
    Msg.Payload := [10, 20, 30, 40, 50]; // 5 bytes payload
    Data := Msg.ToBytes;

    // Length should be header (25) + payload (5) = 30
    CheckEquals(30, Msg.GetLength, 'GetLength');
    CheckEquals(30, Length(Data), 'ToBytes length');

    // Read length from bytes
    ReadLength := TSBWMessageReader.ReadMessageLength(Data);
    CheckEquals(30, ReadLength, 'ReadMessageLength');
  finally
    Msg.Free;
  end;
end;

procedure TMessageTests.TestUIDGenerator;
var
  UID1, UID2, UID3: SBWInteger;
begin
  WriteLn('TestUIDGenerator');

  UID1 := TSBWUIDGenerator.NextUID;
  UID2 := TSBWUIDGenerator.NextUID;
  UID3 := TSBWUIDGenerator.NextUID;

  Check(UID2 = UID1 + 1, 'UIDs are sequential (1)');
  Check(UID3 = UID2 + 1, 'UIDs are sequential (2)');
  Check(UID1 > 0, 'UIDs are positive');
end;

procedure TMessageTests.TestParseCallMessage;
var
  Data: TBytes;
  Msg: TSBWMessage;
begin
  WriteLn('TestParseCallMessage');

  Data := TSBWMessageWriter.BuildCallMessage(10, 20, 100, 1, 2, [], False);
  Msg := TSBWMessageReader.ParseMessage(Data);
  try
    Check(Msg is TSBWCallMessage, 'Parsed as TSBWCallMessage');
    Check(Msg.MessageType = mtCall, 'MessageType is Call');
    CheckEquals(10, Msg.DestinationID, 'DestinationID');
  finally
    Msg.Free;
  end;
end;

procedure TMessageTests.TestParseReplyMessage;
var
  Data: TBytes;
  Msg: TSBWMessage;
begin
  WriteLn('TestParseReplyMessage');

  Data := TSBWMessageWriter.BuildReplyMessage(30, 200, []);
  Msg := TSBWMessageReader.ParseMessage(Data);
  try
    Check(Msg is TSBWReplyMessage, 'Parsed as TSBWReplyMessage');
    Check(Msg.MessageType = mtReply, 'MessageType is Reply');
    CheckEquals(30, Msg.DestinationID, 'DestinationID');
  finally
    Msg.Free;
  end;
end;

procedure TMessageTests.TestParseErrorMessage;
var
  Data: TBytes;
  Msg: TSBWMessage;
  ErrMsg: TSBWErrorMessage;
begin
  WriteLn('TestParseErrorMessage');

  Data := TSBWMessageWriter.BuildErrorMessage(40, 300, ecCommunication, 'Connection lost', 'Details here');
  Msg := TSBWMessageReader.ParseMessage(Data);
  try
    Check(Msg is TSBWErrorMessage, 'Parsed as TSBWErrorMessage');
    Check(Msg.MessageType = mtError, 'MessageType is Error');

    ErrMsg := Msg as TSBWErrorMessage;
    Check(ErrMsg.ErrorCode = ecCommunication, 'ErrorCode');
    CheckEquals('Connection lost', ErrMsg.ErrorMessage, 'ErrorMessage');
    CheckEquals('Details here', ErrMsg.DetailedMessage, 'DetailedMessage');
  finally
    Msg.Free;
  end;
end;

procedure TMessageTests.RunAllTests;
begin
  WriteLn('=== SBW Message Tests ===');
  WriteLn;

  TestCallMessageRoundTrip;
  TestSendMessageRoundTrip;
  TestReplyMessageRoundTrip;
  TestErrorMessageRoundTrip;
  TestCallMessageWithPayload;
  TestReplyMessageWithPayload;
  TestMessageLength;
  TestUIDGenerator;
  TestParseCallMessage;
  TestParseReplyMessage;
  TestParseErrorMessage;
end;

end.

