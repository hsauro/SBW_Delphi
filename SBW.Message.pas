unit SBW.Message;

{******************************************************************************
 * SBW.Message.pas
 *
 * Message framing for the Systems Biology Workbench.
 *
 * Handles the wire format for SBW messages including:
 * - Call/Send messages (25-byte header + payload)
 * - Reply messages (13-byte header + payload)
 * - Error messages (13-byte header + error data)
 *
 * Wire format (little-endian):
 *
 * Call/Send Message:
 *   Length (4)        - Total message size including this field
 *   Destination ID (4) - Target module instance
 *   Type (1)          - 0=Send, 1=Call
 *   UID (4)           - Unique message ID for matching replies
 *   Source ID (4)     - Calling module instance
 *   Service ID (4)    - Which service on destination
 *   Method ID (4)     - Which method on service
 *   Payload           - DataBlock with arguments
 *
 * Reply Message:
 *   Length (4)
 *   Destination ID (4)
 *   Type (1)          - 2=Reply
 *   UID (4)
 *   Payload           - DataBlock with result
 *
 * Error Message:
 *   Length (4)
 *   Destination ID (4)
 *   Type (1)          - 3=Error
 *   UID (4)
 *   Error Code (1)    - Raw byte, no type prefix
 *   Message           - String with type prefix
 *   Detailed Message  - String with type prefix
 *****************************************************************************}

interface

uses
  System.SysUtils, System.Classes,
  SBW.Types, SBW.DataBlock;

type
  /// <summary>
  /// Exception error codes transmitted in error messages
  /// </summary>
  TSBWExceptionCode = (
    ecApplication                    = 0,
    ecRaw                            = 1,
    ecCommunication                  = 2,
    ecModuleStart                    = 3,
    ecIncompatibleMethodSignature    = 4,
    ecTypeMismatch                   = 5,
    ecModuleIdSyntax                 = 6,
    ecIncorrectCategorySyntax        = 7,
    ecServiceNotFound                = 8,
    ecMethodTypeNotBlockType         = 9,
    ecMethodAmbiguous                = 10,
    ecUnsupportedObjectType          = 11,
    ecMethodNotFound                 = 12,
    ecSignatureSyntax                = 13,
    ecModuleDefinition               = 14,
    ecModuleNotFound                 = 15,
    ecBrokerStart                    = 16
  );

  /// <summary>
  /// Base class for SBW messages
  /// </summary>
  TSBWMessage = class
  private
    FDestinationID: SBWModuleID;
    FMessageType: TSBWMessageType;
    FUID: SBWInteger;
  public
    property DestinationID: SBWModuleID read FDestinationID write FDestinationID;
    property MessageType: TSBWMessageType read FMessageType write FMessageType;
    property UID: SBWInteger read FUID write FUID;

    /// <summary>
    /// Serialize the message to bytes for transmission
    /// </summary>
    function ToBytes: TBytes; virtual; abstract;

    /// <summary>
    /// Get total message length including the length field
    /// </summary>
    function GetLength: SBWInteger; virtual; abstract;
  end;

  /// <summary>
  /// Call or Send message - invokes a method on a remote module
  /// </summary>
  TSBWCallMessage = class(TSBWMessage)
  private
    FSourceID: SBWModuleID;
    FServiceID: SBWServiceID;
    FMethodID: SBWMethodID;
    FPayload: TBytes;
  public
    constructor Create;

    property SourceID: SBWModuleID read FSourceID write FSourceID;
    property ServiceID: SBWServiceID read FServiceID write FServiceID;
    property MethodID: SBWMethodID read FMethodID write FMethodID;
    property Payload: TBytes read FPayload write FPayload;

    function ToBytes: TBytes; override;
    function GetLength: SBWInteger; override;

    /// <summary>
    /// Parse a call message from bytes (excluding the already-read length)
    /// </summary>
    class function FromBytes(const Data: TBytes; StartIndex: Integer = 0): TSBWCallMessage;
  end;

  /// <summary>
  /// Reply message - result from a method call
  /// </summary>
  TSBWReplyMessage = class(TSBWMessage)
  private
    FPayload: TBytes;
  public
    constructor Create;

    property Payload: TBytes read FPayload write FPayload;

    function ToBytes: TBytes; override;
    function GetLength: SBWInteger; override;

    class function FromBytes(const Data: TBytes; StartIndex: Integer = 0): TSBWReplyMessage;
  end;

  /// <summary>
  /// Error message - exception from a method call
  /// </summary>
  TSBWErrorMessage = class(TSBWMessage)
  private
    FErrorCode: TSBWExceptionCode;
    FErrorMessage: string;
    FDetailedMessage: string;
  public
    constructor Create;

    property ErrorCode: TSBWExceptionCode read FErrorCode write FErrorCode;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    property DetailedMessage: string read FDetailedMessage write FDetailedMessage;

    function ToBytes: TBytes; override;
    function GetLength: SBWInteger; override;

    class function FromBytes(const Data: TBytes; StartIndex: Integer = 0): TSBWErrorMessage;
  end;

  /// <summary>
  /// Message reader - reads messages from a stream/socket
  /// </summary>
  TSBWMessageReader = class
  private
    class function ReadInt32(const Data: TBytes; var Index: Integer): Int32;
    class function ReadByte(const Data: TBytes; var Index: Integer): Byte;
  public
    /// <summary>
    /// Parse a message from raw bytes. The Data should include the length field.
    /// Returns TSBWCallMessage, TSBWReplyMessage, or TSBWErrorMessage.
    /// </summary>
    class function ParseMessage(const Data: TBytes): TSBWMessage;

    /// <summary>
    /// Read the message length from the first 4 bytes
    /// </summary>
    class function ReadMessageLength(const Data: TBytes): SBWInteger;
  end;

  /// <summary>
  /// Message writer - helper for building messages
  /// </summary>
  TSBWMessageWriter = class
  private
    class procedure WriteInt32(Stream: TMemoryStream; Value: Int32);
    class procedure WriteByte(Stream: TMemoryStream; Value: Byte);
  public
    /// <summary>
    /// Build a Call message
    /// </summary>
    class function BuildCallMessage(
      DestinationID: SBWModuleID;
      SourceID: SBWModuleID;
      UID: SBWInteger;
      ServiceID: SBWServiceID;
      MethodID: SBWMethodID;
      const Payload: TBytes;
      IsSend: Boolean = False
    ): TBytes;

    /// <summary>
    /// Build a Reply message
    /// </summary>
    class function BuildReplyMessage(
      DestinationID: SBWModuleID;
      UID: SBWInteger;
      const Payload: TBytes
    ): TBytes;

    /// <summary>
    /// Build an Error message
    /// </summary>
    class function BuildErrorMessage(
      DestinationID: SBWModuleID;
      UID: SBWInteger;
      ErrorCode: TSBWExceptionCode;
      const ErrorMsg: string;
      const DetailedMsg: string
    ): TBytes;
  end;

  /// <summary>
  /// UID generator for message tracking
  /// </summary>
  TSBWUIDGenerator = class
  private
    class var FNextUID: SBWInteger;
    class var FLock: TObject;
  public
    class constructor Create;
    class destructor Destroy;
    class function NextUID: SBWInteger;
  end;

const
  /// <summary>
  /// Header sizes
  /// </summary>
  SBW_CALL_HEADER_SIZE = 25;   // Length(4) + DestID(4) + Type(1) + UID(4) + SrcID(4) + SvcID(4) + MethID(4)
  SBW_REPLY_HEADER_SIZE = 13;  // Length(4) + DestID(4) + Type(1) + UID(4)

implementation

uses
  System.SyncObjs;

{ TSBWCallMessage }

constructor TSBWCallMessage.Create;
begin
  inherited Create;
  FMessageType := mtCall;
end;

function TSBWCallMessage.GetLength: SBWInteger;
begin
  // Total length including the length field itself
  Result := SBW_CALL_HEADER_SIZE + Length(FPayload);
end;

function TSBWCallMessage.ToBytes: TBytes;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    TSBWMessageWriter.WriteInt32(Stream, GetLength);
    TSBWMessageWriter.WriteInt32(Stream, FDestinationID);
    TSBWMessageWriter.WriteByte(Stream, Ord(FMessageType));
    TSBWMessageWriter.WriteInt32(Stream, FUID);
    TSBWMessageWriter.WriteInt32(Stream, FSourceID);
    TSBWMessageWriter.WriteInt32(Stream, FServiceID);
    TSBWMessageWriter.WriteInt32(Stream, FMethodID);
    if Length(FPayload) > 0 then
      Stream.WriteBuffer(FPayload[0], Length(FPayload));

    SetLength(Result, Stream.Size);
    Stream.Position := 0;
    Stream.ReadBuffer(Result[0], Stream.Size);
  finally
    Stream.Free;
  end;
end;

class function TSBWCallMessage.FromBytes(const Data: TBytes; StartIndex: Integer): TSBWCallMessage;
var
  Index: Integer;
  MsgLength: SBWInteger;
  PayloadLength: Integer;
begin
  Result := TSBWCallMessage.Create;
  Index := StartIndex;

  MsgLength := TSBWMessageReader.ReadInt32(Data, Index);
  Result.FDestinationID := TSBWMessageReader.ReadInt32(Data, Index);
  Result.FMessageType := TSBWMessageType(TSBWMessageReader.ReadByte(Data, Index));
  Result.FUID := TSBWMessageReader.ReadInt32(Data, Index);
  Result.FSourceID := TSBWMessageReader.ReadInt32(Data, Index);
  Result.FServiceID := TSBWMessageReader.ReadInt32(Data, Index);
  Result.FMethodID := TSBWMessageReader.ReadInt32(Data, Index);

  PayloadLength := MsgLength - SBW_CALL_HEADER_SIZE;
  if PayloadLength > 0 then
  begin
    SetLength(Result.FPayload, PayloadLength);
    Move(Data[Index], Result.FPayload[0], PayloadLength);
  end;
end;

{ TSBWReplyMessage }

constructor TSBWReplyMessage.Create;
begin
  inherited Create;
  FMessageType := mtReply;
end;

function TSBWReplyMessage.GetLength: SBWInteger;
begin
  Result := SBW_REPLY_HEADER_SIZE + Length(FPayload);
end;

function TSBWReplyMessage.ToBytes: TBytes;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    TSBWMessageWriter.WriteInt32(Stream, GetLength);
    TSBWMessageWriter.WriteInt32(Stream, FDestinationID);
    TSBWMessageWriter.WriteByte(Stream, Ord(FMessageType));
    TSBWMessageWriter.WriteInt32(Stream, FUID);
    if Length(FPayload) > 0 then
      Stream.WriteBuffer(FPayload[0], Length(FPayload));

    SetLength(Result, Stream.Size);
    Stream.Position := 0;
    Stream.ReadBuffer(Result[0], Stream.Size);
  finally
    Stream.Free;
  end;
end;

class function TSBWReplyMessage.FromBytes(const Data: TBytes; StartIndex: Integer): TSBWReplyMessage;
var
  Index: Integer;
  MsgLength: SBWInteger;
  PayloadLength: Integer;
begin
  Result := TSBWReplyMessage.Create;
  Index := StartIndex;

  MsgLength := TSBWMessageReader.ReadInt32(Data, Index);
  Result.FDestinationID := TSBWMessageReader.ReadInt32(Data, Index);
  Result.FMessageType := TSBWMessageType(TSBWMessageReader.ReadByte(Data, Index));
  Result.FUID := TSBWMessageReader.ReadInt32(Data, Index);

  PayloadLength := MsgLength - SBW_REPLY_HEADER_SIZE;
  if PayloadLength > 0 then
  begin
    SetLength(Result.FPayload, PayloadLength);
    Move(Data[Index], Result.FPayload[0], PayloadLength);
  end;
end;

{ TSBWErrorMessage }

constructor TSBWErrorMessage.Create;
begin
  inherited Create;
  FMessageType := mtError;
end;

function TSBWErrorMessage.GetLength: SBWInteger;
var
  Writer: TSBWDataBlockWriter;
  ErrorPayload: TBytes;
begin
  // Calculate payload size: error code (1) + message string + detailed string
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteString(FErrorMessage);
    Writer.WriteString(FDetailedMessage);
    ErrorPayload := Writer.ToBytes;
  finally
    Writer.Free;
  end;
  Result := SBW_REPLY_HEADER_SIZE + 1 + Length(ErrorPayload);
end;

function TSBWErrorMessage.ToBytes: TBytes;
var
  Stream: TMemoryStream;
  Writer: TSBWDataBlockWriter;
  StringPayload: TBytes;
begin
  // Build the string payload using DataBlockWriter
  Writer := TSBWDataBlockWriter.Create;
  try
    Writer.WriteString(FErrorMessage);
    Writer.WriteString(FDetailedMessage);
    StringPayload := Writer.ToBytes;
  finally
    Writer.Free;
  end;

  Stream := TMemoryStream.Create;
  try
    TSBWMessageWriter.WriteInt32(Stream, SBW_REPLY_HEADER_SIZE + 1 + Length(StringPayload));
    TSBWMessageWriter.WriteInt32(Stream, FDestinationID);
    TSBWMessageWriter.WriteByte(Stream, Ord(FMessageType));
    TSBWMessageWriter.WriteInt32(Stream, FUID);
    TSBWMessageWriter.WriteByte(Stream, Ord(FErrorCode)); // Raw byte, no type prefix
    if Length(StringPayload) > 0 then
      Stream.WriteBuffer(StringPayload[0], Length(StringPayload));

    SetLength(Result, Stream.Size);
    Stream.Position := 0;
    Stream.ReadBuffer(Result[0], Stream.Size);
  finally
    Stream.Free;
  end;
end;

class function TSBWErrorMessage.FromBytes(const Data: TBytes; StartIndex: Integer): TSBWErrorMessage;
var
  Index: Integer;
  MsgLength: SBWInteger;
  PayloadStart: Integer;
  PayloadLength: Integer;
  PayloadData: TBytes;
  Reader: TSBWDataBlockReader;
begin
  Result := TSBWErrorMessage.Create;
  Index := StartIndex;

  MsgLength := TSBWMessageReader.ReadInt32(Data, Index);
  Result.FDestinationID := TSBWMessageReader.ReadInt32(Data, Index);
  Result.FMessageType := TSBWMessageType(TSBWMessageReader.ReadByte(Data, Index));
  Result.FUID := TSBWMessageReader.ReadInt32(Data, Index);

  // Error code is a raw byte (no type prefix)
  Result.FErrorCode := TSBWExceptionCode(TSBWMessageReader.ReadByte(Data, Index));

  // Remaining bytes are the two strings with type prefixes
  PayloadStart := Index;
  PayloadLength := MsgLength - SBW_REPLY_HEADER_SIZE - 1;
  if PayloadLength > 0 then
  begin
    SetLength(PayloadData, PayloadLength);
    Move(Data[PayloadStart], PayloadData[0], PayloadLength);
    Reader := TSBWDataBlockReader.Create(PayloadData);
    try
      Result.FErrorMessage := Reader.ReadString;
      Result.FDetailedMessage := Reader.ReadString;
    finally
      Reader.Free;
    end;
  end;
end;

{ TSBWMessageReader }

class function TSBWMessageReader.ReadInt32(const Data: TBytes; var Index: Integer): Int32;
begin
  Move(Data[Index], Result, 4);
  Inc(Index, 4);
end;

class function TSBWMessageReader.ReadByte(const Data: TBytes; var Index: Integer): Byte;
begin
  Result := Data[Index];
  Inc(Index);
end;

class function TSBWMessageReader.ReadMessageLength(const Data: TBytes): SBWInteger;
begin
  Move(Data[0], Result, 4);
end;

class function TSBWMessageReader.ParseMessage(const Data: TBytes): TSBWMessage;
var
  Index: Integer;
  MsgType: TSBWMessageType;
begin
  // Read message type at offset 8 (after Length and DestinationID)
  Index := 8;
  MsgType := TSBWMessageType(ReadByte(Data, Index));

  case MsgType of
    mtSend, mtCall:
      Result := TSBWCallMessage.FromBytes(Data);
    mtReply:
      Result := TSBWReplyMessage.FromBytes(Data);
    mtError:
      Result := TSBWErrorMessage.FromBytes(Data);
  else
    raise ESBWDataBlockError.CreateFmt('Unknown message type: %d', [Ord(MsgType)]);
  end;
end;

{ TSBWMessageWriter }

class procedure TSBWMessageWriter.WriteInt32(Stream: TMemoryStream; Value: Int32);
begin
  Stream.WriteBuffer(Value, 4);
end;

class procedure TSBWMessageWriter.WriteByte(Stream: TMemoryStream; Value: Byte);
begin
  Stream.WriteBuffer(Value, 1);
end;

class function TSBWMessageWriter.BuildCallMessage(
  DestinationID: SBWModuleID;
  SourceID: SBWModuleID;
  UID: SBWInteger;
  ServiceID: SBWServiceID;
  MethodID: SBWMethodID;
  const Payload: TBytes;
  IsSend: Boolean
): TBytes;
var
  Msg: TSBWCallMessage;
begin
  Msg := TSBWCallMessage.Create;
  try
    Msg.DestinationID := DestinationID;
    Msg.SourceID := SourceID;
    Msg.UID := UID;
    Msg.ServiceID := ServiceID;
    Msg.MethodID := MethodID;
    Msg.Payload := Payload;
    if IsSend then
      Msg.MessageType := mtSend
    else
      Msg.MessageType := mtCall;
    Result := Msg.ToBytes;
  finally
    Msg.Free;
  end;
end;

class function TSBWMessageWriter.BuildReplyMessage(
  DestinationID: SBWModuleID;
  UID: SBWInteger;
  const Payload: TBytes
): TBytes;
var
  Msg: TSBWReplyMessage;
begin
  Msg := TSBWReplyMessage.Create;
  try
    Msg.DestinationID := DestinationID;
    Msg.UID := UID;
    Msg.Payload := Payload;
    Result := Msg.ToBytes;
  finally
    Msg.Free;
  end;
end;

class function TSBWMessageWriter.BuildErrorMessage(
  DestinationID: SBWModuleID;
  UID: SBWInteger;
  ErrorCode: TSBWExceptionCode;
  const ErrorMsg: string;
  const DetailedMsg: string
): TBytes;
var
  Msg: TSBWErrorMessage;
begin
  Msg := TSBWErrorMessage.Create;
  try
    Msg.DestinationID := DestinationID;
    Msg.UID := UID;
    Msg.ErrorCode := ErrorCode;
    Msg.ErrorMessage := ErrorMsg;
    Msg.DetailedMessage := DetailedMsg;
    Result := Msg.ToBytes;
  finally
    Msg.Free;
  end;
end;

{ TSBWUIDGenerator }

class constructor TSBWUIDGenerator.Create;
begin
  FNextUID := 1;
  FLock := TObject.Create;
end;

class destructor TSBWUIDGenerator.Destroy;
begin
  FLock.Free;
end;

class function TSBWUIDGenerator.NextUID: SBWInteger;
begin
  TMonitor.Enter(FLock);
  try
    Result := FNextUID;
    Inc(FNextUID);
  finally
    TMonitor.Exit(FLock);
  end;
end;

end.

