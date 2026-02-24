unit SBW.DataBlock;

{******************************************************************************
 * SBW.DataBlock.pas
 *
 * Data block serialization for the Systems Biology Workbench.
 *
 * DataBlocks are heterogeneous containers used to encode method arguments
 * and return values. This unit provides TSBWDataBlockWriter for encoding
 * and TSBWDataBlockReader for decoding.
 *
 * Wire format (little-endian):
 *   - Each value is preceded by a type byte
 *   - Byte: type(1) + value(1)
 *   - Boolean: type(1) + value(1) where 0=false, 1=true
 *   - Integer: type(1) + value(4) signed 32-bit
 *   - Double: type(1) + value(8) IEEE 64-bit
 *   - String: type(1) + length(4) + chars + null terminator
 *   - Complex: type(1) + real(8) + imag(8)
 *   - Array: type(1) + elementType(1) + dimensions(4) + sizes(4*d) + data
 *   - List: type(1) + count(4) + items...
 *****************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  SBW.Types;

type
  ESBWDataBlockError = class(Exception);
  ESBWTypeMismatch = class(ESBWDataBlockError);
  ESBWReadPastEnd = class(ESBWDataBlockError);

  /// <summary>
  /// Forward declaration for nested list support
  /// </summary>
  TSBWDataBlockWriter = class;
  TSBWDataBlockReader = class;

  /// <summary>
  /// DataBlockWriter - encodes data into SBW binary format
  /// </summary>
  TSBWDataBlockWriter = class
  private
    FStream: TMemoryStream;
    FOwnsStream: Boolean;

  public
    procedure WriteTypeByte(AType: TSBWDataBlockType);
    procedure WriteRawByte(Value: Byte);
    procedure WriteRawInt32(Value: Int32);
    procedure WriteRawDouble(Value: Double);
    procedure WriteRawBytes(const Data: TBytes);

    constructor Create; overload;
    constructor Create(AStream: TMemoryStream; OwnsStream: Boolean = False); overload;
    destructor Destroy; override;

    /// <summary>
    /// Write a byte value
    /// </summary>
    function WriteByte(Value: SBWByte): TSBWDataBlockWriter;

    /// <summary>
    /// Write a boolean value
    /// </summary>
    function WriteBoolean(Value: SBWBoolean): TSBWDataBlockWriter;

    /// <summary>
    /// Write a 32-bit integer
    /// </summary>
    function WriteInteger(Value: SBWInteger): TSBWDataBlockWriter;

    /// <summary>
    /// Write a 64-bit double
    /// </summary>
    function WriteDouble(Value: SBWDouble): TSBWDataBlockWriter;

    /// <summary>
    /// Write a string (UTF-8 encoded on wire)
    /// </summary>
    function WriteString(const Value: string): TSBWDataBlockWriter;

    /// <summary>
    /// Write a complex number
    /// </summary>
    function WriteComplex(const Value: TSBWComplex): TSBWDataBlockWriter;

    /// <summary>
    /// Write a 1D array of integers
    /// </summary>
    function WriteIntegerArray(const Values: TArray<SBWInteger>): TSBWDataBlockWriter;

    /// <summary>
    /// Write a 1D array of doubles
    /// </summary>
    function WriteDoubleArray(const Values: TArray<SBWDouble>): TSBWDataBlockWriter;

    /// <summary>
    /// Write a 1D array of strings
    /// </summary>
    function WriteStringArray(const Values: TArray<string>): TSBWDataBlockWriter;

    /// <summary>
    /// Write a 1D array of bytes
    /// </summary>
    function WriteByteArray(const Values: TArray<SBWByte>): TSBWDataBlockWriter;

    /// <summary>
    /// Write a 2D array of doubles
    /// </summary>
    function WriteDoubleArray2D(const Values: TArray<TArray<SBWDouble>>): TSBWDataBlockWriter;

    /// <summary>
    /// Write a 2D array of integers
    /// </summary>
    function WriteIntegerArray2D(const Values: TArray<TArray<SBWInteger>>): TSBWDataBlockWriter;

    /// <summary>
    /// Begin writing a list with known item count
    /// </summary>
    function WriteListBegin(ItemCount: SBWInteger): TSBWDataBlockWriter;

    /// <summary>
    /// Write an embedded data block (list item)
    /// </summary>
    function WriteDataBlock(Writer: TSBWDataBlockWriter): TSBWDataBlockWriter;

    /// <summary>
    /// Get the serialized data as bytes
    /// </summary>
    function ToBytes: TBytes;

    /// <summary>
    /// Get the current size in bytes
    /// </summary>
    function Size: Int64;

    /// <summary>
    /// Clear all data
    /// </summary>
    procedure Clear;

    /// <summary>
    /// Access to underlying stream (for advanced use)
    /// </summary>
    property Stream: TMemoryStream read FStream;
  end;

  /// <summary>
  /// DataBlockReader - decodes data from SBW binary format
  /// </summary>
  TSBWDataBlockReader = class
  private
    FStream: TMemoryStream;
    FOwnsStream: Boolean;

    function ReadRawByte: Byte;
    function ReadRawInt32: Int32;
    function ReadRawDouble: Double;
    function ReadRawBytes(Count: Integer): TBytes;

    procedure ExpectType(Expected: TSBWDataBlockType);
  public
    constructor Create(const Data: TBytes); overload;
    constructor Create(AStream: TMemoryStream; OwnsStream: Boolean = False); overload;
    destructor Destroy; override;

    /// <summary>
    /// Peek at the next type without consuming it
    /// </summary>
    function PeekNextType: TSBWDataBlockType;

    /// <summary>
    /// Check if more data is available
    /// </summary>
    function HasMore: Boolean;

    /// <summary>
    /// Read a byte value
    /// </summary>
    function ReadByte: SBWByte;

    /// <summary>
    /// Read a boolean value
    /// </summary>
    function ReadBoolean: SBWBoolean;

    /// <summary>
    /// Read a 32-bit integer
    /// </summary>
    function ReadInteger: SBWInteger;

    /// <summary>
    /// Read a 64-bit double
    /// </summary>
    function ReadDouble: SBWDouble;

    /// <summary>
    /// Read a string
    /// </summary>
    function ReadString: string;

    /// <summary>
    /// Read a complex number
    /// </summary>
    function ReadComplex: TSBWComplex;

    /// <summary>
    /// Read a 1D array of integers
    /// </summary>
    function ReadIntegerArray: TArray<SBWInteger>;

    /// <summary>
    /// Read a 1D array of doubles
    /// </summary>
    function ReadDoubleArray: TArray<SBWDouble>;

    /// <summary>
    /// Read a 1D array of strings
    /// </summary>
    function ReadStringArray: TArray<string>;

    /// <summary>
    /// Read a 1D array of bytes
    /// </summary>
    function ReadByteArray: TArray<SBWByte>;

    /// <summary>
    /// Read a 2D array of doubles
    /// </summary>
    function ReadDoubleArray2D: TArray<TArray<SBWDouble>>;

    /// <summary>
    /// Read a 2D array of integers
    /// </summary>
    function ReadIntegerArray2D: TArray<TArray<SBWInteger>>;

    /// <summary>
    /// Read list header, returns item count
    /// </summary>
    function ReadListBegin: SBWInteger;

    /// <summary>
    /// Get remaining bytes count
    /// </summary>
    function Remaining: Int64;

    /// <summary>
    /// Current position in stream
    /// </summary>
    function Position: Int64;

    /// <summary>
    /// Skip the next value (whatever type it is)
    /// </summary>
    procedure Skip;

    /// <summary>
    /// Access to underlying stream
    /// </summary>
    property Stream: TMemoryStream read FStream;
  end;

implementation

{ TSBWDataBlockWriter }

constructor TSBWDataBlockWriter.Create;
begin
  inherited Create;
  FStream := TMemoryStream.Create;
  FOwnsStream := True;
end;

constructor TSBWDataBlockWriter.Create(AStream: TMemoryStream; OwnsStream: Boolean);
begin
  inherited Create;
  FStream := AStream;
  FOwnsStream := OwnsStream;
end;

destructor TSBWDataBlockWriter.Destroy;
begin
  if FOwnsStream then
    FStream.Free;
  inherited;
end;

procedure TSBWDataBlockWriter.WriteTypeByte(AType: TSBWDataBlockType);
begin
  WriteRawByte(Ord(AType));
end;

procedure TSBWDataBlockWriter.WriteRawByte(Value: Byte);
begin
  FStream.WriteBuffer(Value, 1);
end;

procedure TSBWDataBlockWriter.WriteRawInt32(Value: Int32);
begin
  // Little-endian
  FStream.WriteBuffer(Value, SizeOf(Value));
end;

procedure TSBWDataBlockWriter.WriteRawDouble(Value: Double);
begin
  FStream.WriteBuffer(Value, SizeOf(Value));
end;

procedure TSBWDataBlockWriter.WriteRawBytes(const Data: TBytes);
begin
  if Length(Data) > 0 then
    FStream.WriteBuffer(Data[0], Length(Data));
end;

function TSBWDataBlockWriter.WriteByte(Value: SBWByte): TSBWDataBlockWriter;
begin
  WriteTypeByte(dbtByte);
  WriteRawByte(Value);
  Result := Self;
end;

function TSBWDataBlockWriter.WriteBoolean(Value: SBWBoolean): TSBWDataBlockWriter;
begin
  WriteTypeByte(dbtBoolean);
  if Value then
    WriteRawByte(1)
  else
    WriteRawByte(0);
  Result := Self;
end;

function TSBWDataBlockWriter.WriteInteger(Value: SBWInteger): TSBWDataBlockWriter;
begin
  WriteTypeByte(dbtInteger);
  WriteRawInt32(Value);
  Result := Self;
end;

function TSBWDataBlockWriter.WriteDouble(Value: SBWDouble): TSBWDataBlockWriter;
begin
  WriteTypeByte(dbtDouble);
  WriteRawDouble(Value);
  Result := Self;
end;

function TSBWDataBlockWriter.WriteString(const Value: string): TSBWDataBlockWriter;
var
  UTF8Bytes: TBytes;
begin
  WriteTypeByte(dbtString);
  UTF8Bytes := TEncoding.UTF8.GetBytes(Value);
  WriteRawInt32(Length(UTF8Bytes));
  WriteRawBytes(UTF8Bytes);
  WriteRawByte(0); // Null terminator
  Result := Self;
end;

function TSBWDataBlockWriter.WriteComplex(const Value: TSBWComplex): TSBWDataBlockWriter;
begin
  WriteTypeByte(dbtComplex);
  WriteRawDouble(Value.Real);
  WriteRawDouble(Value.Imag);
  Result := Self;
end;

function TSBWDataBlockWriter.WriteIntegerArray(const Values: TArray<SBWInteger>): TSBWDataBlockWriter;
var
  I: Integer;
begin
  WriteTypeByte(dbtArray);
  WriteRawByte(Ord(dbtInteger)); // Element type
  WriteRawInt32(1);              // 1 dimension
  WriteRawInt32(Length(Values)); // Size of dimension 0
  for I := 0 to High(Values) do
    WriteRawInt32(Values[I]);
  Result := Self;
end;

function TSBWDataBlockWriter.WriteDoubleArray(const Values: TArray<SBWDouble>): TSBWDataBlockWriter;
var
  I: Integer;
begin
  WriteTypeByte(dbtArray);
  WriteRawByte(Ord(dbtDouble)); // Element type
  WriteRawInt32(1);             // 1 dimension
  WriteRawInt32(Length(Values));
  for I := 0 to High(Values) do
    WriteRawDouble(Values[I]);
  Result := Self;
end;

function TSBWDataBlockWriter.WriteStringArray(const Values: TArray<string>): TSBWDataBlockWriter;
var
  I: Integer;
  UTF8Bytes: TBytes;
begin
  WriteTypeByte(dbtArray);
  WriteRawByte(Ord(dbtString)); // Element type
  WriteRawInt32(1);             // 1 dimension
  WriteRawInt32(Length(Values));
  for I := 0 to High(Values) do
  begin
    UTF8Bytes := TEncoding.UTF8.GetBytes(Values[I]);
    WriteRawInt32(Length(UTF8Bytes));
    WriteRawBytes(UTF8Bytes);
    WriteRawByte(0); // Null terminator
  end;
  Result := Self;
end;

function TSBWDataBlockWriter.WriteByteArray(const Values: TArray<SBWByte>): TSBWDataBlockWriter;
begin
  WriteTypeByte(dbtArray);
  WriteRawByte(Ord(dbtByte));   // Element type
  WriteRawInt32(1);             // 1 dimension
  WriteRawInt32(Length(Values));
  WriteRawBytes(Values);
  Result := Self;
end;

function TSBWDataBlockWriter.WriteDoubleArray2D(const Values: TArray<TArray<SBWDouble>>): TSBWDataBlockWriter;
var
  I, J: Integer;
  Rows, Cols: Integer;
begin
  WriteTypeByte(dbtArray);
  WriteRawByte(Ord(dbtDouble)); // Element type
  WriteRawInt32(2);             // 2 dimensions

  Rows := Length(Values);
  if Rows > 0 then
    Cols := Length(Values[0])
  else
    Cols := 0;

  WriteRawInt32(Rows);
  WriteRawInt32(Cols);

  for I := 0 to Rows - 1 do
    for J := 0 to Cols - 1 do
      WriteRawDouble(Values[I][J]);

  Result := Self;
end;

function TSBWDataBlockWriter.WriteIntegerArray2D(const Values: TArray<TArray<SBWInteger>>): TSBWDataBlockWriter;
var
  I, J: Integer;
  Rows, Cols: Integer;
begin
  WriteTypeByte(dbtArray);
  WriteRawByte(Ord(dbtInteger)); // Element type
  WriteRawInt32(2);              // 2 dimensions

  Rows := Length(Values);
  if Rows > 0 then
    Cols := Length(Values[0])
  else
    Cols := 0;

  WriteRawInt32(Rows);
  WriteRawInt32(Cols);

  for I := 0 to Rows - 1 do
    for J := 0 to Cols - 1 do
      WriteRawInt32(Values[I][J]);

  Result := Self;
end;

function TSBWDataBlockWriter.WriteListBegin(ItemCount: SBWInteger): TSBWDataBlockWriter;
begin
  WriteTypeByte(dbtList);
  WriteRawInt32(ItemCount);
  Result := Self;
end;

function TSBWDataBlockWriter.WriteDataBlock(Writer: TSBWDataBlockWriter): TSBWDataBlockWriter;
var
  Data: TBytes;
begin
  Data := Writer.ToBytes;
  WriteRawBytes(Data);
  Result := Self;
end;

function TSBWDataBlockWriter.ToBytes: TBytes;
begin
  SetLength(Result, FStream.Size);
  if FStream.Size > 0 then
  begin
    FStream.Position := 0;
    FStream.ReadBuffer(Result[0], FStream.Size);
  end;
end;

function TSBWDataBlockWriter.Size: Int64;
begin
  Result := FStream.Size;
end;

procedure TSBWDataBlockWriter.Clear;
begin
  FStream.Clear;
end;

{ TSBWDataBlockReader }

constructor TSBWDataBlockReader.Create(const Data: TBytes);
begin
  inherited Create;
  FStream := TMemoryStream.Create;
  FOwnsStream := True;
  if Length(Data) > 0 then
  begin
    FStream.WriteBuffer(Data[0], Length(Data));
    FStream.Position := 0;
  end;
end;

constructor TSBWDataBlockReader.Create(AStream: TMemoryStream; OwnsStream: Boolean);
begin
  inherited Create;
  FStream := AStream;
  FOwnsStream := OwnsStream;
end;

destructor TSBWDataBlockReader.Destroy;
begin
  if FOwnsStream then
    FStream.Free;
  inherited;
end;

function TSBWDataBlockReader.ReadRawByte: Byte;
begin
  if FStream.Position >= FStream.Size then
    raise ESBWReadPastEnd.Create('Attempt to read past end of data block');
  FStream.ReadBuffer(Result, 1);
end;

function TSBWDataBlockReader.ReadRawInt32: Int32;
begin
  if FStream.Position + 4 > FStream.Size then
    raise ESBWReadPastEnd.Create('Attempt to read past end of data block');
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

function TSBWDataBlockReader.ReadRawDouble: Double;
begin
  if FStream.Position + 8 > FStream.Size then
    raise ESBWReadPastEnd.Create('Attempt to read past end of data block');
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

function TSBWDataBlockReader.ReadRawBytes(Count: Integer): TBytes;
begin
  if FStream.Position + Count > FStream.Size then
    raise ESBWReadPastEnd.Create('Attempt to read past end of data block');
  SetLength(Result, Count);
  if Count > 0 then
    FStream.ReadBuffer(Result[0], Count);
end;

procedure TSBWDataBlockReader.ExpectType(Expected: TSBWDataBlockType);
var
  Actual: TSBWDataBlockType;
begin
  Actual := TSBWDataBlockType(ReadRawByte);
  if Actual <> Expected then
    raise ESBWTypeMismatch.CreateFmt('Type mismatch: expected %d, got %d',
      [Ord(Expected), Ord(Actual)]);
end;

function TSBWDataBlockReader.PeekNextType: TSBWDataBlockType;
var
  B: Byte;
begin
  if FStream.Position >= FStream.Size then
    raise ESBWReadPastEnd.Create('No more data to peek');
  FStream.ReadBuffer(B, 1);
  FStream.Position := FStream.Position - 1; // Rewind
  Result := TSBWDataBlockType(B);
end;

function TSBWDataBlockReader.HasMore: Boolean;
begin
  Result := FStream.Position < FStream.Size;
end;

function TSBWDataBlockReader.ReadByte: SBWByte;
begin
  ExpectType(dbtByte);
  Result := ReadRawByte;
end;

function TSBWDataBlockReader.ReadBoolean: SBWBoolean;
begin
  ExpectType(dbtBoolean);
  Result := ReadRawByte <> 0;
end;

function TSBWDataBlockReader.ReadInteger: SBWInteger;
begin
  ExpectType(dbtInteger);
  Result := ReadRawInt32;
end;

function TSBWDataBlockReader.ReadDouble: SBWDouble;
begin
  ExpectType(dbtDouble);
  Result := ReadRawDouble;
end;

function TSBWDataBlockReader.ReadString: string;
var
  Len: Int32;
  UTF8Bytes: TBytes;
begin
  ExpectType(dbtString);
  Len := ReadRawInt32;
  UTF8Bytes := ReadRawBytes(Len);
  ReadRawByte; // Consume null terminator
  Result := TEncoding.UTF8.GetString(UTF8Bytes);
end;

function TSBWDataBlockReader.ReadComplex: TSBWComplex;
begin
  ExpectType(dbtComplex);
  Result.Real := ReadRawDouble;
  Result.Imag := ReadRawDouble;
end;

function TSBWDataBlockReader.ReadIntegerArray: TArray<SBWInteger>;
var
  ElementType: Byte;
  Dims, Size, I: Int32;
begin
  ExpectType(dbtArray);
  ElementType := ReadRawByte;
  if TSBWDataBlockType(ElementType) <> dbtInteger then
    raise ESBWTypeMismatch.Create('Expected integer array');
  Dims := ReadRawInt32;
  if Dims <> 1 then
    raise ESBWTypeMismatch.CreateFmt('Expected 1D array, got %d dimensions', [Dims]);
  Size := ReadRawInt32;
  SetLength(Result, Size);
  for I := 0 to Size - 1 do
    Result[I] := ReadRawInt32;
end;

function TSBWDataBlockReader.ReadDoubleArray: TArray<SBWDouble>;
var
  ElementType: Byte;
  Dims, Size, I: Int32;
begin
  ExpectType(dbtArray);
  ElementType := ReadRawByte;
  if TSBWDataBlockType(ElementType) <> dbtDouble then
    raise ESBWTypeMismatch.Create('Expected double array');
  Dims := ReadRawInt32;
  if Dims <> 1 then
    raise ESBWTypeMismatch.CreateFmt('Expected 1D array, got %d dimensions', [Dims]);
  Size := ReadRawInt32;
  SetLength(Result, Size);
  for I := 0 to Size - 1 do
    Result[I] := ReadRawDouble;
end;

function TSBWDataBlockReader.ReadStringArray: TArray<string>;
var
  ElementType: Byte;
  Dims, Size, I, Len: Int32;
  UTF8Bytes: TBytes;
begin
  ExpectType(dbtArray);
  ElementType := ReadRawByte;
  if TSBWDataBlockType(ElementType) <> dbtString then
    raise ESBWTypeMismatch.Create('Expected string array');
  Dims := ReadRawInt32;
  if Dims <> 1 then
    raise ESBWTypeMismatch.CreateFmt('Expected 1D array, got %d dimensions', [Dims]);
  Size := ReadRawInt32;
  SetLength(Result, Size);
  for I := 0 to Size - 1 do
  begin
    Len := ReadRawInt32;
    UTF8Bytes := ReadRawBytes(Len);
    ReadRawByte; // Null terminator
    Result[I] := TEncoding.UTF8.GetString(UTF8Bytes);
  end;
end;

function TSBWDataBlockReader.ReadByteArray: TArray<SBWByte>;
var
  ElementType: Byte;
  Dims, Size: Int32;
begin
  ExpectType(dbtArray);
  ElementType := ReadRawByte;
  if TSBWDataBlockType(ElementType) <> dbtByte then
    raise ESBWTypeMismatch.Create('Expected byte array');
  Dims := ReadRawInt32;
  if Dims <> 1 then
    raise ESBWTypeMismatch.CreateFmt('Expected 1D array, got %d dimensions', [Dims]);
  Size := ReadRawInt32;
  Result := ReadRawBytes(Size);
end;

function TSBWDataBlockReader.ReadDoubleArray2D: TArray<TArray<SBWDouble>>;
var
  ElementType: Byte;
  Dims, Rows, Cols, I, J: Int32;
begin
  ExpectType(dbtArray);
  ElementType := ReadRawByte;
  if TSBWDataBlockType(ElementType) <> dbtDouble then
    raise ESBWTypeMismatch.Create('Expected double array');
  Dims := ReadRawInt32;
  if Dims <> 2 then
    raise ESBWTypeMismatch.CreateFmt('Expected 2D array, got %d dimensions', [Dims]);
  Rows := ReadRawInt32;
  Cols := ReadRawInt32;
  SetLength(Result, Rows);
  for I := 0 to Rows - 1 do
  begin
    SetLength(Result[I], Cols);
    for J := 0 to Cols - 1 do
      Result[I][J] := ReadRawDouble;
  end;
end;

function TSBWDataBlockReader.ReadIntegerArray2D: TArray<TArray<SBWInteger>>;
var
  ElementType: Byte;
  Dims, Rows, Cols, I, J: Int32;
begin
  ExpectType(dbtArray);
  ElementType := ReadRawByte;
  if TSBWDataBlockType(ElementType) <> dbtInteger then
    raise ESBWTypeMismatch.Create('Expected integer array');
  Dims := ReadRawInt32;
  if Dims <> 2 then
    raise ESBWTypeMismatch.CreateFmt('Expected 2D array, got %d dimensions', [Dims]);
  Rows := ReadRawInt32;
  Cols := ReadRawInt32;
  SetLength(Result, Rows);
  for I := 0 to Rows - 1 do
  begin
    SetLength(Result[I], Cols);
    for J := 0 to Cols - 1 do
      Result[I][J] := ReadRawInt32;
  end;
end;

function TSBWDataBlockReader.ReadListBegin: SBWInteger;
begin
  ExpectType(dbtList);
  Result := ReadRawInt32;
end;

function TSBWDataBlockReader.Remaining: Int64;
begin
  Result := FStream.Size - FStream.Position;
end;

function TSBWDataBlockReader.Position: Int64;
begin
  Result := FStream.Position;
end;

procedure TSBWDataBlockReader.Skip;
var
  TypeByte, ElementType: Byte;
  DataType: TSBWDataBlockType;
  Len, Dims, I, TotalElements, ItemCount: Int32;
  Sizes: TArray<Int32>;
begin
  TypeByte := ReadRawByte;
  DataType := TSBWDataBlockType(TypeByte);

  case DataType of
    dbtByte, dbtBoolean:
      ReadRawByte;

    dbtInteger:
      ReadRawInt32;

    dbtDouble:
      ReadRawDouble;

    dbtComplex:
      begin
        ReadRawDouble;
        ReadRawDouble;
      end;

    dbtString:
      begin
        Len := ReadRawInt32;
        ReadRawBytes(Len);
        ReadRawByte; // null terminator
      end;

    dbtArray:
      begin
        ElementType := ReadRawByte;
        Dims := ReadRawInt32;
        SetLength(Sizes, Dims);
        TotalElements := 1;
        for I := 0 to Dims - 1 do
        begin
          Sizes[I] := ReadRawInt32;
          TotalElements := TotalElements * Sizes[I];
        end;

        // Skip element data
        case TSBWDataBlockType(ElementType) of
          dbtByte, dbtBoolean:
            ReadRawBytes(TotalElements);
          dbtInteger:
            ReadRawBytes(TotalElements * 4);
          dbtDouble:
            ReadRawBytes(TotalElements * 8);
          dbtComplex:
            ReadRawBytes(TotalElements * 16);
          dbtString:
            begin
              // Strings are variable length, must read each
              for I := 0 to TotalElements - 1 do
              begin
                Len := ReadRawInt32;
                ReadRawBytes(Len);
                ReadRawByte;
              end;
            end;
        else
          raise ESBWDataBlockError.Create('Unsupported array element type');
        end;
      end;

    dbtList:
      begin
        ItemCount := ReadRawInt32;
        for I := 0 to ItemCount - 1 do
          Skip; // Recursively skip each item
      end;

    dbtVoid, dbtTerminate:
      ; // No data to skip

  else
    raise ESBWDataBlockError.CreateFmt('Unknown type byte: %d', [TypeByte]);
  end;
end;

end.

