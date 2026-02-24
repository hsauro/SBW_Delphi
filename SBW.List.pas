unit SBW.List;

{******************************************************************************
 * SBW.List.pas
 *
 * Developer representation of the SBW List data type.
 *
 * TSBWList is a heterogeneous container that can hold values of any SBW type.
 * Each item is a TSBWListItem which knows its own type and can be queried
 * for the appropriate value.
 *
 * Lists can be nested (a list item can itself be a list) and can contain
 * arrays, making them suitable for returning complex structured data from
 * SBW method calls.
 *
 * Wire format:
 *   Type byte (dbtList = 6)
 *   Item count (4 bytes, signed int32)
 *   Items (each with its own type prefix)
 *****************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  SBW.Types, SBW.DataBlock;

type
  TSBWList = class;

  /// <summary>
  /// A single item in an SBW list. Can hold any SBW primitive type,
  /// a nested list, or an array.
  /// </summary>
  TSBWListItem = class
  private
    FDataType: TSBWDataBlockType;
    FByteValue: Byte;
    FBoolValue: Boolean;
    FIntValue: SBWInteger;
    FDoubleValue: SBWDouble;
    FStringValue: string;
    FComplexValue: TSBWComplex;
    FListValue: TSBWList;
    FIntArrayValue: TArray<SBWInteger>;
    FDoubleArrayValue: TArray<SBWDouble>;
    FStringArrayValue: TArray<string>;
    FByteArrayValue: TArray<Byte>;
    FOwnsListValue: Boolean;
  public
    constructor Create; overload;
    constructor CreateByte(Value: Byte);
    constructor CreateBoolean(Value: Boolean);
    constructor CreateInteger(Value: SBWInteger);
    constructor CreateDouble(Value: SBWDouble);
    constructor CreateString(const Value: string);
    constructor CreateComplex(const Value: TSBWComplex);
    constructor CreateList(Value: TSBWList; OwnsValue: Boolean = True);
    constructor CreateIntegerArray(const Value: TArray<SBWInteger>);
    constructor CreateDoubleArray(const Value: TArray<SBWDouble>);
    constructor CreateStringArray(const Value: TArray<string>);
    constructor CreateByteArray(const Value: TArray<Byte>);
    destructor Destroy; override;

    /// <summary>
    /// Get value as byte. Raises exception if type mismatch.
    /// </summary>
    function GetByte: Byte;

    /// <summary>
    /// Get value as boolean. Raises exception if type mismatch.
    /// </summary>
    function GetBoolean: Boolean;

    /// <summary>
    /// Get value as integer. Raises exception if type mismatch.
    /// </summary>
    function GetInteger: SBWInteger;

    /// <summary>
    /// Get value as double. Raises exception if type mismatch.
    /// </summary>
    function GetDouble: SBWDouble;

    /// <summary>
    /// Get value as scalar (int or double). Returns as double.
    /// Raises exception if neither int nor double.
    /// </summary>
    function GetScalar: SBWDouble;

    /// <summary>
    /// Get value as string. Raises exception if type mismatch.
    /// </summary>
    function GetString: string;

    /// <summary>
    /// Get value as complex. Raises exception if type mismatch.
    /// </summary>
    function GetComplex: TSBWComplex;

    /// <summary>
    /// Get value as nested list. Raises exception if type mismatch.
    /// </summary>
    function GetList: TSBWList;

    /// <summary>
    /// Get value as integer array. Raises exception if type mismatch.
    /// </summary>
    function GetIntegerArray: TArray<SBWInteger>;

    /// <summary>
    /// Get value as double array. Raises exception if type mismatch.
    /// </summary>
    function GetDoubleArray: TArray<SBWDouble>;

    /// <summary>
    /// Get value as string array. Raises exception if type mismatch.
    /// </summary>
    function GetStringArray: TArray<string>;

    /// <summary>
    /// Get value as byte array. Raises exception if type mismatch.
    /// </summary>
    function GetByteArray: TArray<Byte>;

    /// <summary>
    /// Convert value to string representation (for display/debugging)
    /// </summary>
    function AsString: string;

    /// <summary>
    /// The SBW data type of this item
    /// </summary>
    property DataType: TSBWDataBlockType read FDataType;
  end;

  /// <summary>
  /// A heterogeneous list of SBW values.
  /// </summary>
  TSBWList = class
  private
    FItems: TObjectList<TSBWListItem>;
    function GetItem(Index: Integer): TSBWListItem;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Add an item to the list
    /// </summary>
    function Add(Item: TSBWListItem): Integer;

    /// <summary>
    /// Add a byte value
    /// </summary>
    function AddByte(Value: Byte): Integer;

    /// <summary>
    /// Add a boolean value
    /// </summary>
    function AddBoolean(Value: Boolean): Integer;

    /// <summary>
    /// Add an integer value
    /// </summary>
    function AddInteger(Value: SBWInteger): Integer;

    /// <summary>
    /// Add a double value
    /// </summary>
    function AddDouble(Value: SBWDouble): Integer;

    /// <summary>
    /// Add a string value
    /// </summary>
    function AddString(const Value: string): Integer;

    /// <summary>
    /// Add a complex value
    /// </summary>
    function AddComplex(const Value: TSBWComplex): Integer;

    /// <summary>
    /// Add a nested list
    /// </summary>
    function AddList(Value: TSBWList; OwnsValue: Boolean = True): Integer;

    /// <summary>
    /// Add an integer array
    /// </summary>
    function AddIntegerArray(const Value: TArray<SBWInteger>): Integer;

    /// <summary>
    /// Add a double array
    /// </summary>
    function AddDoubleArray(const Value: TArray<SBWDouble>): Integer;

    /// <summary>
    /// Add a string array
    /// </summary>
    function AddStringArray(const Value: TArray<string>): Integer;

    /// <summary>
    /// Delete an item by index
    /// </summary>
    procedure Delete(Index: Integer);

    /// <summary>
    /// Clear all items
    /// </summary>
    procedure Clear;

    /// <summary>
    /// Write this list to a DataBlockWriter
    /// </summary>
    procedure WriteTo(Writer: TSBWDataBlockWriter);

    /// <summary>
    /// Read a list from a DataBlockReader
    /// </summary>
    class function ReadFrom(Reader: TSBWDataBlockReader): TSBWList;

    /// <summary>
    /// Number of items in the list
    /// </summary>
    property Count: Integer read GetCount;

    /// <summary>
    /// Access items by index
    /// </summary>
    property Items[Index: Integer]: TSBWListItem read GetItem; default;
  end;

implementation

{ TSBWListItem }

constructor TSBWListItem.Create;
begin
  inherited Create;
  FDataType := dbtVoid;
  FOwnsListValue := False;
end;

constructor TSBWListItem.CreateByte(Value: Byte);
begin
  inherited Create;
  FDataType := dbtByte;
  FByteValue := Value;
  FOwnsListValue := False;
end;

constructor TSBWListItem.CreateBoolean(Value: Boolean);
begin
  inherited Create;
  FDataType := dbtBoolean;
  FBoolValue := Value;
  FOwnsListValue := False;
end;

constructor TSBWListItem.CreateInteger(Value: SBWInteger);
begin
  inherited Create;
  FDataType := dbtInteger;
  FIntValue := Value;
  FOwnsListValue := False;
end;

constructor TSBWListItem.CreateDouble(Value: SBWDouble);
begin
  inherited Create;
  FDataType := dbtDouble;
  FDoubleValue := Value;
  FOwnsListValue := False;
end;

constructor TSBWListItem.CreateString(const Value: string);
begin
  inherited Create;
  FDataType := dbtString;
  FStringValue := Value;
  FOwnsListValue := False;
end;

constructor TSBWListItem.CreateComplex(const Value: TSBWComplex);
begin
  inherited Create;
  FDataType := dbtComplex;
  FComplexValue := Value;
  FOwnsListValue := False;
end;

constructor TSBWListItem.CreateList(Value: TSBWList; OwnsValue: Boolean);
begin
  inherited Create;
  FDataType := dbtList;
  FListValue := Value;
  FOwnsListValue := OwnsValue;
end;

constructor TSBWListItem.CreateIntegerArray(const Value: TArray<SBWInteger>);
begin
  inherited Create;
  FDataType := dbtArray;
  FIntArrayValue := Value;
  FOwnsListValue := False;
end;

constructor TSBWListItem.CreateDoubleArray(const Value: TArray<SBWDouble>);
begin
  inherited Create;
  FDataType := dbtArray;
  FDoubleArrayValue := Value;
  FOwnsListValue := False;
end;

constructor TSBWListItem.CreateStringArray(const Value: TArray<string>);
begin
  inherited Create;
  FDataType := dbtArray;
  FStringArrayValue := Value;
  FOwnsListValue := False;
end;

constructor TSBWListItem.CreateByteArray(const Value: TArray<Byte>);
begin
  inherited Create;
  FDataType := dbtArray;
  FByteArrayValue := Value;
  FOwnsListValue := False;
end;

destructor TSBWListItem.Destroy;
begin
  if FOwnsListValue and (FListValue <> nil) then
    FListValue.Free;
  inherited;
end;

function TSBWListItem.GetByte: Byte;
begin
  if FDataType <> dbtByte then
    raise ESBWDataBlockError.Create('Byte expected in list item');
  Result := FByteValue;
end;

function TSBWListItem.GetBoolean: Boolean;
begin
  if FDataType <> dbtBoolean then
    raise ESBWDataBlockError.Create('Boolean expected in list item');
  Result := FBoolValue;
end;

function TSBWListItem.GetInteger: SBWInteger;
begin
  if FDataType <> dbtInteger then
    raise ESBWDataBlockError.Create('Integer expected in list item');
  Result := FIntValue;
end;

function TSBWListItem.GetDouble: SBWDouble;
begin
  if FDataType <> dbtDouble then
    raise ESBWDataBlockError.Create('Double expected in list item');
  Result := FDoubleValue;
end;

function TSBWListItem.GetScalar: SBWDouble;
begin
  case FDataType of
    dbtInteger: Result := FIntValue;
    dbtDouble: Result := FDoubleValue;
  else
    raise ESBWDataBlockError.Create('Integer or Double expected in list item');
  end;
end;

function TSBWListItem.GetString: string;
begin
  if FDataType <> dbtString then
    raise ESBWDataBlockError.Create('String expected in list item');
  Result := FStringValue;
end;

function TSBWListItem.GetComplex: TSBWComplex;
begin
  if FDataType <> dbtComplex then
    raise ESBWDataBlockError.Create('Complex expected in list item');
  Result := FComplexValue;
end;

function TSBWListItem.GetList: TSBWList;
begin
  if FDataType <> dbtList then
    raise ESBWDataBlockError.Create('List expected in list item');
  Result := FListValue;
end;

function TSBWListItem.GetIntegerArray: TArray<SBWInteger>;
begin
  if FDataType <> dbtArray then
    raise ESBWDataBlockError.Create('Array expected in list item');
  Result := FIntArrayValue;
end;

function TSBWListItem.GetDoubleArray: TArray<SBWDouble>;
begin
  if FDataType <> dbtArray then
    raise ESBWDataBlockError.Create('Array expected in list item');
  Result := FDoubleArrayValue;
end;

function TSBWListItem.GetStringArray: TArray<string>;
begin
  if FDataType <> dbtArray then
    raise ESBWDataBlockError.Create('Array expected in list item');
  Result := FStringArrayValue;
end;

function TSBWListItem.GetByteArray: TArray<Byte>;
begin
  if FDataType <> dbtArray then
    raise ESBWDataBlockError.Create('Array expected in list item');
  Result := FByteArrayValue;
end;

function TSBWListItem.AsString: string;
var
  I: Integer;
begin
  case FDataType of
    dbtByte:
      Result := IntToStr(FByteValue);
    dbtBoolean:
      if FBoolValue then Result := 'True' else Result := 'False';
    dbtInteger:
      Result := IntToStr(FIntValue);
    dbtDouble:
      Result := FloatToStr(FDoubleValue);
    dbtString:
      Result := '"' + FStringValue + '"';
    dbtComplex:
      Result := Format('(%g + %gi)', [FComplexValue.Real, FComplexValue.Imag]);
    dbtList:
      begin
        Result := '[';
        if FListValue <> nil then
          for I := 0 to FListValue.Count - 1 do
          begin
            if I > 0 then Result := Result + ', ';
            Result := Result + FListValue[I].AsString;
          end;
        Result := Result + ']';
      end;
    dbtArray:
      Result := '<array>';
    dbtVoid:
      Result := '<void>';
  else
    Result := '<unknown>';
  end;
end;

{ TSBWList }

constructor TSBWList.Create;
begin
  inherited Create;
  FItems := TObjectList<TSBWListItem>.Create(True);
end;

destructor TSBWList.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TSBWList.GetItem(Index: Integer): TSBWListItem;
begin
  Result := FItems[Index];
end;

function TSBWList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TSBWList.Add(Item: TSBWListItem): Integer;
begin
  Result := FItems.Add(Item);
end;

function TSBWList.AddByte(Value: Byte): Integer;
begin
  Result := FItems.Add(TSBWListItem.CreateByte(Value));
end;

function TSBWList.AddBoolean(Value: Boolean): Integer;
begin
  Result := FItems.Add(TSBWListItem.CreateBoolean(Value));
end;

function TSBWList.AddInteger(Value: SBWInteger): Integer;
begin
  Result := FItems.Add(TSBWListItem.CreateInteger(Value));
end;

function TSBWList.AddDouble(Value: SBWDouble): Integer;
begin
  Result := FItems.Add(TSBWListItem.CreateDouble(Value));
end;

function TSBWList.AddString(const Value: string): Integer;
begin
  Result := FItems.Add(TSBWListItem.CreateString(Value));
end;

function TSBWList.AddComplex(const Value: TSBWComplex): Integer;
begin
  Result := FItems.Add(TSBWListItem.CreateComplex(Value));
end;

function TSBWList.AddList(Value: TSBWList; OwnsValue: Boolean): Integer;
begin
  Result := FItems.Add(TSBWListItem.CreateList(Value, OwnsValue));
end;

function TSBWList.AddIntegerArray(const Value: TArray<SBWInteger>): Integer;
begin
  Result := FItems.Add(TSBWListItem.CreateIntegerArray(Value));
end;

function TSBWList.AddDoubleArray(const Value: TArray<SBWDouble>): Integer;
begin
  Result := FItems.Add(TSBWListItem.CreateDoubleArray(Value));
end;

function TSBWList.AddStringArray(const Value: TArray<string>): Integer;
begin
  Result := FItems.Add(TSBWListItem.CreateStringArray(Value));
end;

procedure TSBWList.Delete(Index: Integer);
begin
  FItems.Delete(Index);
end;

procedure TSBWList.Clear;
begin
  FItems.Clear;
end;

procedure TSBWList.WriteTo(Writer: TSBWDataBlockWriter);
var
  I: Integer;
  Item: TSBWListItem;
begin
  Writer.WriteListBegin(FItems.Count);
  
  for I := 0 to FItems.Count - 1 do
  begin
    Item := FItems[I];
    case Item.DataType of
      dbtByte:
        Writer.WriteByte(Item.FByteValue);
      dbtBoolean:
        Writer.WriteBoolean(Item.FBoolValue);
      dbtInteger:
        Writer.WriteInteger(Item.FIntValue);
      dbtDouble:
        Writer.WriteDouble(Item.FDoubleValue);
      dbtString:
        Writer.WriteString(Item.FStringValue);
      dbtComplex:
        Writer.WriteComplex(Item.FComplexValue);
      dbtList:
        Item.FListValue.WriteTo(Writer);
      dbtArray:
        begin
          // Determine array element type by which array field is populated
          if Length(Item.FIntArrayValue) > 0 then
            Writer.WriteIntegerArray(Item.FIntArrayValue)
          else if Length(Item.FDoubleArrayValue) > 0 then
            Writer.WriteDoubleArray(Item.FDoubleArrayValue)
          else if Length(Item.FStringArrayValue) > 0 then
            Writer.WriteStringArray(Item.FStringArrayValue)
          else if Length(Item.FByteArrayValue) > 0 then
            Writer.WriteByteArray(Item.FByteArrayValue)
          else
            Writer.WriteIntegerArray([]); // Empty array
        end;
    end;
  end;
end;

class function TSBWList.ReadFrom(Reader: TSBWDataBlockReader): TSBWList;
var
  ItemCount: SBWInteger;
  I: Integer;
  ItemType: TSBWDataBlockType;
  NestedList: TSBWList;
begin
  Result := TSBWList.Create;
  try
    ItemCount := Reader.ReadListBegin;
    
    for I := 0 to ItemCount - 1 do
    begin
      ItemType := Reader.PeekNextType;
      
      case ItemType of
        dbtByte:
          Result.AddByte(Reader.ReadByte);
        dbtBoolean:
          Result.AddBoolean(Reader.ReadBoolean);
        dbtInteger:
          Result.AddInteger(Reader.ReadInteger);
        dbtDouble:
          Result.AddDouble(Reader.ReadDouble);
        dbtString:
          Result.AddString(Reader.ReadString);
        dbtComplex:
          Result.AddComplex(Reader.ReadComplex);
        dbtList:
          begin
            NestedList := TSBWList.ReadFrom(Reader);
            Result.AddList(NestedList, True);
          end;
        dbtArray:
          begin
            // For now, try to read as integer array
            // TODO: Properly detect array element type
            Result.AddIntegerArray(Reader.ReadIntegerArray);
          end;
      else
        raise ESBWDataBlockError.CreateFmt('Unexpected type in list: %d', [Ord(ItemType)]);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
