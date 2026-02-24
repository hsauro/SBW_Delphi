unit SBW_Array;

{******************************************************************************
 * SBW.Array.pas
 *
 * Developer representation of the SBW Array data type.
 *
 * TSBWArray is a homogeneous container that holds values of a single SBW type.
 * Unlike TSBWList (which is heterogeneous and can mix types), arrays enforce
 * type uniformity and support multi-dimensional indexing.
 *
 * Arrays are suitable for:
 *   - Matrices and vectors of numeric data
 *   - Time series and simulation results
 *   - Stoichiometry matrices (integer 2D arrays)
 *   - Parameter vectors (double 1D arrays)
 *   - Lists of species/reaction names (string 1D arrays)
 *
 * Wire format:
 *   Type byte (dbtArray = 5)
 *   Element type byte (e.g., dbtDouble = 2)
 *   Dimension count (4 bytes, signed int32)
 *   Dimension sizes (4 bytes each)
 *   Element data (row-major order for multi-dimensional)
 *
 * Design notes:
 *   - The element type is fixed at creation and cannot be changed
 *   - Multi-dimensional arrays are stored in row-major order (C-style)
 *   - Dimensions are indexed from 0
 *   - For 2D arrays: [row, col] where row is the first dimension
 *
 * Creation:

 * TSBWArray.Create(dbtDouble, 10) — 1D array
 * TSBWArray.Create(dbtInteger, [3, 4]) — 2D matrix
 * SBWDoubleArray([1.0, 2.0, 3.0]) — factory function
 * SBWDoubleMatrix([[1,2],[3,4]]) — 2D from nested arrays
 * SBWIdentityMatrix(3) / SBWZeroMatrix(2, 3) — common matrices

 * Access:

 * 1D: GetDouble(i), SetDouble(i, val)
 * 2D: GetDouble2D(row, col), SetDouble2D(row, col, val)
 * N-D: GetDoubleAt([i,j,k]), SetDoubleAt([i,j,k], val)
 * Bulk: AsDoubleArray, AsDoubleArray2D, SetFromDoubleArray2D

 * Serialization:

 * WriteTo(Writer) / TSBWArray.ReadFrom(Reader) — compatible with existing wire format
 *****************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  SBW.Types, SBW.DataBlock;

type
  ESBWArrayError = class(Exception);

  /// <summary>
  /// Forward declaration
  /// </summary>
  TSBWArray = class;

  /// <summary>
  /// A homogeneous multi-dimensional array of SBW values.
  /// All elements share the same type, enabling efficient storage and access.
  /// </summary>
  TSBWArray = class
  private
    FElementType: TSBWDataBlockType;
    FDimensions: TArray<Integer>;
    FTotalElements: Integer;
    
    // Storage - only one of these is used based on FElementType
    FByteData: TArray<Byte>;
    FBooleanData: TArray<Boolean>;
    FIntegerData: TArray<SBWInteger>;
    FDoubleData: TArray<SBWDouble>;
    FStringData: TArray<string>;
    FComplexData: TArray<TSBWComplex>;
    
    function GetDimensionCount: Integer;
    function GetDimension(Index: Integer): Integer;
    function CalculateFlatIndex(const Indices: array of Integer): Integer;
    procedure ValidateIndices(const Indices: array of Integer);
    procedure AllocateStorage;
  public
    /// <summary>
    /// Create an empty array (must call Reshape before use)
    /// </summary>
    constructor Create(AElementType: TSBWDataBlockType); overload;
    
    /// <summary>
    /// Create a 1D array with specified size
    /// </summary>
    constructor Create(AElementType: TSBWDataBlockType; Size: Integer); overload;
    
    /// <summary>
    /// Create a multi-dimensional array with specified sizes
    /// </summary>
    constructor Create(AElementType: TSBWDataBlockType; 
      const DimensionSizes: array of Integer); overload;
    
    destructor Destroy; override;
    
    // =========================================================================
    // Shape manipulation
    // =========================================================================
    
    /// <summary>
    /// Reshape the array to new dimensions. Total element count may change.
    /// Existing data is preserved where indices overlap, new elements are zeroed.
    /// </summary>
    procedure Reshape(const NewDimensions: array of Integer);
    
    /// <summary>
    /// Get the size of a specific dimension
    /// </summary>
    function Size(Dimension: Integer = 0): Integer;
    
    /// <summary>
    /// Get all dimension sizes as an array
    /// </summary>
    function Shape: TArray<Integer>;
    
    // =========================================================================
    // 1D array access (for convenience)
    // =========================================================================
    
    function GetByte(Index: Integer): Byte;
    procedure SetByte(Index: Integer; Value: Byte);
    
    function GetBoolean(Index: Integer): Boolean;
    procedure SetBoolean(Index: Integer; Value: Boolean);
    
    function GetInteger(Index: Integer): SBWInteger;
    procedure SetInteger(Index: Integer; Value: SBWInteger);
    
    function GetDouble(Index: Integer): SBWDouble;
    procedure SetDouble(Index: Integer; Value: SBWDouble);
    
    function GetString(Index: Integer): string;
    procedure SetString(Index: Integer; const Value: string);
    
    function GetComplex(Index: Integer): TSBWComplex;
    procedure SetComplex(Index: Integer; const Value: TSBWComplex);
    
    // =========================================================================
    // Multi-dimensional access
    // =========================================================================
    
    function GetByteAt(const Indices: array of Integer): Byte;
    procedure SetByteAt(const Indices: array of Integer; Value: Byte);
    
    function GetBooleanAt(const Indices: array of Integer): Boolean;
    procedure SetBooleanAt(const Indices: array of Integer; Value: Boolean);
    
    function GetIntegerAt(const Indices: array of Integer): SBWInteger;
    procedure SetIntegerAt(const Indices: array of Integer; Value: SBWInteger);
    
    function GetDoubleAt(const Indices: array of Integer): SBWDouble;
    procedure SetDoubleAt(const Indices: array of Integer; Value: SBWDouble);
    
    function GetStringAt(const Indices: array of Integer): string;
    procedure SetStringAt(const Indices: array of Integer; const Value: string);
    
    function GetComplexAt(const Indices: array of Integer): TSBWComplex;
    procedure SetComplexAt(const Indices: array of Integer; const Value: TSBWComplex);
    
    // =========================================================================
    // 2D convenience methods (common for matrices)
    // =========================================================================
    
    function GetDouble2D(Row, Col: Integer): SBWDouble;
    procedure SetDouble2D(Row, Col: Integer; Value: SBWDouble);
    
    function GetInteger2D(Row, Col: Integer): SBWInteger;
    procedure SetInteger2D(Row, Col: Integer; Value: SBWInteger);
    
    function Rows: Integer;
    function Cols: Integer;
    
    // =========================================================================
    // Bulk data access (for efficiency)
    // =========================================================================
    
    /// <summary>
    /// Get the underlying byte data (for byte arrays)
    /// </summary>
    function AsByteArray: TArray<Byte>;
    
    /// <summary>
    /// Get the underlying boolean data (for boolean arrays)
    /// </summary>
    function AsBooleanArray: TArray<Boolean>;
    
    /// <summary>
    /// Get the underlying integer data (for integer arrays)
    /// </summary>
    function AsIntegerArray: TArray<SBWInteger>;
    
    /// <summary>
    /// Get the underlying double data (for double arrays)
    /// </summary>
    function AsDoubleArray: TArray<SBWDouble>;
    
    /// <summary>
    /// Get the underlying string data (for string arrays)
    /// </summary>
    function AsStringArray: TArray<string>;
    
    /// <summary>
    /// Get the underlying complex data (for complex arrays)
    /// </summary>
    function AsComplexArray: TArray<TSBWComplex>;
    
    /// <summary>
    /// Get as 2D double array (for 2D double arrays)
    /// </summary>
    function AsDoubleArray2D: TArray<TArray<SBWDouble>>;
    
    /// <summary>
    /// Get as 2D integer array (for 2D integer arrays)
    /// </summary>
    function AsIntegerArray2D: TArray<TArray<SBWInteger>>;
    
    // =========================================================================
    // Bulk data assignment
    // =========================================================================
    
    procedure SetFromByteArray(const Data: TArray<Byte>);
    procedure SetFromBooleanArray(const Data: TArray<Boolean>);
    procedure SetFromIntegerArray(const Data: TArray<SBWInteger>);
    procedure SetFromDoubleArray(const Data: TArray<SBWDouble>);
    procedure SetFromStringArray(const Data: TArray<string>);
    procedure SetFromComplexArray(const Data: TArray<TSBWComplex>);
    
    procedure SetFromDoubleArray2D(const Data: TArray<TArray<SBWDouble>>);
    procedure SetFromIntegerArray2D(const Data: TArray<TArray<SBWInteger>>);
    
    // =========================================================================
    // Serialization
    // =========================================================================
    
    /// <summary>
    /// Write this array to a DataBlockWriter
    /// </summary>
    procedure WriteTo(Writer: TSBWDataBlockWriter);
    
    /// <summary>
    /// Read an array from a DataBlockReader
    /// </summary>
    class function ReadFrom(Reader: TSBWDataBlockReader): TSBWArray;
    
    // =========================================================================
    // Utility
    // =========================================================================
    
    /// <summary>
    /// Create a string representation for debugging
    /// </summary>
    function AsString: string;
    
    /// <summary>
    /// Create a deep copy of this array
    /// </summary>
    function Clone: TSBWArray;
    
    /// <summary>
    /// Fill all elements with a default value (0, false, '', etc.)
    /// </summary>
    procedure Clear;
    
    /// <summary>
    /// Fill all elements with a specified value
    /// </summary>
    procedure Fill(Value: SBWDouble); overload;
    procedure Fill(Value: SBWInteger); overload;
    procedure Fill(const Value: string); overload;
    
    // =========================================================================
    // Properties
    // =========================================================================
    
    /// <summary>
    /// The element type of all values in this array
    /// </summary>
    property ElementType: TSBWDataBlockType read FElementType;
    
    /// <summary>
    /// Number of dimensions (1 for vector, 2 for matrix, etc.)
    /// </summary>
    property DimensionCount: Integer read GetDimensionCount;
    
    /// <summary>
    /// Size of each dimension
    /// </summary>
    property Dimensions[Index: Integer]: Integer read GetDimension;
    
    /// <summary>
    /// Total number of elements across all dimensions
    /// </summary>
    property TotalElements: Integer read FTotalElements;
  end;

  // ===========================================================================
  // Factory functions for common array types
  // ===========================================================================
  
  /// <summary>
  /// Create a 1D double array from values
  /// </summary>
  function SBWDoubleArray(const Values: TArray<SBWDouble>): TSBWArray;
  
  /// <summary>
  /// Create a 1D integer array from values
  /// </summary>
  function SBWIntegerArray(const Values: TArray<SBWInteger>): TSBWArray;
  
  /// <summary>
  /// Create a 1D string array from values
  /// </summary>
  function SBWStringArray(const Values: TArray<string>): TSBWArray;
  
  /// <summary>
  /// Create a 2D double matrix from values
  /// </summary>
  function SBWDoubleMatrix(const Values: TArray<TArray<SBWDouble>>): TSBWArray;
  
  /// <summary>
  /// Create a 2D integer matrix from values
  /// </summary>
  function SBWIntegerMatrix(const Values: TArray<TArray<SBWInteger>>): TSBWArray;
  
  /// <summary>
  /// Create a zero-filled double matrix of given size
  /// </summary>
  function SBWZeroMatrix(ARows, ACols: Integer): TSBWArray;
  
  /// <summary>
  /// Create an identity matrix of given size
  /// </summary>
  function SBWIdentityMatrix(Size: Integer): TSBWArray;

implementation

{ TSBWArray }

constructor TSBWArray.Create(AElementType: TSBWDataBlockType);
begin
  inherited Create;
  
  // Validate element type
  if not (AElementType in [dbtByte, dbtBoolean, dbtInteger, dbtDouble, 
                           dbtString, dbtComplex]) then
    raise ESBWArrayError.CreateFmt('Invalid array element type: %d', 
      [Ord(AElementType)]);
  
  FElementType := AElementType;
  SetLength(FDimensions, 0);
  FTotalElements := 0;
end;

constructor TSBWArray.Create(AElementType: TSBWDataBlockType; Size: Integer);
begin
  Create(AElementType);
  Reshape([Size]);
end;

constructor TSBWArray.Create(AElementType: TSBWDataBlockType;
  const DimensionSizes: array of Integer);
begin
  Create(AElementType);
  Reshape(DimensionSizes);
end;

destructor TSBWArray.Destroy;
begin
  // Dynamic arrays are automatically freed
  inherited;
end;

function TSBWArray.GetDimensionCount: Integer;
begin
  Result := Length(FDimensions);
end;

function TSBWArray.GetDimension(Index: Integer): Integer;
begin
  if (Index < 0) or (Index >= Length(FDimensions)) then
    raise ESBWArrayError.CreateFmt('Dimension index %d out of range [0..%d]',
      [Index, Length(FDimensions) - 1]);
  Result := FDimensions[Index];
end;

function TSBWArray.CalculateFlatIndex(const Indices: array of Integer): Integer;
var
  I, Multiplier: Integer;
begin
  ValidateIndices(Indices);
  
  Result := 0;
  Multiplier := 1;
  
  // Row-major order: last index varies fastest
  for I := High(Indices) downto 0 do
  begin
    Result := Result + Indices[I] * Multiplier;
    Multiplier := Multiplier * FDimensions[I];
  end;
end;

procedure TSBWArray.ValidateIndices(const Indices: array of Integer);
var
  I: Integer;
begin
  if Length(Indices) <> Length(FDimensions) then
    raise ESBWArrayError.CreateFmt(
      'Index count mismatch: got %d indices, array has %d dimensions',
      [Length(Indices), Length(FDimensions)]);
  
  for I := 0 to High(Indices) do
  begin
    if (Indices[I] < 0) or (Indices[I] >= FDimensions[I]) then
      raise ESBWArrayError.CreateFmt(
        'Index %d out of range [0..%d] for dimension %d',
        [Indices[I], FDimensions[I] - 1, I]);
  end;
end;

procedure TSBWArray.AllocateStorage;
begin
  case FElementType of
    dbtByte:    SetLength(FByteData, FTotalElements);
    dbtBoolean: SetLength(FBooleanData, FTotalElements);
    dbtInteger: SetLength(FIntegerData, FTotalElements);
    dbtDouble:  SetLength(FDoubleData, FTotalElements);
    dbtString:  SetLength(FStringData, FTotalElements);
    dbtComplex: SetLength(FComplexData, FTotalElements);
  end;
end;

procedure TSBWArray.Reshape(const NewDimensions: array of Integer);
var
  I, NewTotal: Integer;
begin
  // Validate dimensions
  if Length(NewDimensions) = 0 then
    raise ESBWArrayError.Create('Array must have at least one dimension');
  
  NewTotal := 1;
  for I := 0 to High(NewDimensions) do
  begin
    if NewDimensions[I] < 0 then
      raise ESBWArrayError.CreateFmt('Invalid dimension size: %d', 
        [NewDimensions[I]]);
    NewTotal := NewTotal * NewDimensions[I];
  end;
  
  // Copy dimension sizes
  SetLength(FDimensions, Length(NewDimensions));
  for I := 0 to High(NewDimensions) do
    FDimensions[I] := NewDimensions[I];
  
  FTotalElements := NewTotal;
  AllocateStorage;
end;

function TSBWArray.Size(Dimension: Integer): Integer;
begin
  Result := GetDimension(Dimension);
end;

function TSBWArray.Shape: TArray<Integer>;
begin
  Result := Copy(FDimensions);
end;

// =============================================================================
// 1D Access
// =============================================================================

function TSBWArray.GetByte(Index: Integer): Byte;
begin
  if FElementType <> dbtByte then
    raise ESBWArrayError.Create('Array element type is not Byte');
  if (Index < 0) or (Index >= FTotalElements) then
    raise ESBWArrayError.CreateFmt('Index %d out of range [0..%d]', 
      [Index, FTotalElements - 1]);
  Result := FByteData[Index];
end;

procedure TSBWArray.SetByte(Index: Integer; Value: Byte);
begin
  if FElementType <> dbtByte then
    raise ESBWArrayError.Create('Array element type is not Byte');
  if (Index < 0) or (Index >= FTotalElements) then
    raise ESBWArrayError.CreateFmt('Index %d out of range [0..%d]', 
      [Index, FTotalElements - 1]);
  FByteData[Index] := Value;
end;

function TSBWArray.GetBoolean(Index: Integer): Boolean;
begin
  if FElementType <> dbtBoolean then
    raise ESBWArrayError.Create('Array element type is not Boolean');
  if (Index < 0) or (Index >= FTotalElements) then
    raise ESBWArrayError.CreateFmt('Index %d out of range [0..%d]', 
      [Index, FTotalElements - 1]);
  Result := FBooleanData[Index];
end;

procedure TSBWArray.SetBoolean(Index: Integer; Value: Boolean);
begin
  if FElementType <> dbtBoolean then
    raise ESBWArrayError.Create('Array element type is not Boolean');
  if (Index < 0) or (Index >= FTotalElements) then
    raise ESBWArrayError.CreateFmt('Index %d out of range [0..%d]', 
      [Index, FTotalElements - 1]);
  FBooleanData[Index] := Value;
end;

function TSBWArray.GetInteger(Index: Integer): SBWInteger;
begin
  if FElementType <> dbtInteger then
    raise ESBWArrayError.Create('Array element type is not Integer');
  if (Index < 0) or (Index >= FTotalElements) then
    raise ESBWArrayError.CreateFmt('Index %d out of range [0..%d]', 
      [Index, FTotalElements - 1]);
  Result := FIntegerData[Index];
end;

procedure TSBWArray.SetInteger(Index: Integer; Value: SBWInteger);
begin
  if FElementType <> dbtInteger then
    raise ESBWArrayError.Create('Array element type is not Integer');
  if (Index < 0) or (Index >= FTotalElements) then
    raise ESBWArrayError.CreateFmt('Index %d out of range [0..%d]', 
      [Index, FTotalElements - 1]);
  FIntegerData[Index] := Value;
end;

function TSBWArray.GetDouble(Index: Integer): SBWDouble;
begin
  if FElementType <> dbtDouble then
    raise ESBWArrayError.Create('Array element type is not Double');
  if (Index < 0) or (Index >= FTotalElements) then
    raise ESBWArrayError.CreateFmt('Index %d out of range [0..%d]', 
      [Index, FTotalElements - 1]);
  Result := FDoubleData[Index];
end;

procedure TSBWArray.SetDouble(Index: Integer; Value: SBWDouble);
begin
  if FElementType <> dbtDouble then
    raise ESBWArrayError.Create('Array element type is not Double');
  if (Index < 0) or (Index >= FTotalElements) then
    raise ESBWArrayError.CreateFmt('Index %d out of range [0..%d]', 
      [Index, FTotalElements - 1]);
  FDoubleData[Index] := Value;
end;

function TSBWArray.GetString(Index: Integer): string;
begin
  if FElementType <> dbtString then
    raise ESBWArrayError.Create('Array element type is not String');
  if (Index < 0) or (Index >= FTotalElements) then
    raise ESBWArrayError.CreateFmt('Index %d out of range [0..%d]', 
      [Index, FTotalElements - 1]);
  Result := FStringData[Index];
end;

procedure TSBWArray.SetString(Index: Integer; const Value: string);
begin
  if FElementType <> dbtString then
    raise ESBWArrayError.Create('Array element type is not String');
  if (Index < 0) or (Index >= FTotalElements) then
    raise ESBWArrayError.CreateFmt('Index %d out of range [0..%d]', 
      [Index, FTotalElements - 1]);
  FStringData[Index] := Value;
end;

function TSBWArray.GetComplex(Index: Integer): TSBWComplex;
begin
  if FElementType <> dbtComplex then
    raise ESBWArrayError.Create('Array element type is not Complex');
  if (Index < 0) or (Index >= FTotalElements) then
    raise ESBWArrayError.CreateFmt('Index %d out of range [0..%d]', 
      [Index, FTotalElements - 1]);
  Result := FComplexData[Index];
end;

procedure TSBWArray.SetComplex(Index: Integer; const Value: TSBWComplex);
begin
  if FElementType <> dbtComplex then
    raise ESBWArrayError.Create('Array element type is not Complex');
  if (Index < 0) or (Index >= FTotalElements) then
    raise ESBWArrayError.CreateFmt('Index %d out of range [0..%d]', 
      [Index, FTotalElements - 1]);
  FComplexData[Index] := Value;
end;

// =============================================================================
// Multi-dimensional Access
// =============================================================================

function TSBWArray.GetByteAt(const Indices: array of Integer): Byte;
begin
  if FElementType <> dbtByte then
    raise ESBWArrayError.Create('Array element type is not Byte');
  Result := FByteData[CalculateFlatIndex(Indices)];
end;

procedure TSBWArray.SetByteAt(const Indices: array of Integer; Value: Byte);
begin
  if FElementType <> dbtByte then
    raise ESBWArrayError.Create('Array element type is not Byte');
  FByteData[CalculateFlatIndex(Indices)] := Value;
end;

function TSBWArray.GetBooleanAt(const Indices: array of Integer): Boolean;
begin
  if FElementType <> dbtBoolean then
    raise ESBWArrayError.Create('Array element type is not Boolean');
  Result := FBooleanData[CalculateFlatIndex(Indices)];
end;

procedure TSBWArray.SetBooleanAt(const Indices: array of Integer; Value: Boolean);
begin
  if FElementType <> dbtBoolean then
    raise ESBWArrayError.Create('Array element type is not Boolean');
  FBooleanData[CalculateFlatIndex(Indices)] := Value;
end;

function TSBWArray.GetIntegerAt(const Indices: array of Integer): SBWInteger;
begin
  if FElementType <> dbtInteger then
    raise ESBWArrayError.Create('Array element type is not Integer');
  Result := FIntegerData[CalculateFlatIndex(Indices)];
end;

procedure TSBWArray.SetIntegerAt(const Indices: array of Integer; 
  Value: SBWInteger);
begin
  if FElementType <> dbtInteger then
    raise ESBWArrayError.Create('Array element type is not Integer');
  FIntegerData[CalculateFlatIndex(Indices)] := Value;
end;

function TSBWArray.GetDoubleAt(const Indices: array of Integer): SBWDouble;
begin
  if FElementType <> dbtDouble then
    raise ESBWArrayError.Create('Array element type is not Double');
  Result := FDoubleData[CalculateFlatIndex(Indices)];
end;

procedure TSBWArray.SetDoubleAt(const Indices: array of Integer; 
  Value: SBWDouble);
begin
  if FElementType <> dbtDouble then
    raise ESBWArrayError.Create('Array element type is not Double');
  FDoubleData[CalculateFlatIndex(Indices)] := Value;
end;

function TSBWArray.GetStringAt(const Indices: array of Integer): string;
begin
  if FElementType <> dbtString then
    raise ESBWArrayError.Create('Array element type is not String');
  Result := FStringData[CalculateFlatIndex(Indices)];
end;

procedure TSBWArray.SetStringAt(const Indices: array of Integer; 
  const Value: string);
begin
  if FElementType <> dbtString then
    raise ESBWArrayError.Create('Array element type is not String');
  FStringData[CalculateFlatIndex(Indices)] := Value;
end;

function TSBWArray.GetComplexAt(const Indices: array of Integer): TSBWComplex;
begin
  if FElementType <> dbtComplex then
    raise ESBWArrayError.Create('Array element type is not Complex');
  Result := FComplexData[CalculateFlatIndex(Indices)];
end;

procedure TSBWArray.SetComplexAt(const Indices: array of Integer; 
  const Value: TSBWComplex);
begin
  if FElementType <> dbtComplex then
    raise ESBWArrayError.Create('Array element type is not Complex');
  FComplexData[CalculateFlatIndex(Indices)] := Value;
end;

// =============================================================================
// 2D Convenience Methods
// =============================================================================

function TSBWArray.GetDouble2D(Row, Col: Integer): SBWDouble;
begin
  Result := GetDoubleAt([Row, Col]);
end;

procedure TSBWArray.SetDouble2D(Row, Col: Integer; Value: SBWDouble);
begin
  SetDoubleAt([Row, Col], Value);
end;

function TSBWArray.GetInteger2D(Row, Col: Integer): SBWInteger;
begin
  Result := GetIntegerAt([Row, Col]);
end;

procedure TSBWArray.SetInteger2D(Row, Col: Integer; Value: SBWInteger);
begin
  SetIntegerAt([Row, Col], Value);
end;

function TSBWArray.Rows: Integer;
begin
  if Length(FDimensions) < 1 then
    Result := 0
  else
    Result := FDimensions[0];
end;

function TSBWArray.Cols: Integer;
begin
  if Length(FDimensions) < 2 then
    Result := 0
  else
    Result := FDimensions[1];
end;

// =============================================================================
// Bulk Data Access
// =============================================================================

function TSBWArray.AsByteArray: TArray<Byte>;
begin
  if FElementType <> dbtByte then
    raise ESBWArrayError.Create('Array element type is not Byte');
  Result := Copy(FByteData);
end;

function TSBWArray.AsBooleanArray: TArray<Boolean>;
begin
  if FElementType <> dbtBoolean then
    raise ESBWArrayError.Create('Array element type is not Boolean');
  Result := Copy(FBooleanData);
end;

function TSBWArray.AsIntegerArray: TArray<SBWInteger>;
begin
  if FElementType <> dbtInteger then
    raise ESBWArrayError.Create('Array element type is not Integer');
  Result := Copy(FIntegerData);
end;

function TSBWArray.AsDoubleArray: TArray<SBWDouble>;
begin
  if FElementType <> dbtDouble then
    raise ESBWArrayError.Create('Array element type is not Double');
  Result := Copy(FDoubleData);
end;

function TSBWArray.AsStringArray: TArray<string>;
begin
  if FElementType <> dbtString then
    raise ESBWArrayError.Create('Array element type is not String');
  Result := Copy(FStringData);
end;

function TSBWArray.AsComplexArray: TArray<TSBWComplex>;
begin
  if FElementType <> dbtComplex then
    raise ESBWArrayError.Create('Array element type is not Complex');
  Result := Copy(FComplexData);
end;

function TSBWArray.AsDoubleArray2D: TArray<TArray<SBWDouble>>;
var
  I, J, RowCount, ColCount: Integer;
begin
  if FElementType <> dbtDouble then
    raise ESBWArrayError.Create('Array element type is not Double');
  if Length(FDimensions) <> 2 then
    raise ESBWArrayError.Create('Array is not 2-dimensional');
  
  RowCount := FDimensions[0];
  ColCount := FDimensions[1];
  SetLength(Result, RowCount);
  
  for I := 0 to RowCount - 1 do
  begin
    SetLength(Result[I], ColCount);
    for J := 0 to ColCount - 1 do
      Result[I][J] := FDoubleData[I * ColCount + J];
  end;
end;

function TSBWArray.AsIntegerArray2D: TArray<TArray<SBWInteger>>;
var
  I, J, RowCount, ColCount: Integer;
begin
  if FElementType <> dbtInteger then
    raise ESBWArrayError.Create('Array element type is not Integer');
  if Length(FDimensions) <> 2 then
    raise ESBWArrayError.Create('Array is not 2-dimensional');
  
  RowCount := FDimensions[0];
  ColCount := FDimensions[1];
  SetLength(Result, RowCount);
  
  for I := 0 to RowCount - 1 do
  begin
    SetLength(Result[I], ColCount);
    for J := 0 to ColCount - 1 do
      Result[I][J] := FIntegerData[I * ColCount + J];
  end;
end;

// =============================================================================
// Bulk Data Assignment
// =============================================================================

procedure TSBWArray.SetFromByteArray(const Data: TArray<Byte>);
begin
  if FElementType <> dbtByte then
    raise ESBWArrayError.Create('Array element type is not Byte');
  if Length(Data) <> FTotalElements then
    raise ESBWArrayError.CreateFmt(
      'Data length %d does not match array size %d', 
      [Length(Data), FTotalElements]);
  FByteData := Copy(Data);
end;

procedure TSBWArray.SetFromBooleanArray(const Data: TArray<Boolean>);
begin
  if FElementType <> dbtBoolean then
    raise ESBWArrayError.Create('Array element type is not Boolean');
  if Length(Data) <> FTotalElements then
    raise ESBWArrayError.CreateFmt(
      'Data length %d does not match array size %d', 
      [Length(Data), FTotalElements]);
  FBooleanData := Copy(Data);
end;

procedure TSBWArray.SetFromIntegerArray(const Data: TArray<SBWInteger>);
begin
  if FElementType <> dbtInteger then
    raise ESBWArrayError.Create('Array element type is not Integer');
  if Length(Data) <> FTotalElements then
    raise ESBWArrayError.CreateFmt(
      'Data length %d does not match array size %d', 
      [Length(Data), FTotalElements]);
  FIntegerData := Copy(Data);
end;

procedure TSBWArray.SetFromDoubleArray(const Data: TArray<SBWDouble>);
begin
  if FElementType <> dbtDouble then
    raise ESBWArrayError.Create('Array element type is not Double');
  if Length(Data) <> FTotalElements then
    raise ESBWArrayError.CreateFmt(
      'Data length %d does not match array size %d', 
      [Length(Data), FTotalElements]);
  FDoubleData := Copy(Data);
end;

procedure TSBWArray.SetFromStringArray(const Data: TArray<string>);
begin
  if FElementType <> dbtString then
    raise ESBWArrayError.Create('Array element type is not String');
  if Length(Data) <> FTotalElements then
    raise ESBWArrayError.CreateFmt(
      'Data length %d does not match array size %d', 
      [Length(Data), FTotalElements]);
  FStringData := Copy(Data);
end;

procedure TSBWArray.SetFromComplexArray(const Data: TArray<TSBWComplex>);
begin
  if FElementType <> dbtComplex then
    raise ESBWArrayError.Create('Array element type is not Complex');
  if Length(Data) <> FTotalElements then
    raise ESBWArrayError.CreateFmt(
      'Data length %d does not match array size %d', 
      [Length(Data), FTotalElements]);
  FComplexData := Copy(Data);
end;

procedure TSBWArray.SetFromDoubleArray2D(const Data: TArray<TArray<SBWDouble>>);
var
  I, J, RowCount, ColCount: Integer;
begin
  if FElementType <> dbtDouble then
    raise ESBWArrayError.Create('Array element type is not Double');
  
  RowCount := Length(Data);
  if RowCount = 0 then
  begin
    Reshape([0, 0]);
    Exit;
  end;
  
  ColCount := Length(Data[0]);
  Reshape([RowCount, ColCount]);
  
  for I := 0 to RowCount - 1 do
  begin
    if Length(Data[I]) <> ColCount then
      raise ESBWArrayError.Create('Jagged arrays are not supported');
    for J := 0 to ColCount - 1 do
      FDoubleData[I * ColCount + J] := Data[I][J];
  end;
end;

procedure TSBWArray.SetFromIntegerArray2D(const Data: TArray<TArray<SBWInteger>>);
var
  I, J, RowCount, ColCount: Integer;
begin
  if FElementType <> dbtInteger then
    raise ESBWArrayError.Create('Array element type is not Integer');
  
  RowCount := Length(Data);
  if RowCount = 0 then
  begin
    Reshape([0, 0]);
    Exit;
  end;
  
  ColCount := Length(Data[0]);
  Reshape([RowCount, ColCount]);
  
  for I := 0 to RowCount - 1 do
  begin
    if Length(Data[I]) <> ColCount then
      raise ESBWArrayError.Create('Jagged arrays are not supported');
    for J := 0 to ColCount - 1 do
      FIntegerData[I * ColCount + J] := Data[I][J];
  end;
end;

// =============================================================================
// Serialization
// =============================================================================

procedure TSBWArray.WriteTo(Writer: TSBWDataBlockWriter);
var
  I: Integer;
  UTF8Bytes: TBytes;
  TypeByte, ElementTypeByte, BoolByte: Byte;
  DimCount, StrLen: Int32;
begin
  // Write type byte
  TypeByte := Ord(dbtArray);
  Writer.Stream.WriteBuffer(TypeByte, 1);
  
  // Write element type
  ElementTypeByte := Ord(FElementType);
  Writer.Stream.WriteBuffer(ElementTypeByte, 1);
  
  // Write dimension count
  DimCount := Length(FDimensions);
  Writer.Stream.WriteBuffer(DimCount, SizeOf(Int32));
  
  // Write dimension sizes
  for I := 0 to High(FDimensions) do
    Writer.Stream.WriteBuffer(FDimensions[I], SizeOf(Int32));
  
  // Write element data
  case FElementType of
    dbtByte:
      begin
        if FTotalElements > 0 then
          Writer.Stream.WriteBuffer(FByteData[0], FTotalElements);
      end;
    
    dbtBoolean:
      begin
        for I := 0 to FTotalElements - 1 do
        begin
          if FBooleanData[I] then
            BoolByte := 1
          else
            BoolByte := 0;
          Writer.Stream.WriteBuffer(BoolByte, 1);
        end;
      end;
    
    dbtInteger:
      begin
        for I := 0 to FTotalElements - 1 do
          Writer.Stream.WriteBuffer(FIntegerData[I], SizeOf(Int32));
      end;
    
    dbtDouble:
      begin
        for I := 0 to FTotalElements - 1 do
          Writer.Stream.WriteBuffer(FDoubleData[I], SizeOf(Double));
      end;
    
    dbtString:
      begin
        BoolByte := 0; // Null terminator
        for I := 0 to FTotalElements - 1 do
        begin
          UTF8Bytes := TEncoding.UTF8.GetBytes(FStringData[I]);
          StrLen := Length(UTF8Bytes);
          Writer.Stream.WriteBuffer(StrLen, SizeOf(Int32));
          if StrLen > 0 then
            Writer.Stream.WriteBuffer(UTF8Bytes[0], StrLen);
          Writer.Stream.WriteBuffer(BoolByte, 1); // Null terminator
        end;
      end;
    
    dbtComplex:
      begin
        for I := 0 to FTotalElements - 1 do
        begin
          Writer.Stream.WriteBuffer(FComplexData[I].Real, SizeOf(Double));
          Writer.Stream.WriteBuffer(FComplexData[I].Imag, SizeOf(Double));
        end;
      end;
  end;
end;

class function TSBWArray.ReadFrom(Reader: TSBWDataBlockReader): TSBWArray;
var
  TypeByte, ElementTypeByte: Byte;
  DimCount, I, Len: Int32;
  Dims: TArray<Integer>;
  UTF8Bytes: TBytes;
  BoolByte: Byte;
begin
  // Read and verify type byte
  Reader.Stream.ReadBuffer(TypeByte, 1);
  if TSBWDataBlockType(TypeByte) <> dbtArray then
    raise ESBWArrayError.CreateFmt('Expected array type byte, got %d', [TypeByte]);
  
  // Read element type
  Reader.Stream.ReadBuffer(ElementTypeByte, 1);
  
  // Read dimension count
  Reader.Stream.ReadBuffer(DimCount, SizeOf(Int32));
  
  // Read dimension sizes
  SetLength(Dims, DimCount);
  for I := 0 to DimCount - 1 do
    Reader.Stream.ReadBuffer(Dims[I], SizeOf(Int32));
  
  // Create the array
  Result := TSBWArray.Create(TSBWDataBlockType(ElementTypeByte), Dims);
  try
    // Read element data
    case Result.FElementType of
      dbtByte:
        begin
          if Result.FTotalElements > 0 then
            Reader.Stream.ReadBuffer(Result.FByteData[0], Result.FTotalElements);
        end;
      
      dbtBoolean:
        begin
          for I := 0 to Result.FTotalElements - 1 do
          begin
            Reader.Stream.ReadBuffer(BoolByte, 1);
            Result.FBooleanData[I] := BoolByte <> 0;
          end;
        end;
      
      dbtInteger:
        begin
          for I := 0 to Result.FTotalElements - 1 do
            Reader.Stream.ReadBuffer(Result.FIntegerData[I], SizeOf(Int32));
        end;
      
      dbtDouble:
        begin
          for I := 0 to Result.FTotalElements - 1 do
            Reader.Stream.ReadBuffer(Result.FDoubleData[I], SizeOf(Double));
        end;
      
      dbtString:
        begin
          for I := 0 to Result.FTotalElements - 1 do
          begin
            Reader.Stream.ReadBuffer(Len, SizeOf(Int32));
            SetLength(UTF8Bytes, Len);
            if Len > 0 then
              Reader.Stream.ReadBuffer(UTF8Bytes[0], Len);
            Reader.Stream.ReadBuffer(BoolByte, 1); // Null terminator
            Result.FStringData[I] := TEncoding.UTF8.GetString(UTF8Bytes);
          end;
        end;
      
      dbtComplex:
        begin
          for I := 0 to Result.FTotalElements - 1 do
          begin
            Reader.Stream.ReadBuffer(Result.FComplexData[I].Real, SizeOf(Double));
            Reader.Stream.ReadBuffer(Result.FComplexData[I].Imag, SizeOf(Double));
          end;
        end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

// =============================================================================
// Utility
// =============================================================================

function TSBWArray.AsString: string;
var
  I: Integer;
  Builder: TStringBuilder;
  
  function ElementToString(Index: Integer): string;
  begin
    case FElementType of
      dbtByte:    Result := IntToStr(FByteData[Index]);
      dbtBoolean: if FBooleanData[Index] then Result := 'true' else Result := 'false';
      dbtInteger: Result := IntToStr(FIntegerData[Index]);
      dbtDouble:  Result := FloatToStr(FDoubleData[Index]);
      dbtString:  Result := '"' + FStringData[Index] + '"';
      dbtComplex: Result := Format('(%g + %gi)', 
                    [FComplexData[Index].Real, FComplexData[Index].Imag]);
    else
      Result := '?';
    end;
  end;
  
begin
  Builder := TStringBuilder.Create;
  try
    // Show type and shape
    Builder.Append('[');
    case FElementType of
      dbtByte:    Builder.Append('byte');
      dbtBoolean: Builder.Append('bool');
      dbtInteger: Builder.Append('int');
      dbtDouble:  Builder.Append('double');
      dbtString:  Builder.Append('string');
      dbtComplex: Builder.Append('complex');
    end;
    
    Builder.Append(' (');
    for I := 0 to High(FDimensions) do
    begin
      if I > 0 then Builder.Append(' x ');
      Builder.Append(IntToStr(FDimensions[I]));
    end;
    Builder.Append('): ');
    
    // Show elements (truncated if too many)
    if FTotalElements <= 10 then
    begin
      for I := 0 to FTotalElements - 1 do
      begin
        if I > 0 then Builder.Append(', ');
        Builder.Append(ElementToString(I));
      end;
    end
    else
    begin
      for I := 0 to 4 do
      begin
        if I > 0 then Builder.Append(', ');
        Builder.Append(ElementToString(I));
      end;
      Builder.Append(', ..., ');
      for I := FTotalElements - 3 to FTotalElements - 1 do
      begin
        if I > FTotalElements - 3 then Builder.Append(', ');
        Builder.Append(ElementToString(I));
      end;
    end;
    
    Builder.Append(']');
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function TSBWArray.Clone: TSBWArray;
begin
  Result := TSBWArray.Create(FElementType, FDimensions);
  
  case FElementType of
    dbtByte:    Result.FByteData := Copy(FByteData);
    dbtBoolean: Result.FBooleanData := Copy(FBooleanData);
    dbtInteger: Result.FIntegerData := Copy(FIntegerData);
    dbtDouble:  Result.FDoubleData := Copy(FDoubleData);
    dbtString:  Result.FStringData := Copy(FStringData);
    dbtComplex: Result.FComplexData := Copy(FComplexData);
  end;
end;

procedure TSBWArray.Clear;
var
  I: Integer;
  Zero: TSBWComplex;
begin
  Zero.Real := 0;
  Zero.Imag := 0;
  
  case FElementType of
    dbtByte:
      for I := 0 to FTotalElements - 1 do FByteData[I] := 0;
    dbtBoolean:
      for I := 0 to FTotalElements - 1 do FBooleanData[I] := False;
    dbtInteger:
      for I := 0 to FTotalElements - 1 do FIntegerData[I] := 0;
    dbtDouble:
      for I := 0 to FTotalElements - 1 do FDoubleData[I] := 0.0;
    dbtString:
      for I := 0 to FTotalElements - 1 do FStringData[I] := '';
    dbtComplex:
      for I := 0 to FTotalElements - 1 do FComplexData[I] := Zero;
  end;
end;

procedure TSBWArray.Fill(Value: SBWDouble);
var
  I: Integer;
begin
  if FElementType <> dbtDouble then
    raise ESBWArrayError.Create('Array element type is not Double');
  for I := 0 to FTotalElements - 1 do
    FDoubleData[I] := Value;
end;

procedure TSBWArray.Fill(Value: SBWInteger);
var
  I: Integer;
begin
  if FElementType <> dbtInteger then
    raise ESBWArrayError.Create('Array element type is not Integer');
  for I := 0 to FTotalElements - 1 do
    FIntegerData[I] := Value;
end;

procedure TSBWArray.Fill(const Value: string);
var
  I: Integer;
begin
  if FElementType <> dbtString then
    raise ESBWArrayError.Create('Array element type is not String');
  for I := 0 to FTotalElements - 1 do
    FStringData[I] := Value;
end;

// =============================================================================
// Factory Functions
// =============================================================================

function SBWDoubleArray(const Values: TArray<SBWDouble>): TSBWArray;
begin
  Result := TSBWArray.Create(dbtDouble, Length(Values));
  Result.SetFromDoubleArray(Values);
end;

function SBWIntegerArray(const Values: TArray<SBWInteger>): TSBWArray;
begin
  Result := TSBWArray.Create(dbtInteger, Length(Values));
  Result.SetFromIntegerArray(Values);
end;

function SBWStringArray(const Values: TArray<string>): TSBWArray;
begin
  Result := TSBWArray.Create(dbtString, Length(Values));
  Result.SetFromStringArray(Values);
end;

function SBWDoubleMatrix(const Values: TArray<TArray<SBWDouble>>): TSBWArray;
begin
  Result := TSBWArray.Create(dbtDouble);
  Result.SetFromDoubleArray2D(Values);
end;

function SBWIntegerMatrix(const Values: TArray<TArray<SBWInteger>>): TSBWArray;
begin
  Result := TSBWArray.Create(dbtInteger);
  Result.SetFromIntegerArray2D(Values);
end;

function SBWZeroMatrix(ARows, ACols: Integer): TSBWArray;
begin
  Result := TSBWArray.Create(dbtDouble, [ARows, ACols]);
  // Already zero-filled by SetLength
end;

function SBWIdentityMatrix(Size: Integer): TSBWArray;
var
  I: Integer;
begin
  Result := TSBWArray.Create(dbtDouble, [Size, Size]);
  for I := 0 to Size - 1 do
    Result.SetDouble2D(I, I, 1.0);
end;

end.
