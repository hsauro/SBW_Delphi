unit SBW.Signature;

{******************************************************************************
 * SBW.Signature.pas
 *
 * Method signature parsing for the Systems Biology Workbench.
 *
 * Parses SBW method signature strings like:
 *   double sin(double x)
 *   int[] getValues()
 }
//   {string name, double value} getPerson(int id)
{*
 *   void doAnalysis(string sbml)
 *   void f(...)
 *
 * Grammar (from SBW specification):
 *   Letter ::= 'a'..'z','A'..'Z'
 *   Digit ::= '0'..'9'
 *   Space ::= ( '\t' | ' ' )+
 *   SName ::= '_'* Letter ( Letter | Digit | '_' )*
 *   Type ::= 'int' | 'double' | 'string' | 'boolean' | 'byte' | 'complex' | ArrayType | ListType
 *   ArrayType ::= Type Space? '[]'
 *}
 //   ListType ::= '{' Space? ArgList Space? '}'
 {*
 *   ArgList ::= ( Type [Space SName] ( Space? ',' Space? Type [Space SName] )* )?
 *   ReturnType ::= 'void' | Type
 *   VarArgList ::= (Space? ArgList [Space? ',' Space? '...'] Space? ) | Space? '...' Space?
 *   Signature ::= ReturnType Space SName Space? '(' VarArgList ')'
 *****************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  SBW.Types;

type
  ESBWSignatureError = class(Exception);

  TSBWSignatureType = class;

  /// <summary>
  /// An argument/parameter in a signature
  /// </summary>
  TSBWSignatureElement = class
  private
    FName: string;
    FType: TSBWSignatureType;
  public
    constructor Create(AType: TSBWSignatureType; const AName: string = '');
    destructor Destroy; override;

    property Name: string read FName write FName;
    property DataType: TSBWSignatureType read FType;
  end;

  /// <summary>
  /// A type in a signature (scalar, array, or list)
  /// </summary>
  TSBWSignatureType = class
  private
    FBaseType: TSBWDataBlockType;
    FArrayDimensions: Integer;        // 0 = not array, 1 = 1D, 2 = 2D, etc.
    FArrayInnerType: TSBWSignatureType;
    FListContents: TObjectList<TSBWSignatureElement>;
    FOwnsInnerType: Boolean;
  public
    constructor Create(ABaseType: TSBWDataBlockType);
    destructor Destroy; override;

    /// <summary>
    /// Create an array type
    /// </summary>
    class function CreateArray(InnerType: TSBWSignatureType; Dimensions: Integer = 1): TSBWSignatureType;

    /// <summary>
    /// Create a list type
    /// </summary>
    class function CreateList: TSBWSignatureType;

    /// <summary>
    /// Convert to signature string representation
    /// </summary>
    function ToString: string; override;

    /// <summary>
    /// Check if this is an array type
    /// </summary>
    function IsArray: Boolean;

    /// <summary>
    /// Check if this is a list type
    /// </summary>
    function IsList: Boolean;

    /// <summary>
    /// Check if this is void
    /// </summary>
    function IsVoid: Boolean;

    property BaseType: TSBWDataBlockType read FBaseType;
    property ArrayDimensions: Integer read FArrayDimensions;
    property ArrayInnerType: TSBWSignatureType read FArrayInnerType;
    property ListContents: TObjectList<TSBWSignatureElement> read FListContents;
  end;

  /// <summary>
  /// A parsed method signature
  /// </summary>
  TSBWSignature = class
  private
    FName: string;
    FReturnType: TSBWSignatureType;
    FArguments: TObjectList<TSBWSignatureElement>;
    FHasVarArgs: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Convert back to signature string
    /// </summary>
    function ToString: string; override;

    property Name: string read FName write FName;
    property ReturnType: TSBWSignatureType read FReturnType write FReturnType;
    property Arguments: TObjectList<TSBWSignatureElement> read FArguments;
    property HasVarArgs: Boolean read FHasVarArgs write FHasVarArgs;
  end;

  /// <summary>
  /// Signature parser
  /// </summary>
  TSBWSignatureParser = class
  private
    FInput: string;
    FPos: Integer;
    FLength: Integer;

    function Peek: Char;
    function PeekAhead(Offset: Integer): Char;
    function AtEnd: Boolean;
    procedure Advance;
    procedure SkipWhitespace;
    function Match(C: Char): Boolean;
    function MatchString(const S: string): Boolean;
    function IsLetter(C: Char): Boolean;
    function IsDigit(C: Char): Boolean;
    function IsIdentChar(C: Char): Boolean;

    function ParseName: string;
    function ParseType: TSBWSignatureType;
    function ParseScalarType: TSBWSignatureType;
    function ParseListType: TSBWSignatureType;
    function ParseArgList: TObjectList<TSBWSignatureElement>;
    function ParseElement: TSBWSignatureElement;
  public
    /// <summary>
    /// Parse a complete method signature
    /// </summary>
    function Parse(const SignatureStr: string): TSBWSignature;

    /// <summary>
    /// Parse just a type specification
    /// </summary>
    function ParseTypeOnly(const TypeStr: string): TSBWSignatureType;
  end;

/// <summary>
/// Convert a base type to its string representation
/// </summary>
function DataBlockTypeToString(T: TSBWDataBlockType): string;

/// <summary>
/// Parse a type name string to enum
/// </summary>
function StringToDataBlockType(const S: string): TSBWDataBlockType;

implementation

function DataBlockTypeToString(T: TSBWDataBlockType): string;
begin
  case T of
    dbtByte:      Result := 'byte';
    dbtInteger:   Result := 'int';
    dbtDouble:    Result := 'double';
    dbtBoolean:   Result := 'boolean';
    dbtString:    Result := 'string';
    dbtArray:     Result := 'array';
    dbtList:      Result := 'list';
    dbtVoid:      Result := 'void';
    dbtComplex:   Result := 'complex';
  else
    Result := 'unknown';
  end;
end;

function StringToDataBlockType(const S: string): TSBWDataBlockType;
var
  LowerS: string;
begin
  LowerS := LowerCase(S);
  if LowerS = 'byte' then
    Result := dbtByte
  else if (LowerS = 'int') or (LowerS = 'integer') then
    Result := dbtInteger
  else if LowerS = 'double' then
    Result := dbtDouble
  else if (LowerS = 'boolean') or (LowerS = 'bool') then
    Result := dbtBoolean
  else if LowerS = 'string' then
    Result := dbtString
  else if LowerS = 'void' then
    Result := dbtVoid
  else if LowerS = 'complex' then
    Result := dbtComplex
  else
    raise ESBWSignatureError.CreateFmt('Unknown type: %s', [S]);
end;

{ TSBWSignatureElement }

constructor TSBWSignatureElement.Create(AType: TSBWSignatureType; const AName: string);
begin
  inherited Create;
  FType := AType;
  FName := AName;
end;

destructor TSBWSignatureElement.Destroy;
begin
  FType.Free;
  inherited;
end;

{ TSBWSignatureType }

constructor TSBWSignatureType.Create(ABaseType: TSBWDataBlockType);
begin
  inherited Create;
  FBaseType := ABaseType;
  FArrayDimensions := 0;
  FArrayInnerType := nil;
  FListContents := nil;
  FOwnsInnerType := False;
end;

destructor TSBWSignatureType.Destroy;
begin
  if FOwnsInnerType and (FArrayInnerType <> nil) then
    FArrayInnerType.Free;
  FListContents.Free;
  inherited;
end;

class function TSBWSignatureType.CreateArray(InnerType: TSBWSignatureType; Dimensions: Integer): TSBWSignatureType;
begin
  Result := TSBWSignatureType.Create(dbtArray);
  Result.FArrayDimensions := Dimensions;
  Result.FArrayInnerType := InnerType;
  Result.FOwnsInnerType := True;
end;

class function TSBWSignatureType.CreateList: TSBWSignatureType;
begin
  Result := TSBWSignatureType.Create(dbtList);
  Result.FListContents := TObjectList<TSBWSignatureElement>.Create(True);
end;

function TSBWSignatureType.IsArray: Boolean;
begin
  Result := FBaseType = dbtArray;
end;

function TSBWSignatureType.IsList: Boolean;
begin
  Result := FBaseType = dbtList;
end;

function TSBWSignatureType.IsVoid: Boolean;
begin
  Result := FBaseType = dbtVoid;
end;

function TSBWSignatureType.ToString: string;
var
  I: Integer;
  Elem: TSBWSignatureElement;
begin
  if IsArray then
  begin
    Result := FArrayInnerType.ToString;
    for I := 1 to FArrayDimensions do
      Result := Result + '[]';
  end
  else if IsList then
  begin
    Result := '{';
    if FListContents <> nil then
    begin
      for I := 0 to FListContents.Count - 1 do
      begin
        if I > 0 then
          Result := Result + ', ';
        Elem := FListContents[I];
        Result := Result + Elem.DataType.ToString;
        if Elem.Name <> '' then
          Result := Result + ' ' + Elem.Name;
      end;
    end;
    Result := Result + '}';
  end
  else
    Result := DataBlockTypeToString(FBaseType);
end;

{ TSBWSignature }

constructor TSBWSignature.Create;
begin
  inherited Create;
  FArguments := TObjectList<TSBWSignatureElement>.Create(True);
  FHasVarArgs := False;
end;

destructor TSBWSignature.Destroy;
begin
  FArguments.Free;
  FReturnType.Free;
  inherited;
end;

function TSBWSignature.ToString: string;
var
  I: Integer;
  Elem: TSBWSignatureElement;
begin
  Result := FReturnType.ToString + ' ' + FName + '(';

  for I := 0 to FArguments.Count - 1 do
  begin
    if I > 0 then
      Result := Result + ', ';
    Elem := FArguments[I];
    Result := Result + Elem.DataType.ToString;
    if Elem.Name <> '' then
      Result := Result + ' ' + Elem.Name;
  end;

  if FHasVarArgs then
  begin
    if FArguments.Count > 0 then
      Result := Result + ', ';
    Result := Result + '...';
  end;

  Result := Result + ')';
end;

{ TSBWSignatureParser }

function TSBWSignatureParser.Peek: Char;
begin
  if FPos <= FLength then
    Result := FInput[FPos]
  else
    Result := #0;
end;

function TSBWSignatureParser.PeekAhead(Offset: Integer): Char;
begin
  if FPos + Offset <= FLength then
    Result := FInput[FPos + Offset]
  else
    Result := #0;
end;

function TSBWSignatureParser.AtEnd: Boolean;
begin
  Result := FPos > FLength;
end;

procedure TSBWSignatureParser.Advance;
begin
  Inc(FPos);
end;

procedure TSBWSignatureParser.SkipWhitespace;
begin
  while (not AtEnd) and CharInSet(Peek, [' ', #9, #10, #13]) do
    Advance;
end;

function TSBWSignatureParser.Match(C: Char): Boolean;
begin
  if Peek = C then
  begin
    Advance;
    Result := True;
  end
  else
    Result := False;
end;

function TSBWSignatureParser.MatchString(const S: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
  begin
    if PeekAhead(I - 1) <> S[I] then
      Exit(False);
  end;
  // Make sure it's not a prefix of a longer identifier
  if IsIdentChar(PeekAhead(Length(S))) then
    Exit(False);

  for I := 1 to Length(S) do
    Advance;
  Result := True;
end;

function TSBWSignatureParser.IsLetter(C: Char): Boolean;
begin
  Result := CharInSet(C, ['a'..'z', 'A'..'Z']);
end;

function TSBWSignatureParser.IsDigit(C: Char): Boolean;
begin
  Result := CharInSet(C, ['0'..'9']);
end;

function TSBWSignatureParser.IsIdentChar(C: Char): Boolean;
begin
  Result := IsLetter(C) or IsDigit(C) or (C = '_');
end;

function TSBWSignatureParser.ParseName: string;
begin
  Result := '';

  // SName ::= '_'* Letter ( Letter | Digit | '_' )*
  // Allow leading underscores
  while Peek = '_' do
  begin
    Result := Result + '_';
    Advance;
  end;

  // Must have at least one letter
  if not IsLetter(Peek) then
    Exit('');

  while IsIdentChar(Peek) do
  begin
    Result := Result + Peek;
    Advance;
  end;
end;

function TSBWSignatureParser.ParseScalarType: TSBWSignatureType;
var
  TypeName: string;
begin
  // Try to match known type names
  if MatchString('double') then
    Result := TSBWSignatureType.Create(dbtDouble)
  else if MatchString('string') then
    Result := TSBWSignatureType.Create(dbtString)
  else if MatchString('boolean') then
    Result := TSBWSignatureType.Create(dbtBoolean)
  else if MatchString('bool') then
    Result := TSBWSignatureType.Create(dbtBoolean)
  else if MatchString('integer') then
    Result := TSBWSignatureType.Create(dbtInteger)
  else if MatchString('int') then
    Result := TSBWSignatureType.Create(dbtInteger)
  else if MatchString('byte') then
    Result := TSBWSignatureType.Create(dbtByte)
  else if MatchString('complex') then
    Result := TSBWSignatureType.Create(dbtComplex)
  else if MatchString('void') then
    Result := TSBWSignatureType.Create(dbtVoid)
  else
  begin
    TypeName := ParseName;
    if TypeName = '' then
      raise ESBWSignatureError.CreateFmt('Expected type at position %d', [FPos]);
    raise ESBWSignatureError.CreateFmt('Unknown type: %s', [TypeName]);
  end;
end;

function TSBWSignatureParser.ParseListType: TSBWSignatureType;
var
  Elements: TObjectList<TSBWSignatureElement>;
  Elem: TSBWSignatureElement;
begin
  // '{' Space? ArgList Space? '}'
  if not Match('{') then
    raise ESBWSignatureError.Create('Expected ''{''');

  Result := TSBWSignatureType.CreateList;
  try
    SkipWhitespace;

    // Parse list contents (ArgList)
    while (not AtEnd) and (Peek <> '}') do
    begin
      Elem := ParseElement;
      Result.ListContents.Add(Elem);

      SkipWhitespace;
      if Peek = ',' then
      begin
        Advance;
        SkipWhitespace;
      end
      else
        Break;
    end;

    SkipWhitespace;
    if not Match('}') then
      raise ESBWSignatureError.Create('Expected ''}''');
  except
    Result.Free;
    raise;
  end;
end;

function TSBWSignatureParser.ParseType: TSBWSignatureType;
var
  BaseType: TSBWSignatureType;
  Dims: Integer;
begin
  SkipWhitespace;

  // Check for list type
  if Peek = '{' then
  begin
    BaseType := ParseListType;
  end
  else
  begin
    BaseType := ParseScalarType;
  end;

  // Check for array dimensions
  SkipWhitespace;
  Dims := 0;
  while (Peek = '[') and (PeekAhead(1) = ']') do
  begin
    Advance; // [
    Advance; // ]
    Inc(Dims);
    SkipWhitespace;
  end;

  if Dims > 0 then
    Result := TSBWSignatureType.CreateArray(BaseType, Dims)
  else
    Result := BaseType;
end;

function TSBWSignatureParser.ParseElement: TSBWSignatureElement;
var
  ElemType: TSBWSignatureType;
  ElemName: string;
  SavePos: Integer;
begin
  ElemType := ParseType;
  try
    SkipWhitespace;

    // Try to parse optional name
    SavePos := FPos;
    ElemName := ParseName;

    // Check if what we parsed is actually the next type (no name given)
    // This happens when we see "int, double" - the "double" isn't a name
    if (ElemName <> '') and CharInSet(Peek, [',', ')', '}']) then
    begin
      // It's a valid name
    end
    else if ElemName <> '' then
    begin
      // Might be the next type - check if it's a keyword
      if (LowerCase(ElemName) = 'int') or (LowerCase(ElemName) = 'double') or
         (LowerCase(ElemName) = 'string') or (LowerCase(ElemName) = 'boolean') or
         (LowerCase(ElemName) = 'byte') or (LowerCase(ElemName) = 'complex') or
         (LowerCase(ElemName) = 'void') then
      begin
        // It's not a name, rewind
        FPos := SavePos;
        ElemName := '';
      end;
    end;

    Result := TSBWSignatureElement.Create(ElemType, ElemName);
    ElemType := nil; // Ownership transferred
  except
    ElemType.Free;
    raise;
  end;
end;

function TSBWSignatureParser.ParseArgList: TObjectList<TSBWSignatureElement>;
var
  Elem: TSBWSignatureElement;
begin
  Result := TObjectList<TSBWSignatureElement>.Create(True);
  try
    SkipWhitespace;

    while (not AtEnd) and (Peek <> ')') do
    begin
      // Check for varargs
      if (Peek = '.') and (PeekAhead(1) = '.') and (PeekAhead(2) = '.') then
        Break;

      Elem := ParseElement;
      Result.Add(Elem);

      SkipWhitespace;
      if Peek = ',' then
      begin
        Advance;
        SkipWhitespace;
        // Check for varargs after comma
        if (Peek = '.') and (PeekAhead(1) = '.') and (PeekAhead(2) = '.') then
          Break;
      end
      else
        Break;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TSBWSignatureParser.Parse(const SignatureStr: string): TSBWSignature;
var
  RetType: TSBWSignatureType;
  MethodName: string;
  Args: TObjectList<TSBWSignatureElement>;
  HasVarArgs: Boolean;
begin
  FInput := SignatureStr;
  FPos := 1;
  FLength := Length(SignatureStr);

  // Parse return type
  RetType := ParseType;
  try
    SkipWhitespace;

    // Parse method name
    MethodName := ParseName;
    if MethodName = '' then
      raise ESBWSignatureError.Create('Expected method name');

    SkipWhitespace;

    // Parse '('
    if not Match('(') then
      raise ESBWSignatureError.Create('Expected ''(''');

    // Parse argument list
    Args := ParseArgList;
    try
      SkipWhitespace;

      // Check for varargs
      HasVarArgs := False;
      if (Peek = '.') and (PeekAhead(1) = '.') and (PeekAhead(2) = '.') then
      begin
        Advance;
        Advance;
        Advance;
        HasVarArgs := True;
        SkipWhitespace;
      end;

      // Parse ')'
      if not Match(')') then
        raise ESBWSignatureError.Create('Expected '')''');

      // Build result
      Result := TSBWSignature.Create;
      Result.FReturnType := RetType;
      RetType := nil; // Ownership transferred

      Result.FName := MethodName;
      Result.FHasVarArgs := HasVarArgs;

      // Transfer arguments
      while Args.Count > 0 do
      begin
        Result.Arguments.Add(Args.Extract(Args[0]));
      end;
      Args.Free;
    except
      Args.Free;
      raise;
    end;
  except
    RetType.Free;
    raise;
  end;
end;

function TSBWSignatureParser.ParseTypeOnly(const TypeStr: string): TSBWSignatureType;
begin
  FInput := TypeStr;
  FPos := 1;
  FLength := Length(TypeStr);

  Result := ParseType;
end;

end.

