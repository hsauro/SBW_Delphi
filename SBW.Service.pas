unit SBW.Service;

{******************************************************************************
 * SBW.Service.pas
 *
 * Service and method handler infrastructure for SBW.
 *
 * A Service is a collection of methods exposed by a module. Each method
 * has a signature and a handler that processes incoming calls.
 *****************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Rtti,
  SBW.Types, SBW.DataBlock, SBW.Signature;

type
  TSBWServiceMethod = class;
  TSBWService = class;

  /// <summary>
  /// Method handler procedure type.
  /// FromModuleID: The module that made the call
  /// Args: DataBlockReader containing the arguments
  /// Returns: DataBlockWriter containing the result
  /// </summary>
  TSBWMethodHandler = reference to function(FromModuleID: SBWModuleID;
    Args: TSBWDataBlockReader): TSBWDataBlockWriter;

  /// <summary>
  /// Simple method handler that takes no args and returns nothing
  /// </summary>
  TSBWSimpleHandler = reference to procedure;

  /// <summary>
  /// A method within a service
  /// </summary>
  TSBWServiceMethod = class
  private
    FName: string;
    FSignature: TSBWSignature;
    FSignatureString: string;
    FHandler: TSBWMethodHandler;
    FHelp: string;
    FSynchronized: Boolean;
    FMethodID: SBWMethodID;
  public
    constructor Create(const ASignatureString: string; AHandler: TSBWMethodHandler);
    destructor Destroy; override;

    /// <summary>
    /// Invoke this method with the given arguments
    /// </summary>
    function Invoke(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;

    property Name: string read FName;
    property Signature: TSBWSignature read FSignature;
    property SignatureString: string read FSignatureString;
    property Handler: TSBWMethodHandler read FHandler write FHandler;
    property Help: string read FHelp write FHelp;
    property Synchronized: Boolean read FSynchronized write FSynchronized;
    property MethodID: SBWMethodID read FMethodID write FMethodID;
  end;

  /// <summary>
  /// A service exposed by a module
  /// </summary>
  TSBWService = class
  private
    FName: string;
    FDisplayName: string;
    FCategory: string;
    FHelp: string;
    FMethods: TObjectList<TSBWServiceMethod>;
    FMethodsByName: TDictionary<string, TSBWServiceMethod>;
    FServiceID: SBWServiceID;
    FNextMethodID: SBWMethodID;
  public
    constructor Create(const AName, ADisplayName, ACategory: string);
    destructor Destroy; override;

    /// <summary>
    /// Add a method to this service
    /// </summary>
    function AddMethod(const SignatureString: string; Handler: TSBWMethodHandler;
      const Help: string = ''; Synchronized: Boolean = False): TSBWServiceMethod;

    /// <summary>
    /// Find a method by name
    /// </summary>
    function FindMethod(const MethodName: string): TSBWServiceMethod;

    /// <summary>
    /// Find a method by ID
    /// </summary>
    function FindMethodByID(MethodID: SBWMethodID): TSBWServiceMethod;

    /// <summary>
    /// Find a method by signature (can match partial signatures)
    /// </summary>
    function FindMethodBySignature(const SignatureString: string): TSBWServiceMethod;

    /// <summary>
    /// Get all method signatures
    /// </summary>
    function GetMethodSignatures: TArray<string>;

    property Name: string read FName;
    property DisplayName: string read FDisplayName write FDisplayName;
    property Category: string read FCategory write FCategory;
    property Help: string read FHelp write FHelp;
    property Methods: TObjectList<TSBWServiceMethod> read FMethods;
    property ServiceID: SBWServiceID read FServiceID write FServiceID;
  end;

  /// <summary>
  /// Registry of services for a module
  /// </summary>
  TSBWServiceRegistry = class
  private
    FServices: TObjectList<TSBWService>;
    FServicesByName: TDictionary<string, TSBWService>;
    FNextServiceID: SBWServiceID;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Add a new service
    /// </summary>
    function AddService(const Name, DisplayName, Category: string;
      const Help: string = ''): TSBWService;

    /// <summary>
    /// Find a service by name
    /// </summary>
    function FindService(const ServiceName: string): TSBWService;

    /// <summary>
    /// Find a service by ID
    /// </summary>
    function FindServiceByID(ServiceID: SBWServiceID): TSBWService;

    /// <summary>
    /// Get all services in a category
    /// </summary>
    function FindServicesByCategory(const Category: string; Recursive: Boolean = True): TArray<TSBWService>;

    /// <summary>
    /// Get all service descriptors
    /// </summary>
    function GetServiceDescriptors: TArray<TSBWServiceDescriptor>;

    property Services: TObjectList<TSBWService> read FServices;
  end;

  /// <summary>
  /// Helper class to build method handlers from Delphi methods
  /// </summary>
  TSBWMethodBuilder = class
  public
    /// <summary>
    /// Create a handler for a method that takes a double and returns a double
    /// </summary>
    class function DoubleToDouble(Func: TFunc<Double, Double>): TSBWMethodHandler;

    /// <summary>
    /// Create a handler for a method that takes two doubles and returns a double
    /// </summary>
    class function DoubleDoubleToDouble(Func: TFunc<Double, Double, Double>): TSBWMethodHandler;

    /// <summary>
    /// Create a handler for a method that takes a string and returns void
    /// </summary>
    class function StringToVoid(Proc: TProc<string>): TSBWMethodHandler;

    /// <summary>
    /// Create a handler for a method that takes nothing and returns an integer
    /// </summary>
    class function VoidToInteger(Func: TFunc<Integer>): TSBWMethodHandler;

    /// <summary>
    /// Create a handler for a method that takes nothing and returns a string
    /// </summary>
    class function VoidToString(Func: TFunc<string>): TSBWMethodHandler;

    /// <summary>
    /// Create a handler for a method that takes an integer array and returns an integer
    /// </summary>
    class function IntArrayToInt(Func: TFunc<TArray<Integer>, Integer>): TSBWMethodHandler;

    /// <summary>
    /// Create a handler for a method that takes a double array and returns a double
    /// </summary>
    class function DoubleArrayToDouble(Func: TFunc<TArray<Double>, Double>): TSBWMethodHandler;

    /// <summary>
    /// Create a handler for a void method with no arguments
    /// </summary>
    class function VoidToVoid(Proc: TProc): TSBWMethodHandler;
  end;

implementation

{ TSBWServiceMethod }

constructor TSBWServiceMethod.Create(const ASignatureString: string; AHandler: TSBWMethodHandler);
var
  Parser: TSBWSignatureParser;
begin
  inherited Create;
  FSignatureString := ASignatureString;
  FHandler := AHandler;
  FSynchronized := False;

  Parser := TSBWSignatureParser.Create;
  try
    FSignature := Parser.Parse(ASignatureString);
    FName := FSignature.Name;
  finally
    Parser.Free;
  end;
end;

destructor TSBWServiceMethod.Destroy;
begin
  FSignature.Free;
  inherited;
end;

function TSBWServiceMethod.Invoke(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;
begin
  if Assigned(FHandler) then
    Result := FHandler(FromModuleID, Args)
  else
    Result := TSBWDataBlockWriter.Create; // Return empty result
end;

{ TSBWService }

constructor TSBWService.Create(const AName, ADisplayName, ACategory: string);
begin
  inherited Create;
  FName := AName;
  FDisplayName := ADisplayName;
  FCategory := ACategory;
  FMethods := TObjectList<TSBWServiceMethod>.Create(True);
  FMethodsByName := TDictionary<string, TSBWServiceMethod>.Create;
  FNextMethodID := 0;
end;

destructor TSBWService.Destroy;
begin
  FMethodsByName.Free;
  FMethods.Free;
  inherited;
end;

function TSBWService.AddMethod(const SignatureString: string; Handler: TSBWMethodHandler;
  const Help: string; Synchronized: Boolean): TSBWServiceMethod;
begin
  Result := TSBWServiceMethod.Create(SignatureString, Handler);
  Result.Help := Help;
  Result.Synchronized := Synchronized;
  Result.MethodID := FNextMethodID;
  Inc(FNextMethodID);

  FMethods.Add(Result);
  FMethodsByName.Add(LowerCase(Result.Name), Result);
end;

function TSBWService.FindMethod(const MethodName: string): TSBWServiceMethod;
begin
  if not FMethodsByName.TryGetValue(LowerCase(MethodName), Result) then
    Result := nil;
end;

function TSBWService.FindMethodByID(MethodID: SBWMethodID): TSBWServiceMethod;
var
  Method: TSBWServiceMethod;
begin
  for Method in FMethods do
    if Method.MethodID = MethodID then
      Exit(Method);
  Result := nil;
end;

function TSBWService.FindMethodBySignature(const SignatureString: string): TSBWServiceMethod;
var
  Parser: TSBWSignatureParser;
  Sig: TSBWSignature;
  Method: TSBWServiceMethod;
begin
  // First try to parse as a full signature
  Parser := TSBWSignatureParser.Create;
  try
    try
      Sig := Parser.Parse(SignatureString);
      try
        // Look for exact name match first
        Result := FindMethod(Sig.Name);
        if Result <> nil then
          Exit;
      finally
        Sig.Free;
      end;
    except
      // Not a valid signature, try as just a name
      Result := FindMethod(SignatureString);
      if Result <> nil then
        Exit;
    end;
  finally
    Parser.Free;
  end;

  // Try partial match
  for Method in FMethods do
    if Pos(LowerCase(SignatureString), LowerCase(Method.SignatureString)) > 0 then
      Exit(Method);

  Result := nil;
end;

function TSBWService.GetMethodSignatures: TArray<string>;
var
  I: Integer;
begin
  SetLength(Result, FMethods.Count);
  for I := 0 to FMethods.Count - 1 do
    Result[I] := FMethods[I].SignatureString;
end;

{ TSBWServiceRegistry }

constructor TSBWServiceRegistry.Create;
begin
  inherited Create;
  FServices := TObjectList<TSBWService>.Create(True);
  FServicesByName := TDictionary<string, TSBWService>.Create;
  FNextServiceID := 0;
end;

destructor TSBWServiceRegistry.Destroy;
begin
  FServicesByName.Free;
  FServices.Free;
  inherited;
end;

function TSBWServiceRegistry.AddService(const Name, DisplayName, Category: string;
  const Help: string): TSBWService;
begin
  Result := TSBWService.Create(Name, DisplayName, Category);
  Result.Help := Help;
  Result.ServiceID := FNextServiceID;
  Inc(FNextServiceID);

  FServices.Add(Result);
  FServicesByName.Add(LowerCase(Name), Result);
end;

function TSBWServiceRegistry.FindService(const ServiceName: string): TSBWService;
begin
  if not FServicesByName.TryGetValue(LowerCase(ServiceName), Result) then
    Result := nil;
end;

function TSBWServiceRegistry.FindServiceByID(ServiceID: SBWServiceID): TSBWService;
var
  Service: TSBWService;
begin
  for Service in FServices do
    if Service.ServiceID = ServiceID then
      Exit(Service);
  Result := nil;
end;

function TSBWServiceRegistry.FindServicesByCategory(const Category: string;
  Recursive: Boolean): TArray<TSBWService>;
var
  List: TList<TSBWService>;
  Service: TSBWService;
  LowerCat, ServiceCat: string;
begin
  List := TList<TSBWService>.Create;
  try
    LowerCat := LowerCase(Category);
    for Service in FServices do
    begin
      ServiceCat := LowerCase(Service.Category);
      if Recursive then
      begin
        // Match if category starts with the search category
        if (ServiceCat = LowerCat) or
           (Pos(LowerCat + '/', ServiceCat) = 1) then
          List.Add(Service);
      end
      else
      begin
        // Exact match only
        if ServiceCat = LowerCat then
          List.Add(Service);
      end;
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

function TSBWServiceRegistry.GetServiceDescriptors: TArray<TSBWServiceDescriptor>;
var
  I: Integer;
  Service: TSBWService;
begin
  SetLength(Result, FServices.Count);
  for I := 0 to FServices.Count - 1 do
  begin
    Service := FServices[I];
    Result[I].ServiceName := Service.Name;
    Result[I].ServiceDisplayName := Service.DisplayName;
    Result[I].ServiceCategory := Service.Category;
    Result[I].Help := Service.Help;
    // Module info would be filled in by the caller
  end;
end;

{ TSBWMethodBuilder }

class function TSBWMethodBuilder.DoubleToDouble(Func: TFunc<Double, Double>): TSBWMethodHandler;
begin
  Result := function(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter
    var
      Arg, Res: Double;
    begin
      Arg := Args.ReadDouble;
      Res := Func(Arg);
      Result := TSBWDataBlockWriter.Create;
      Result.WriteDouble(Res);
    end;
end;

class function TSBWMethodBuilder.DoubleDoubleToDouble(Func: TFunc<Double, Double, Double>): TSBWMethodHandler;
begin
  Result := function(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter
    var
      Arg1, Arg2, Res: Double;
    begin
      Arg1 := Args.ReadDouble;
      Arg2 := Args.ReadDouble;
      Res := Func(Arg1, Arg2);
      Result := TSBWDataBlockWriter.Create;
      Result.WriteDouble(Res);
    end;
end;

class function TSBWMethodBuilder.StringToVoid(Proc: TProc<string>): TSBWMethodHandler;
begin
  Result := function(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter
    var
      Arg: string;
    begin
      Arg := Args.ReadString;
      Proc(Arg);
      Result := TSBWDataBlockWriter.Create;
    end;
end;

class function TSBWMethodBuilder.VoidToInteger(Func: TFunc<Integer>): TSBWMethodHandler;
begin
  Result := function(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter
    var
      Res: Integer;
    begin
      Res := Func();
      Result := TSBWDataBlockWriter.Create;
      Result.WriteInteger(Res);
    end;
end;

class function TSBWMethodBuilder.VoidToString(Func: TFunc<string>): TSBWMethodHandler;
begin
  Result := function(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter
    var
      Res: string;
    begin
      Res := Func();
      Result := TSBWDataBlockWriter.Create;
      Result.WriteString(Res);
    end;
end;

class function TSBWMethodBuilder.IntArrayToInt(Func: TFunc<TArray<Integer>, Integer>): TSBWMethodHandler;
begin
  Result := function(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter
    var
      Arr: TArray<Integer>;
      Res: Integer;
    begin
      Arr := Args.ReadIntegerArray;
      Res := Func(Arr);
      Result := TSBWDataBlockWriter.Create;
      Result.WriteInteger(Res);
    end;
end;

class function TSBWMethodBuilder.DoubleArrayToDouble(Func: TFunc<TArray<Double>, Double>): TSBWMethodHandler;
begin
  Result := function(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter
    var
      Arr: TArray<Double>;
      Res: Double;
    begin
      Arr := Args.ReadDoubleArray;
      Res := Func(Arr);
      Result := TSBWDataBlockWriter.Create;
      Result.WriteDouble(Res);
    end;
end;

class function TSBWMethodBuilder.VoidToVoid(Proc: TProc): TSBWMethodHandler;
begin
  Result := function(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter
    begin
      Proc();
      Result := TSBWDataBlockWriter.Create;
    end;
end;

end.

