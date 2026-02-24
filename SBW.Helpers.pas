unit SBW.Helpers;

{******************************************************************************
 * SBW.Helpers.pas
 *
 * Convenience helper methods for working with SBW.
 *
 * These methods wrap the TSBWModuleImpl broker query methods to return
 * simpler Delphi types instead of TSBWList objects.
 *
 * Categories:
 *   - Module Discovery: Find and list connected modules
 *   - Service Discovery: Find and list services on modules
 *   - Method Discovery: Find and list methods on services
 *   - Descriptor Helpers: Extract info from descriptors
 *   - Call Helpers: Simplified method invocation
 *****************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  SBW.Types, SBW.DataBlock, SBW.List, SBW.Module;

type
  /// <summary>
  /// Simple record for module info
  /// </summary>
  TSBWModuleEntry = record
    ModuleID: SBWModuleID;
    ModuleName: string;
  end;

  /// <summary>
  /// Simple record for service info
  /// </summary>
  TSBWServiceEntry = record
    ServiceID: SBWServiceID;
    ServiceName: string;
  end;

  /// <summary>
  /// Simple record for method info
  /// </summary>
  TSBWMethodEntry = record
    MethodID: SBWMethodID;
    Signature: string;
    Name: string;  // Extracted from signature
  end;

  /// <summary>
  /// Module descriptor unpacked into a record
  /// </summary>
  TSBWModuleDescriptorRec = record
    Name: string;
    DisplayName: string;
    ManagementType: Integer;
    CommandLine: string;
    Help: string;
  end;

  /// <summary>
  /// Service descriptor unpacked into a record
  /// </summary>
  TSBWServiceDescriptorRec = record
    ModuleName: string;
    ServiceName: string;
    DisplayName: string;
    Category: string;
    Help: string;
  end;

  /// <summary>
  /// Helper class with static convenience methods
  /// </summary>
  TSBWHelpers = class
  public
    //=========================================================================
    // Module Discovery Helpers
    //=========================================================================

    /// <summary>
    /// Get list of all connected module names
    /// </summary>
    class function GetModuleNames(Module: TSBWModuleImpl): TArray<string>;

    /// <summary>
    /// Get list of all connected module IDs
    /// </summary>
    class function GetModuleIds(Module: TSBWModuleImpl): TArray<SBWModuleID>;

    /// <summary>
    /// Get list of all connected modules as ID/Name pairs
    /// </summary>
    class function GetModules(Module: TSBWModuleImpl): TArray<TSBWModuleEntry>;

    /// <summary>
    /// Check if a module with the given name is connected
    /// </summary>
    class function ModuleExists(Module: TSBWModuleImpl; const ModuleName: string): Boolean;

    /// <summary>
    /// Get module ID by name, raises exception if not found
    /// </summary>
    class function RequireModuleId(Module: TSBWModuleImpl; const ModuleName: string): SBWModuleID;

    //=========================================================================
    // Service Discovery Helpers
    //=========================================================================

    /// <summary>
    /// Get list of service names for a module
    /// </summary>
    class function GetServiceNames(Module: TSBWModuleImpl; ModuleID: SBWModuleID): TArray<string>; overload;
    class function GetServiceNames(Module: TSBWModuleImpl; const ModuleName: string): TArray<string>; overload;

    /// <summary>
    /// Get list of services as ID/Name pairs
    /// </summary>
    class function GetServices(Module: TSBWModuleImpl; ModuleID: SBWModuleID): TArray<TSBWServiceEntry>; overload;
    class function GetServices(Module: TSBWModuleImpl; const ModuleName: string): TArray<TSBWServiceEntry>; overload;

    /// <summary>
    /// Check if a service exists on a module
    /// </summary>
    class function ServiceExists(Module: TSBWModuleImpl; ModuleID: SBWModuleID;
      const ServiceName: string): Boolean;

    /// <summary>
    /// Get service ID by name, raises exception if not found
    /// </summary>
    class function RequireServiceId(Module: TSBWModuleImpl; ModuleID: SBWModuleID;
      const ServiceName: string): SBWServiceID; overload;
    class function RequireServiceId(Module: TSBWModuleImpl; const ModuleName, ServiceName: string): SBWServiceID; overload;

    //=========================================================================
    // Method Discovery Helpers
    //=========================================================================

    /// <summary>
    /// Get list of method signatures for a service
    /// </summary>
    class function GetMethodSignatures(Module: TSBWModuleImpl; ModuleID: SBWModuleID;
      ServiceID: SBWServiceID): TArray<string>; overload;
    class function GetMethodSignatures(Module: TSBWModuleImpl; const ModuleName,
      ServiceName: string): TArray<string>; overload;

    /// <summary>
    /// Get list of method names (extracted from signatures)
    /// </summary>
    class function GetMethodNames(Module: TSBWModuleImpl; ModuleID: SBWModuleID;
      ServiceID: SBWServiceID): TArray<string>; overload;
    class function GetMethodNames(Module: TSBWModuleImpl; const ModuleName,
      ServiceName: string): TArray<string>; overload;

    /// <summary>
    /// Get list of methods as ID/Signature/Name records
    /// </summary>
    class function GetMethods(Module: TSBWModuleImpl; ModuleID: SBWModuleID;
      ServiceID: SBWServiceID): TArray<TSBWMethodEntry>; overload;
    class function GetMethods(Module: TSBWModuleImpl; const ModuleName,
      ServiceName: string): TArray<TSBWMethodEntry>; overload;

    /// <summary>
    /// Get method ID by name, raises exception if not found
    /// </summary>
    class function RequireMethodId(Module: TSBWModuleImpl; ModuleID: SBWModuleID;
      ServiceID: SBWServiceID; const MethodName: string): SBWMethodID; overload;
    class function RequireMethodId(Module: TSBWModuleImpl; const ModuleName,
      ServiceName, MethodName: string): SBWMethodID; overload;

    //=========================================================================
    // Descriptor Helpers
    //=========================================================================

    /// <summary>
    /// Unpack a module descriptor list into a record
    /// </summary>
    class function UnpackModuleDescriptor(const Descriptor: TSBWList): TSBWModuleDescriptorRec;

    /// <summary>
    /// Unpack a service descriptor list into a record
    /// </summary>
    class function UnpackServiceDescriptor(const Descriptor: TSBWList): TSBWServiceDescriptorRec;

    /// <summary>
    /// Get module descriptor as a record
    /// </summary>
    class function GetModuleDescriptorRec(Module: TSBWModuleImpl;
      const ModuleName: string; IncludeRunning: Boolean = True): TSBWModuleDescriptorRec; overload;
    class function GetModuleDescriptorRec(Module: TSBWModuleImpl;
      ModuleID: SBWModuleID): TSBWModuleDescriptorRec; overload;

    /// <summary>
    /// Get all module descriptors as records
    /// </summary>
    class function GetModuleDescriptorRecs(Module: TSBWModuleImpl;
      LocalOnly, IncludeRunning: Boolean): TArray<TSBWModuleDescriptorRec>;

    /// <summary>
    /// Get service descriptor as a record
    /// </summary>
    class function GetServiceDescriptorRec(Module: TSBWModuleImpl;
      ModuleID: SBWModuleID; const ServiceName: string): TSBWServiceDescriptorRec; overload;
    class function GetServiceDescriptorRec(Module: TSBWModuleImpl;
      ModuleID: SBWModuleID; ServiceID: SBWServiceID): TSBWServiceDescriptorRec; overload;

    /// <summary>
    /// Get all service descriptors for a module as records
    /// </summary>
    class function GetServiceDescriptorRecs(Module: TSBWModuleImpl;
      ModuleID: SBWModuleID): TArray<TSBWServiceDescriptorRec>;

    //=========================================================================
    // Signature Parsing Helpers
    //=========================================================================

    /// <summary>
    /// Extract method name from a signature string like "double sin(double)"
    /// </summary>
    class function ExtractMethodName(const Signature: string): string;

    /// <summary>
    /// Extract return type from a signature string
    /// </summary>
    class function ExtractReturnType(const Signature: string): string;

    //=========================================================================
    // Simplified Call Helpers
    //=========================================================================

    /// <summary>
    /// Call a method that takes a double and returns a double
    /// </summary>
    class function CallDoubleToDouble(Module: TSBWModuleImpl;
      ModuleID: SBWModuleID; ServiceID: SBWServiceID; MethodID: SBWMethodID;
      Arg: Double): Double; overload;
    class function CallDoubleToDouble(Module: TSBWModuleImpl;
      const ModuleName, ServiceName, MethodName: string; Arg: Double): Double; overload;

    /// <summary>
    /// Call a method that takes two doubles and returns a double
    /// </summary>
    class function CallDoubleDoubleToDouble(Module: TSBWModuleImpl;
      ModuleID: SBWModuleID; ServiceID: SBWServiceID; MethodID: SBWMethodID;
      Arg1, Arg2: Double): Double; overload;
    class function CallDoubleDoubleToDouble(Module: TSBWModuleImpl;
      const ModuleName, ServiceName, MethodName: string; Arg1, Arg2: Double): Double; overload;

    /// <summary>
    /// Call a method that takes an integer array and returns an integer
    /// </summary>
    class function CallIntArrayToInt(Module: TSBWModuleImpl;
      ModuleID: SBWModuleID; ServiceID: SBWServiceID; MethodID: SBWMethodID;
      const Args: TArray<Integer>): Integer; overload;
    class function CallIntArrayToInt(Module: TSBWModuleImpl;
      const ModuleName, ServiceName, MethodName: string;
      const Args: TArray<Integer>): Integer; overload;

    /// <summary>
    /// Call a method that takes a string and returns a string
    /// </summary>
    class function CallStringToString(Module: TSBWModuleImpl;
      ModuleID: SBWModuleID; ServiceID: SBWServiceID; MethodID: SBWMethodID;
      const Arg: string): string; overload;
    class function CallStringToString(Module: TSBWModuleImpl;
      const ModuleName, ServiceName, MethodName: string; const Arg: string): string; overload;

    /// <summary>
    /// Call a method that takes no arguments and returns a string
    /// </summary>
    class function CallVoidToString(Module: TSBWModuleImpl;
      ModuleID: SBWModuleID; ServiceID: SBWServiceID; MethodID: SBWMethodID): string; overload;
    class function CallVoidToString(Module: TSBWModuleImpl;
      const ModuleName, ServiceName, MethodName: string): string; overload;
  end;

  /// <summary>
  /// Exception raised when a required SBW resource is not found
  /// </summary>
  ESBWNotFound = class(Exception);

implementation

{ TSBWHelpers }

//=============================================================================
// Module Discovery Helpers
//=============================================================================

class function TSBWHelpers.GetModuleNames(Module: TSBWModuleImpl): TArray<string>;
var
  List: TSBWList;
  I: Integer;
begin
  List := Module.GetListOfModules;
  try
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
      Result[I] := List[I].GetList[1].GetString;
  finally
    List.Free;
  end;
end;

class function TSBWHelpers.GetModuleIds(Module: TSBWModuleImpl): TArray<SBWModuleID>;
var
  List: TSBWList;
  I: Integer;
begin
  List := Module.GetListOfModules;
  try
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
      Result[I] := List[I].GetList[0].GetInteger;
  finally
    List.Free;
  end;
end;

class function TSBWHelpers.GetModules(Module: TSBWModuleImpl): TArray<TSBWModuleEntry>;
var
  List: TSBWList;
  Item: TSBWList;
  I: Integer;
begin
  List := Module.GetListOfModules;
  try
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
    begin
      Item := List[I].GetList;
      Result[I].ModuleID := Item[0].GetInteger;
      Result[I].ModuleName := Item[1].GetString;
    end;
  finally
    List.Free;
  end;
end;

class function TSBWHelpers.ModuleExists(Module: TSBWModuleImpl;
  const ModuleName: string): Boolean;
begin
  Result := Module.GetModuleIdByName(ModuleName) <> SBW_NO_MODULE_ID;
end;

class function TSBWHelpers.RequireModuleId(Module: TSBWModuleImpl;
  const ModuleName: string): SBWModuleID;
begin
  Result := Module.GetModuleIdByName(ModuleName);
  if Result = SBW_NO_MODULE_ID then
    raise ESBWNotFound.CreateFmt('Module not found: %s', [ModuleName]);
end;

//=============================================================================
// Service Discovery Helpers
//=============================================================================

class function TSBWHelpers.GetServiceNames(Module: TSBWModuleImpl;
  ModuleID: SBWModuleID): TArray<string>;
var
  List: TSBWList;
  I: Integer;
begin
  List := Module.GetServiceIds(ModuleID);
  try
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
      Result[I] := List[I].GetList[1].GetString;
  finally
    List.Free;
  end;
end;

class function TSBWHelpers.GetServiceNames(Module: TSBWModuleImpl;
  const ModuleName: string): TArray<string>;
begin
  Result := GetServiceNames(Module, RequireModuleId(Module, ModuleName));
end;

class function TSBWHelpers.GetServices(Module: TSBWModuleImpl;
  ModuleID: SBWModuleID): TArray<TSBWServiceEntry>;
var
  List: TSBWList;
  Item: TSBWList;
  I: Integer;
begin
  List := Module.GetServiceIds(ModuleID);
  try
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
    begin
      Item := List[I].GetList;
      Result[I].ServiceID := Item[0].GetInteger;
      Result[I].ServiceName := Item[1].GetString;
    end;
  finally
    List.Free;
  end;
end;

class function TSBWHelpers.GetServices(Module: TSBWModuleImpl;
  const ModuleName: string): TArray<TSBWServiceEntry>;
begin
  Result := GetServices(Module, RequireModuleId(Module, ModuleName));
end;

class function TSBWHelpers.ServiceExists(Module: TSBWModuleImpl;
  ModuleID: SBWModuleID; const ServiceName: string): Boolean;
begin
  Result := Module.FindServiceId(ModuleID, ServiceName) >= 0;
end;

class function TSBWHelpers.RequireServiceId(Module: TSBWModuleImpl;
  ModuleID: SBWModuleID; const ServiceName: string): SBWServiceID;
begin
  Result := Module.FindServiceId(ModuleID, ServiceName);
  if Result < 0 then
    raise ESBWNotFound.CreateFmt('Service not found: %s', [ServiceName]);
end;

class function TSBWHelpers.RequireServiceId(Module: TSBWModuleImpl;
  const ModuleName, ServiceName: string): SBWServiceID;
begin
  Result := RequireServiceId(Module, RequireModuleId(Module, ModuleName), ServiceName);
end;

//=============================================================================
// Method Discovery Helpers
//=============================================================================

class function TSBWHelpers.GetMethodSignatures(Module: TSBWModuleImpl;
  ModuleID: SBWModuleID; ServiceID: SBWServiceID): TArray<string>;
var
  List: TSBWList;
  I: Integer;
begin
  List := Module.GetMethodIds(ModuleID, ServiceID);
  try
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
      Result[I] := List[I].GetList[1].GetString;
  finally
    List.Free;
  end;
end;

class function TSBWHelpers.GetMethodSignatures(Module: TSBWModuleImpl;
  const ModuleName, ServiceName: string): TArray<string>;
var
  ModuleID: SBWModuleID;
  ServiceID: SBWServiceID;
begin
  ModuleID := RequireModuleId(Module, ModuleName);
  ServiceID := RequireServiceId(Module, ModuleID, ServiceName);
  Result := GetMethodSignatures(Module, ModuleID, ServiceID);
end;

class function TSBWHelpers.GetMethodNames(Module: TSBWModuleImpl;
  ModuleID: SBWModuleID; ServiceID: SBWServiceID): TArray<string>;
var
  Signatures: TArray<string>;
  I: Integer;
begin
  Signatures := GetMethodSignatures(Module, ModuleID, ServiceID);
  SetLength(Result, Length(Signatures));
  for I := 0 to High(Signatures) do
    Result[I] := ExtractMethodName(Signatures[I]);
end;

class function TSBWHelpers.GetMethodNames(Module: TSBWModuleImpl;
  const ModuleName, ServiceName: string): TArray<string>;
var
  ModuleID: SBWModuleID;
  ServiceID: SBWServiceID;
begin
  ModuleID := RequireModuleId(Module, ModuleName);
  ServiceID := RequireServiceId(Module, ModuleID, ServiceName);
  Result := GetMethodNames(Module, ModuleID, ServiceID);
end;

class function TSBWHelpers.GetMethods(Module: TSBWModuleImpl;
  ModuleID: SBWModuleID; ServiceID: SBWServiceID): TArray<TSBWMethodEntry>;
var
  List: TSBWList;
  Item: TSBWList;
  I: Integer;
begin
  List := Module.GetMethodIds(ModuleID, ServiceID);
  try
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
    begin
      Item := List[I].GetList;
      Result[I].MethodID := Item[0].GetInteger;
      Result[I].Signature := Item[1].GetString;
      Result[I].Name := ExtractMethodName(Result[I].Signature);
    end;
  finally
    List.Free;
  end;
end;

class function TSBWHelpers.GetMethods(Module: TSBWModuleImpl;
  const ModuleName, ServiceName: string): TArray<TSBWMethodEntry>;
var
  ModuleID: SBWModuleID;
  ServiceID: SBWServiceID;
begin
  ModuleID := RequireModuleId(Module, ModuleName);
  ServiceID := RequireServiceId(Module, ModuleID, ServiceName);
  Result := GetMethods(Module, ModuleID, ServiceID);
end;

class function TSBWHelpers.RequireMethodId(Module: TSBWModuleImpl;
  ModuleID: SBWModuleID; ServiceID: SBWServiceID;
  const MethodName: string): SBWMethodID;
var
  Methods: TArray<TSBWMethodEntry>;
  I: Integer;
  LowerName: string;
begin
  Methods := GetMethods(Module, ModuleID, ServiceID);
  LowerName := LowerCase(MethodName);
  for I := 0 to High(Methods) do
    if LowerCase(Methods[I].Name) = LowerName then
      Exit(Methods[I].MethodID);
  raise ESBWNotFound.CreateFmt('Method not found: %s', [MethodName]);
end;

class function TSBWHelpers.RequireMethodId(Module: TSBWModuleImpl;
  const ModuleName, ServiceName, MethodName: string): SBWMethodID;
var
  ModuleID: SBWModuleID;
  ServiceID: SBWServiceID;
begin
  ModuleID := RequireModuleId(Module, ModuleName);
  ServiceID := RequireServiceId(Module, ModuleID, ServiceName);
  Result := RequireMethodId(Module, ModuleID, ServiceID, MethodName);
end;

//=============================================================================
// Descriptor Helpers
//=============================================================================

class function TSBWHelpers.UnpackModuleDescriptor(
  const Descriptor: TSBWList): TSBWModuleDescriptorRec;
begin
  Result := Default(TSBWModuleDescriptorRec);
  if Descriptor.Count >= 1 then Result.Name := Descriptor[0].GetString;
  if Descriptor.Count >= 2 then Result.DisplayName := Descriptor[1].GetString;
  if Descriptor.Count >= 3 then Result.ManagementType := Descriptor[2].GetInteger;
  if Descriptor.Count >= 4 then Result.CommandLine := Descriptor[3].GetString;
  if Descriptor.Count >= 5 then Result.Help := Descriptor[4].GetString;
end;

class function TSBWHelpers.UnpackServiceDescriptor(
  const Descriptor: TSBWList): TSBWServiceDescriptorRec;
begin
  Result := Default(TSBWServiceDescriptorRec);
  if Descriptor.Count >= 1 then Result.ModuleName := Descriptor[0].GetString;
  if Descriptor.Count >= 2 then Result.ServiceName := Descriptor[1].GetString;
  if Descriptor.Count >= 3 then Result.DisplayName := Descriptor[2].GetString;
  if Descriptor.Count >= 4 then Result.Category := Descriptor[3].GetString;
  if Descriptor.Count >= 5 then Result.Help := Descriptor[4].GetString;
end;

class function TSBWHelpers.GetModuleDescriptorRec(Module: TSBWModuleImpl;
  const ModuleName: string; IncludeRunning: Boolean): TSBWModuleDescriptorRec;
var
  Desc: TSBWList;
begin
  Desc := Module.GetModuleDescriptor(ModuleName, IncludeRunning);
  try
    Result := UnpackModuleDescriptor(Desc);
  finally
    Desc.Free;
  end;
end;

class function TSBWHelpers.GetModuleDescriptorRec(Module: TSBWModuleImpl;
  ModuleID: SBWModuleID): TSBWModuleDescriptorRec;
var
  Desc: TSBWList;
begin
  Desc := Module.GetModuleDescriptor(ModuleID);
  try
    Result := UnpackModuleDescriptor(Desc);
  finally
    Desc.Free;
  end;
end;

class function TSBWHelpers.GetModuleDescriptorRecs(Module: TSBWModuleImpl;
  LocalOnly, IncludeRunning: Boolean): TArray<TSBWModuleDescriptorRec>;
var
  Descs: TArray<TSBWList>;
  I: Integer;
begin
  Descs := Module.GetModuleDescriptors(LocalOnly, IncludeRunning);
  try
    SetLength(Result, Length(Descs));
    for I := 0 to High(Descs) do
      Result[I] := UnpackModuleDescriptor(Descs[I]);
  finally
    for I := 0 to High(Descs) do
      Descs[I].Free;
  end;
end;

class function TSBWHelpers.GetServiceDescriptorRec(Module: TSBWModuleImpl;
  ModuleID: SBWModuleID; const ServiceName: string): TSBWServiceDescriptorRec;
var
  Desc: TSBWList;
begin
  Desc := Module.GetServiceDescriptor(ModuleID, ServiceName);
  try
    Result := UnpackServiceDescriptor(Desc);
  finally
    Desc.Free;
  end;
end;

class function TSBWHelpers.GetServiceDescriptorRec(Module: TSBWModuleImpl;
  ModuleID: SBWModuleID; ServiceID: SBWServiceID): TSBWServiceDescriptorRec;
var
  Desc: TSBWList;
begin
  Desc := Module.GetServiceDescriptor(ModuleID, ServiceID);
  try
    Result := UnpackServiceDescriptor(Desc);
  finally
    Desc.Free;
  end;
end;

class function TSBWHelpers.GetServiceDescriptorRecs(Module: TSBWModuleImpl;
  ModuleID: SBWModuleID): TArray<TSBWServiceDescriptorRec>;
var
  Descs: TArray<TSBWList>;
  I: Integer;
begin
  Descs := Module.GetServiceDescriptors(ModuleID);
  try
    SetLength(Result, Length(Descs));
    for I := 0 to High(Descs) do
      Result[I] := UnpackServiceDescriptor(Descs[I]);
  finally
    for I := 0 to High(Descs) do
      Descs[I].Free;
  end;
end;

//=============================================================================
// Signature Parsing Helpers
//=============================================================================

class function TSBWHelpers.ExtractMethodName(const Signature: string): string;
var
  OpenParen, SpacePos: Integer;
begin
  // Signature format: "returnType methodName(args)"
  // Find the opening parenthesis
  OpenParen := Pos('(', Signature);
  if OpenParen = 0 then
    Exit(Signature);

  // Get everything before the parenthesis
  Result := Trim(Copy(Signature, 1, OpenParen - 1));

  // Find the last space (separating return type from method name)
  SpacePos := LastDelimiter(' ', Result);
  if SpacePos > 0 then
    Result := Copy(Result, SpacePos + 1, Length(Result));
end;

class function TSBWHelpers.ExtractReturnType(const Signature: string): string;
var
  OpenParen, SpacePos: Integer;
  BeforeParen: string;
begin
  // Signature format: "returnType methodName(args)"
  OpenParen := Pos('(', Signature);
  if OpenParen = 0 then
    Exit('');

  BeforeParen := Trim(Copy(Signature, 1, OpenParen - 1));
  SpacePos := LastDelimiter(' ', BeforeParen);
  if SpacePos > 0 then
    Result := Trim(Copy(BeforeParen, 1, SpacePos - 1))
  else
    Result := '';
end;

//=============================================================================
// Simplified Call Helpers
//=============================================================================

class function TSBWHelpers.CallDoubleToDouble(Module: TSBWModuleImpl;
  ModuleID: SBWModuleID; ServiceID: SBWServiceID; MethodID: SBWMethodID;
  Arg: Double): Double;
var
  Args: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
begin
  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteDouble(Arg);
    Reader := Module.CallMethod(ModuleID, ServiceID, MethodID, Args);
    try
      Result := Reader.ReadDouble;
    finally
      Reader.Free;
    end;
  finally
    Args.Free;
  end;
end;

class function TSBWHelpers.CallDoubleToDouble(Module: TSBWModuleImpl;
  const ModuleName, ServiceName, MethodName: string; Arg: Double): Double;
var
  ModuleID: SBWModuleID;
  ServiceID: SBWServiceID;
  MethodID: SBWMethodID;
begin
  ModuleID := RequireModuleId(Module, ModuleName);
  ServiceID := RequireServiceId(Module, ModuleID, ServiceName);
  MethodID := RequireMethodId(Module, ModuleID, ServiceID, MethodName);
  Result := CallDoubleToDouble(Module, ModuleID, ServiceID, MethodID, Arg);
end;

class function TSBWHelpers.CallDoubleDoubleToDouble(Module: TSBWModuleImpl;
  ModuleID: SBWModuleID; ServiceID: SBWServiceID; MethodID: SBWMethodID;
  Arg1, Arg2: Double): Double;
var
  Args: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
begin
  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteDouble(Arg1);
    Args.WriteDouble(Arg2);
    Reader := Module.CallMethod(ModuleID, ServiceID, MethodID, Args);
    try
      Result := Reader.ReadDouble;
    finally
      Reader.Free;
    end;
  finally
    Args.Free;
  end;
end;

class function TSBWHelpers.CallDoubleDoubleToDouble(Module: TSBWModuleImpl;
  const ModuleName, ServiceName, MethodName: string; Arg1, Arg2: Double): Double;
var
  ModuleID: SBWModuleID;
  ServiceID: SBWServiceID;
  MethodID: SBWMethodID;
begin
  ModuleID := RequireModuleId(Module, ModuleName);
  ServiceID := RequireServiceId(Module, ModuleID, ServiceName);
  MethodID := RequireMethodId(Module, ModuleID, ServiceID, MethodName);
  Result := CallDoubleDoubleToDouble(Module, ModuleID, ServiceID, MethodID, Arg1, Arg2);
end;

class function TSBWHelpers.CallIntArrayToInt(Module: TSBWModuleImpl;
  ModuleID: SBWModuleID; ServiceID: SBWServiceID; MethodID: SBWMethodID;
  const Args: TArray<Integer>): Integer;
var
  ArgsWriter: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
begin
  ArgsWriter := TSBWDataBlockWriter.Create;
  try
    ArgsWriter.WriteIntegerArray(Args);
    Reader := Module.CallMethod(ModuleID, ServiceID, MethodID, ArgsWriter);
    try
      Result := Reader.ReadInteger;
    finally
      Reader.Free;
    end;
  finally
    ArgsWriter.Free;
  end;
end;

class function TSBWHelpers.CallIntArrayToInt(Module: TSBWModuleImpl;
  const ModuleName, ServiceName, MethodName: string;
  const Args: TArray<Integer>): Integer;
var
  ModuleID: SBWModuleID;
  ServiceID: SBWServiceID;
  MethodID: SBWMethodID;
begin
  ModuleID := RequireModuleId(Module, ModuleName);
  ServiceID := RequireServiceId(Module, ModuleID, ServiceName);
  MethodID := RequireMethodId(Module, ModuleID, ServiceID, MethodName);
  Result := CallIntArrayToInt(Module, ModuleID, ServiceID, MethodID, Args);
end;

class function TSBWHelpers.CallStringToString(Module: TSBWModuleImpl;
  ModuleID: SBWModuleID; ServiceID: SBWServiceID; MethodID: SBWMethodID;
  const Arg: string): string;
var
  Args: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
begin
  Args := TSBWDataBlockWriter.Create;
  try
    Args.WriteString(Arg);
    Reader := Module.CallMethod(ModuleID, ServiceID, MethodID, Args);
    try
      Result := Reader.ReadString;
    finally
      Reader.Free;
    end;
  finally
    Args.Free;
  end;
end;

class function TSBWHelpers.CallStringToString(Module: TSBWModuleImpl;
  const ModuleName, ServiceName, MethodName: string; const Arg: string): string;
var
  ModuleID: SBWModuleID;
  ServiceID: SBWServiceID;
  MethodID: SBWMethodID;
begin
  ModuleID := RequireModuleId(Module, ModuleName);
  ServiceID := RequireServiceId(Module, ModuleID, ServiceName);
  MethodID := RequireMethodId(Module, ModuleID, ServiceID, MethodName);
  Result := CallStringToString(Module, ModuleID, ServiceID, MethodID, Arg);
end;

class function TSBWHelpers.CallVoidToString(Module: TSBWModuleImpl;
  ModuleID: SBWModuleID; ServiceID: SBWServiceID; MethodID: SBWMethodID): string;
var
  Args: TSBWDataBlockWriter;
  Reader: TSBWDataBlockReader;
begin
  Args := TSBWDataBlockWriter.Create;
  try
    Reader := Module.CallMethod(ModuleID, ServiceID, MethodID, Args);
    try
      Result := Reader.ReadString;
    finally
      Reader.Free;
    end;
  finally
    Args.Free;
  end;
end;

class function TSBWHelpers.CallVoidToString(Module: TSBWModuleImpl;
  const ModuleName, ServiceName, MethodName: string): string;
var
  ModuleID: SBWModuleID;
  ServiceID: SBWServiceID;
  MethodID: SBWMethodID;
begin
  ModuleID := RequireModuleId(Module, ModuleName);
  ServiceID := RequireServiceId(Module, ModuleID, ServiceName);
  MethodID := RequireMethodId(Module, ModuleID, ServiceID, MethodName);
  Result := CallVoidToString(Module, ModuleID, ServiceID, MethodID);
end;

end.
