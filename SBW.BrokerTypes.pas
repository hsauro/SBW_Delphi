unit SBW.BrokerTypes;

(******************************************************************************
 * SBW.BrokerTypes.pas
 *
 * Type definitions for the SBW Broker.
 *
 * Contains:
 * - TSBWBrokerMethodInfo: Method metadata
 * - TSBWBrokerServiceInfo: Service metadata with methods
 * - TSBWModuleInfo: Connected module with services
 *****************************************************************************)

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  SBW.Types, SBW.Connection;

type
  { Forward declaration for circular references within this unit }
  TSBWBrokerServiceInfo = class;

  /// <summary>
  /// Information about a method registered with the broker
  /// </summary>
  TSBWBrokerMethodInfo = class
  private
    FMethodID: SBWMethodID;
    FName: string;
    FSignature: string;
    FHelp: string;
  public
    property MethodID: SBWMethodID read FMethodID write FMethodID;
    property Name: string read FName write FName;
    property Signature: string read FSignature write FSignature;
    property Help: string read FHelp write FHelp;
  end;

  /// <summary>
  /// Information about a service registered with the broker
  /// </summary>
  TSBWBrokerServiceInfo = class
  private
    FServiceID: SBWServiceID;
    FName: string;
    FDisplayName: string;
    FCategory: string;
    FHelp: string;
    FMethods: TObjectList<TSBWBrokerMethodInfo>;
  public
    constructor Create;
    destructor Destroy; override;
    function FindMethodByID(MethodID: SBWMethodID): TSBWBrokerMethodInfo;
    function FindMethodByName(const MethodName: string): TSBWBrokerMethodInfo;

    property ServiceID: SBWServiceID read FServiceID write FServiceID;
    property Name: string read FName write FName;
    property DisplayName: string read FDisplayName write FDisplayName;
    property Category: string read FCategory write FCategory;
    property Help: string read FHelp write FHelp;
    property Methods: TObjectList<TSBWBrokerMethodInfo> read FMethods;
  end;

  /// <summary>
  /// Information about a module instance connected to the broker
  /// </summary>
  TSBWModuleInfo = class
  private
    FModuleID: SBWModuleID;
    FModuleName: string;
    FDisplayName: string;
    FModuleType: Integer;
    FCommandLine: string;
    FHelp: string;
    FConnection: TSBWClientConnection;
    FServices: TObjectList<TSBWBrokerServiceInfo>;
  public
    constructor Create(AModuleID: SBWModuleID; const AModuleName: string;
      AConnection: TSBWClientConnection);
    destructor Destroy; override;

    function FindServiceByID(ServiceID: SBWServiceID): TSBWBrokerServiceInfo;
    function FindServiceByName(const ServiceName: string): TSBWBrokerServiceInfo;

    property ModuleID: SBWModuleID read FModuleID write FModuleID;
    property ModuleName: string read FModuleName write FModuleName;
    property DisplayName: string read FDisplayName write FDisplayName;
    property ModuleType: Integer read FModuleType write FModuleType;
    property CommandLine: string read FCommandLine write FCommandLine;
    property Help: string read FHelp write FHelp;
    property Connection: TSBWClientConnection read FConnection;
    property Services: TObjectList<TSBWBrokerServiceInfo> read FServices;
  end;

  // Events used by the Broker
  TOnModuleEvent = procedure(ModuleID: SBWModuleID) of object;
  TOnLogEvent = procedure(const Msg: string) of object;

implementation

{ TSBWBrokerServiceInfo }

constructor TSBWBrokerServiceInfo.Create;
begin
  inherited Create;
  FMethods := TObjectList<TSBWBrokerMethodInfo>.Create(True);
end;

destructor TSBWBrokerServiceInfo.Destroy;
begin
  FMethods.Free;
  inherited;
end;

function TSBWBrokerServiceInfo.FindMethodByID(MethodID: SBWMethodID): TSBWBrokerMethodInfo;
var
  Meth: TSBWBrokerMethodInfo;
begin
  for Meth in FMethods do
    if Meth.MethodID = MethodID then
      Exit(Meth);
  Result := nil;
end;

function TSBWBrokerServiceInfo.FindMethodByName(const MethodName: string): TSBWBrokerMethodInfo;
var
  Meth: TSBWBrokerMethodInfo;
begin
  for Meth in FMethods do
    if SameText(Meth.Name, MethodName) then
      Exit(Meth);
  Result := nil;
end;

{ TSBWModuleInfo }

constructor TSBWModuleInfo.Create(AModuleID: SBWModuleID; const AModuleName: string;
  AConnection: TSBWClientConnection);
begin
  inherited Create;
  FModuleID := AModuleID;
  FModuleName := AModuleName;
  FDisplayName := AModuleName;
  FConnection := AConnection;
  FServices := TObjectList<TSBWBrokerServiceInfo>.Create(True);
end;

destructor TSBWModuleInfo.Destroy;
begin
  FServices.Free;
  inherited;
end;

function TSBWModuleInfo.FindServiceByID(ServiceID: SBWServiceID): TSBWBrokerServiceInfo;
var
  Svc: TSBWBrokerServiceInfo;
begin
  for Svc in FServices do
    if Svc.ServiceID = ServiceID then
      Exit(Svc);
  Result := nil;
end;

function TSBWModuleInfo.FindServiceByName(const ServiceName: string): TSBWBrokerServiceInfo;
var
  Svc: TSBWBrokerServiceInfo;
  LowerName: string;
begin
  LowerName := LowerCase(ServiceName);
  for Svc in FServices do
    if LowerCase(Svc.Name) = LowerName then
      Exit(Svc);
  Result := nil;
end;

end.
