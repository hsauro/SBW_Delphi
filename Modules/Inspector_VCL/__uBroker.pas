unit uBroker;

interface

Uses SysUtils, uSBWD, uSBWCallObject, uSBWUtils, uSBWCommon;

const
  getVersionId = 0;
  registerModuleId = 13;
  changeModuleNameId = 14;
  registerServiceId = 15;
  unregisterModuleId = 16;
  shutdownBrokerId = 17;

type
  TBroker = class (TObject)

     sbw : TObject;
     ds : TSBWDataStream;

     function  getVersion : string;
     procedure registerModule (moduleName : string; moduleDisplayName : string; moduleType : integer; commandLine : string; helpString : string);
     procedure changeModuleName (moduleId : integer; newModuleName : string);
     procedure registerService (moduleName : string; serviceName : string; serviceDisplayName : string; category : string; helpString : string);
     procedure unregisterModule(moduleName : string);
     procedure shutdownBroker;

     constructor Create (sbw : TObject);
     destructor  Destroy; override;
  end;


implementation


Uses uTSBW;

constructor TBroker.Create (sbw : TObject);
begin
  inherited Create;
  self.sbw := sbw;
  ds := TSBWDataStream.Create;
end;


destructor TBroker.Destroy;
begin
  ds.Free;
  inherited Destroy;
end;


function TBroker.getVersion : string;
var msg : PChar; l : integer;
begin
  msg := (sbw as TSBW).Call ((sbw as TSBW).BrokerId, SystemMethodsId, getVersionId, l);
  ds.setMsg(msg, l);
  result := ds.getString;
end;


procedure TBroker.registerModule (moduleName : string; moduleDisplayName : string; moduleType : integer; commandLine : string; helpString : string);
var msg : PChar; l : integer;
begin
  msg := (sbw as TSBW).Call ((sbw as TSBW).BrokerId, BrokerServiceId, registerModuleId, [moduleName, moduleDisplayName, moduleType, commandLine, helpString], l);
end;


procedure TBroker.changeModuleName (moduleId : integer; newModuleName : string);
var msg : PChar; l : integer;
begin
  msg := (sbw as TSBW).Call ((sbw as TSBW).BrokerId, BrokerServiceId, changeModuleNameId, [moduleId, newModuleName], l);
end;


procedure TBroker.registerService (moduleName : string; serviceName : string; serviceDisplayName : string; category : string; helpString : string);
var msg : PChar; l : integer;
begin
  msg := (sbw as TSBW).Call ((sbw as TSBW).BrokerId, BrokerServiceId, changeModuleNameId,
             [moduleName, serviceName, serviceDisplayName, category, helpString], l);
end;


procedure TBroker.unregisterModule(moduleName : string);
var msg : PChar; l : integer;
begin
  msg := (sbw as TSBW).Call ((sbw as TSBW).BrokerId, BrokerServiceId, changeModuleNameId, [moduleName], l);
end;


procedure TBroker.shutdownBroker;
var msg : PChar; l : integer;
begin
  msg := (sbw as TSBW).Call ((sbw as TSBW).BrokerId, BrokerServiceId, changeModuleNameId, l);
end;



end.
