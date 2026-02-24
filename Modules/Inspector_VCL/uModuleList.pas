unit uModuleList;

interface

uses SysUtils, Classes, StrUtils, Contnrs, uSBWCommon, uSBWD, uTSBW;

type
  TMethodObject = class (TObject)
          ModuleId : integer;
          ServiceId : integer;
          MethodId : integer;
          helpStr : string;

          moduleName : string;
          serviceName : string;
          methodName : string;
          sigStr : string;
  end;
  
  TServiceObject = class (TObject)
     ServiceRecord : TServiceRecord;
     methodList : TStringList;
     function getMethodObject (index : integer) : TMethodObject;
     destructor Destroy; override;
  end;


  TModuleItem = class (TObject)
     name : string;
     Id : integer;
     ss : TStringList;
     function getServiceObject (index : integer) : TServiceObject;
     constructor Create (name : string; Id : integer);
     destructor  Destroy; override;
  end;


  TModuleList = class (TObjectList)
        sbwRef : TSBW;
        function  getModuleItem (index : integer) : TModuleItem;
        procedure setModuleItem (index : integer; moduleItem : TModuleItem);
        function  find (name : string; var index : integer) : boolean;
        function  add (moduleName : string; moduleId : integer) : integer; overload;
        procedure deleteModule (Id : integer);
        property  moduleItems[Index: Integer]: TModuleItem read getModuleItem write setModuleItem; default;

        constructor Create (sbw : TSBW);   
  end;


implementation


function TServiceObject.getMethodObject (index : integer) : TMethodObject;
begin
  result := TMethodObject (methodList.Objects[index]);
end;


destructor TServiceObject.Destroy;
var mo : TMethodObject;
    i : integer;
begin
  ServiceRecord.Free;
  for i := 0 to methodList.Count - 1 do
      begin
      mo := getMethodObject (i);
      mo.Free;
      end;
  methodList.Free;
  inherited Destroy;
end;


// --------------------------------------------------------------------------


constructor TModuleItem.Create (name : string; Id : integer);
begin
  inherited Create;
  self.name := name;
  self.Id := Id;
  ss := TStringList.Create;
end;


destructor TModuleItem.Destroy;
var so : TServiceObject;
    i : integer;
begin
  for i := 0 to ss.Count - 1 do
      begin
      so := getServiceObject(i);
      so.Free;
      end;

  ss.Free;
  inherited Destroy;
end;


function TModuleItem.getServiceObject (index : integer) : TServiceObject;
begin
  result := TServiceObject (ss.Objects[Index]);
end;


// --------------------------------------------------------------------------

constructor TModuleList.Create (sbw : TSBW);
begin
  inherited Create;
  sbwRef := sbw;
end;


procedure TModuleList.setModuleItem (index : integer; moduleItem : TModuleItem);
begin
  items[Index] := moduleItem;
end;


function TModuleList.getModuleItem (index : integer) : TModuleItem;
begin
  result := TModuleItem (items[index]);
end;


function TModuleList.find (name : string; var index : integer) : boolean;
var i : integer;
begin
  result := False;
  for i := 0 to count - 1 do
      if getModuleItem (i).name = name then
         begin
         result := true;
         Index := i;
         exit;
         end;
end;

function TModuleList.add (moduleName : string; moduleId : integer) : integer;
var mi : TModuleItem;
    i, j : integer;
    MethodRecord : TMethodRecord;
    ServiceRecord : TServiceRecord;
    PServiceDescriptor : PSBWServiceDescriptor;
    so : TServiceObject;
    mo : TMethodObject;

begin
  mi := TModuleItem.Create (moduleName, moduleId);
  mi.ss := sbwRef.getServiceList (moduleId);

  for i := 0 to mi.ss.Count - 1 do
      begin
      so := TServiceObject.Create;
      so.methodList := sbwRef.getMethodList (moduleId, i);  // ServiceIds increase from zero

      ServiceRecord := TServiceRecord.Create;
      ServiceRecord.ModuleId := moduleId;
      ServiceRecord.ServiceId := i;
      PServiceDescriptor := SBWServiceGetDescriptor (moduleId, i);
      ServiceRecord.name := PServiceDescriptor.serviceName;
      ServiceRecord.helpStr := PServiceDescriptor^.helpStr;

      so.ServiceRecord := ServiceRecord;

      SBWFreeServiceDescriptor (PServiceDescriptor);
      mi.ss.Objects[i] := so;

      // Add children nodes to the node just added
      for j := 0 to so.methodList.Count - 1 do
          begin
          mo := TMethodObject.Create;
          mo.ModuleId := ModuleId;
          mo.helpStr := sbwRef.getHelpStr (ModuleId, i, j);
          mo.ServiceId := i;
          mo.MethodId := j;
          mo.moduleName := moduleName;
          mo.serviceName := so.ServiceRecord.name;
          mo.methodName := so.methodList[j];
          mo.sigStr := sbwRef.getMethodSignatureString (ModuleId, i, j);
          so.methodList.Objects[j] := mo;
          end;
     end;
  result := self.add (TObject (mi));
end;


procedure TModuleList.deleteModule (Id : integer);
var i : integer; mi : TModuleItem;
begin
  for i := 0 to Count - 1 do
      begin
      mi := getModuleItem (i);
      if Id = mi.Id then
         begin
         Delete (i);
         exit;
         end;
      end;
end;


end.
