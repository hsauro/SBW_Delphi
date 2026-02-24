unit ufMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  SBW.Module,
  SBW.Types,
  SBW.Connection,
  SBW.List, FMX.TreeView, FMX.Layouts, FMX.ListBox, FMX.TabControl, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView;

type
  TfrmMain = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    TabControl1: TTabControl;
    tbRunningModules: TTabItem;
    lbModuleList: TListBox;
    Layout4: TLayout;
    lbMethods: TListBox;
    lbDetails: TListBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbModuleListClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Client: TSBWModuleImpl;
    Host: string;
    Port: Word;
    Modules : TSBWList;
    Services: TSBWList;
    Methods: TSBWList;
    Item: TSBWList;
    ModuleId: SBWInteger;
    ModuleName: string;
    ServiceId: SBWServiceID;
    ServiceName: string;
    MethodId: SBWMethodID;
    Signature: string;
    CurrentModuleName : String;
    //procedure DisplayTree (index : integer);
    procedure BuildMethodsList;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

function GetServiceNames (Client : TSBWModuleImpl; ModuleId : Integer) : TArray<string>;
var  ServiceList : TSBWList;
     i, SBWModuleId : Integer;
     InnerList : TSBWList;
begin
  ServiceList := Client.GetServiceIds(ModuleId);
  SetLength (result, ServiceList.Count);
  for i := 0 to ServiceList.Count - 1 do
         begin
         InnerList := ServiceList[i].GetList;
         result[i] := InnerList[1].GetString;
         end;
end;

function GetMethodNames (Client : TSBWModuleImpl; ModuleName, ServiceName : String) : TArray<string>;
var  MethodList : TSBWList;
     i, SBWModuleId : Integer;
     InnerList : TSBWList;
begin
  MethodList := Client.GetMethodIds(ModuleName, ServiceName);
  SetLength (result, MethodList.Count);
  for i := 0 to MethodList.Count - 1 do
         begin
         InnerList := MethodList[i].GetList;
         result[i] := InnerList[1].GetString;
         end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Client.Disconnect;
  Client.Free;
  FinalizeSockets;
end;

procedure TfrmMain.lbModuleListClick(Sender: TObject);
begin
  lbMethods.Items.Clear;
  CurrentModuleName := lbModuleList.Items[lbModuleList.ItemIndex];
  //BtnDoc.enabled := True;
  BuildMethodsList
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var i  : integer;
begin
  Host := '127.0.0.1';
  Port := SBW_DEFAULT_PORT;
  InitializeSockets;

  Client := TSBWModuleImpl.Create(
  'edu.demo.inspector',
        'Inspector',
        mmtSelfManaged);
   Client.Connect(Host, Port);

   Modules := Client.GetListOfModules;
   for i := 0 to Modules.Count - 1 do
       begin
       Item := Modules[i].GetList;
       lbModuleList.Items.Add(Item[1].GetString);
       end;
end;


procedure TfrmMain.BuildMethodsList;
var i, j : integer; ss, ms : TStringList;
    MyTreeNode : TTreeViewItem;
    moduleName, sigStr : string;
    //MethodRecord : TMethodRecord;
    //ServiceRecord : TServiceRecord;
    ServiceDescriptor : TArray<TSBWList>;
    ModuleId : integer;
    methodName : string;
    sp_index : integer;
    bt_index : integer;
    serviceName : string;
    index : integer;
    SBWModuleID : Integer;
    ServiceList : TArray<String>;
    MethodsList : TArray<String>;
    InnerList : TSBWList;
begin
  try
    try
     lbMethods.Items.Clear;
     SBWModuleId := Client.GetModuleIdByName(CurrentModuleName);
     ServiceList := GetServiceNames (Client, SBWModuleId);
     for i := 0 to length (ServiceList) - 1 do
         begin
         lbMethods.Items.Add (ServiceList[i]);
         MethodsList :=  GetMethodNames(Client, CurrentModuleName, ServiceList[i]);
         for j := 0 to length (MethodsList) - 1 do
             begin
             lbMethods.Items.Add ('   ' + MethodsList[j]);
             end;
          end;



//     ss := sbw.getServiceList (moduleId);
//     with TreeView.Items do
//          begin
//          Clear; // remove any existing nodes
//
//          for i := 0 to ss.Count - 1 do
//              begin
//              ms := sbw.getMethodList (moduleId, i);  // ServiceIds increase from zero
//              MyTreeNode := Add(nil, ss[i]); // Add a root node
//              ServiceRecord := TServiceRecord.Create;
//              ServiceRecord.ModuleId := moduleId;
//              ServiceRecord.ServiceId := i;
//              PServiceDescriptor := SBWServiceGetDescriptor (moduleId, i);
//              ServiceRecord.helpStr := PServiceDescriptor^.helpStr;
//              ServiceRecord.name := PServiceDescriptor^.serviceName;
//              MyTreeNode.Data := ServiceRecord;
//              SBWFreeServiceDescriptor (PServiceDescriptor);
//
//              // Add children nodes to the node just added
//              serviceName := ServiceRecord.name;
//              for j := 0 to ms.Count - 1 do
//                  begin
//                  MethodRecord := TMethodRecord.Create;
//                  MethodRecord.ModuleId := ModuleId;
//                  MethodRecord.helpStr := sbw.getHelpStr (ModuleId, i, j);
//                  MethodRecord.ServiceId := i;
//                  MethodRecord.MethodId := j;
//                  MethodRecord.moduleName := moduleName;
//                  MethodRecord.serviceName := serviceName;
//                  MethodRecord.methodName := ms[j];
//                  sigStr := sbw.getMethodSignatureString (ModuleId, i, j);
//                  AddChildObject (MyTreeNode, ms[j] + ' : ' + sigStr, MethodRecord);
//                  end;
//              ms.Free;
//              end;
//          end;
//     ss.Free;
  finally
     //Screen.Cursor := crDefault;
  end;
  except
    //on e: ESBWException do
     //  begin
    //   if e.errorCode <> integer (ModuleNotFoundExceptionCode) then
   //       showmessage (e.Message);
    //   end;
  end;
end;

end.
