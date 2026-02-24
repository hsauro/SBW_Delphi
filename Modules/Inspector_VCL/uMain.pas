unit uMain;

// Inspector Tool for SBW

// Should compile under Linux/Kylix

interface

uses 
SysUtils,
Classes,
StrUtils,
Contnrs,
Windows,
Messages,
Forms,
Controls,
Dialogs,
Graphics,
StdCtrls,
extctrls,
ComCtrls,
shellapi,
Menus,
Clipbrd,
uSBWD,
uTSBW,
uSBWCommon,
uSBWCallObject,
uSBWList,
uSBWArray,
syncobjs,
uRegEdit,
uSBWUtils,
Buttons,
uModuleList,
ImgList,
XPMan,
System.ImageList,
FMX.Controls, FMX.Types, FMX.Controls.Presentation;

const
   VERSION = 0.87;
   LIST_MODULES = 1;
   SHOW_MESSAGE = 2;
   TERMINATE_APPLICATION = 3;
   SHUT_DOWN_MESSAGE = 4; 

type
  TCurrentlySelectedMethod = record
     moduleId, serviceId, methodId : integer;
  end;

  TTranslatorPlugin = class (TObject)
                 pluginName : string;
                 id : integer;
                 constructor Create (name : string; id : integer);
  end;

  TfrmMain = class(TForm)
    titlePanel: TPanel;
    btnPanel: TPanel;
    StatusBar: TStatusBar;
    Label1: TLabel;
    BtnClose: TButton;
    Label5: TLabel;
    PopupMenu: TPopupMenu;
    mnuRunModule: TMenuItem;
    mnuModulePopup: TPopupMenu;
    mnuRunMethod: TMenuItem;
    btnStopModule: TButton;
    btnStartModule: TButton;
    mnuPopupCopy: TMenuItem;
    btnRunMethod: TButton;
    registerPopup: TPopupMenu;
    mnuRegisterModule: TMenuItem;
    mnuUnregisterModule: TMenuItem;
    Button1: TButton;
    N1: TMenuItem;
    mnuShutdownModule: TMenuItem;
    N2: TMenuItem;
    mnuCopyModule: TMenuItem;
    N3: TMenuItem;
    mnuUnregisterModule_RegTab: TMenuItem;
    ImageList1: TImageList;
    XPManifest1: TXPManifest;
    procedure BtnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbwFailedToConnect(Sender: TObject; msg: String);
    procedure lbModuleListClick(Sender: TObject);
    procedure TreeViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure sbwModuleShutDown(Sender: TObject; Id: Integer);
    procedure TreeViewRegContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure mnuRunModuleClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure sbwRegister(Sender: TObject);
    procedure BtnDocClick(Sender: TObject);
    procedure sbwModuleStartup(Sender: TObject; name: AnsiString;
      Id: Integer);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure btnStopModuleClick(Sender: TObject);
    procedure btnStartModuleClick(Sender: TObject);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuPopupCopyClick(Sender: TObject);
    procedure sbwShutDown(Sender: TObject);
    procedure mnuRegisterModuleClick(Sender: TObject);
    procedure mnuUnregisterModuleClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TreeViewRegMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuShutdownModuleClick(Sender: TObject);
    procedure TreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sbwRegistrationChange(Sender: TObject);
    procedure mnuCopyModuleClick(Sender: TObject);
    procedure lbModuleListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure mnuUnregisterModule_RegTabClick(Sender: TObject);
    procedure TreeViewRegChange(Sender: TObject; Node: TTreeNode);
    procedure cboBrokerChange(Sender: TObject);
    procedure mnuCopyRegisteredPopupClick(Sender: TObject);
    procedure mnuRunMethodClick(Sender: TObject);
    procedure mnuCopySubLevelClick(Sender: TObject);
    procedure lbModuleListExit(Sender: TObject);
    procedure btnRunMethodClick(Sender: TObject);
    procedure TreeViewDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeViewDeletion(Sender: TObject; Node: TTreeNode);
  private
    { Private declarations }
    q : TPoint;
    CurrentMousePosition : TPoint;
    currentlySelectedMethod : TCurrentlySelectedMethod;
    finishedGUIWork : boolean;
    procedure displayTree (index : integer);
    procedure buildTreeView;
    procedure listModules;
    procedure listRegistrySettings (brokerId : integer);
    procedure DoGUIProcess(var Msg: TMessage); message DO_GUI_PROCESS;
  public
    { Public declarations }
    MouseX, MouseY : integer;
    st : TStringList;
    showMsgVariable : string;
    moduleList : TModuleList;
    function getVersion (ds : TSBWDataStream) : SBWDataBlockWriter;
    function testInteger (ds : TSBWDataStream) : SBWDataBlockWriter;
    function testDouble (ds : TSBWDataStream) : SBWDataBlockWriter;
    function testBoolean (ds : TSBWDataStream) : SBWDataBlockWriter;
    function testString (ds : TSBWDataStream) : SBWDataBlockWriter;
    function testList (ds : TSBWDataStream) : SBWDataBlockWriter;
    function test1DArray (ds : TSBWDataStream) : SBWDataBlockWriter;
    function test2DArray (ds : TSBWDataStream) : SBWDataBlockWriter;

    function testAdd2 (ds : TSBWDataStream) : SBWDataBlockWriter;
    function testAdd3 (ds : TSBWDataStream) : SBWDataBlockWriter;
    function testAddLists (ds : TSBWDataStream) : SBWDataBlockWriter;
  end;

var
  frmMain: TfrmMain;

implementation

uses ufRunMethod, IOUtils;//, JclSysInfo;

{$R *.FMX}

var  ct : TCriticalSection;

constructor TTranslatorPlugin.Create (name : string; id : integer);
begin
  inherited Create;
  pluginName := name;
  self.id := id;
end;


// ------------------------------------------------------------------------


procedure TfrmMain.DoGUIProcess (var Msg: TMessage);
var Id : integer;
begin
   ct.Acquire;
   Id := Msg.LParam;
   case Msg.WParam of
      LIST_MODULES
        : begin
          lbModuleList.Items.Assign (st);
          end;

      SHOW_MESSAGE
        : begin
          showmessage (showMsgVariable);
          end;

      TERMINATE_APPLICATION
       : begin
         Application.Terminate;
         end;

       SHUT_DOWN_MESSAGE :
         begin
         try
          if lbModuleList.ItemIndex <> -1 then
             begin
             if Id = integer (lbModuleList.items.Objects[lbModuleList.ItemIndex]) then
                TreeView.Items.Clear;
             end;
          lblModuleId.caption := '*****';
          btnRunMethod.enabled := False;
          btnDoc.enabled := False;
          detailsList.Clear;
          moduleList.deleteModule (Id);
          listModules;
         except
          on E:exception do
             begin
             showmessage ('Exception in sbwModuleShutDown, id=' + inttostr (Id) + ' Error message: ' + e.message);
             end;
         end;
         end;
      end;
   ct.Release;
   finishedGUIWork := True;
end;


procedure TfrmMain.BtnCloseClick(Sender: TObject);
begin
  Application.Terminate;
end;


procedure TfrmMain.FormShow(Sender: TObject);
var errMsg : string;
begin
  StatusBar.Panels[2].Text := ' Inspector version: ' + floattostr (VERSION);
  if sbw.connected then
     begin
     StatusBar.Panels[0].Text := ' CONNECTED';
     StatusBar.Panels[1].Text := ' Currently running broker version: ' + sbw.broker.getVersion();
     listModules;
     listRegistrySettings (-1);
     cboBroker.ItemIndex := 0;
     end
  else
     begin
     StatusBar.Panels[0].Text := ' Failed to Connect';
     end;
end;


procedure TfrmMain.sbwFailedToConnect(Sender: TObject; msg: String);
var i, j : integer;
begin
  for i := 0 to PageControl.PageCount - 1 do
      begin
      PageControl.Pages[i].enabled := False;
      // Make sure every thing looks dimmed!
      for j := 0 to PageControl.Pages[i].ControlCount - 1 do
          PageControl.Pages[i].Controls[j].enabled := False;
      end;
  Showmessage (msg);
  StatusBar.Panels[0].Text := 'FAILED TO CONNECT';
end;


procedure TfrmMain.displayTree (index : integer);
var moduleItem : TModuleItem;
    MyTreeNode : TTreeNode;
    i, j : integer;
    ms : TStringList;
    so : TServiceObject;
    mo : TMethodObject;
    MethodRecord : TMethodRecord;
begin
  moduleItem := ModuleList[index];
  with TreeView.Items do
       begin
       Clear; // remove any existing nodes

       for i := 0 to moduleItem.ss.Count - 1 do
           begin
           so := moduleItem.getServiceObject (i);
           ms := so.methodList;
           MyTreeNode := Add(nil, moduleItem.ss[i]); // Add a root node
           MyTreeNode.Data := so.ServiceRecord;

           // Add children nodes to the node just added
           for j := 0 to ms.Count - 1 do
               begin
               mo := so.getMethodObject (j);
               MethodRecord := TMethodRecord.Create;
               MethodRecord.ModuleId := mo.ModuleId;
               MethodRecord.helpStr := mo.helpStr;
               MethodRecord.ServiceId := i;
               MethodRecord.MethodId := j;
               MethodRecord.moduleName := mo.moduleName;
               MethodRecord.serviceName := mo.serviceName;
               MethodRecord.methodName := mo.methodName;
               AddChildObject (MyTreeNode, mo.methodName + ' : ' + mo.sigStr, MethodRecord);
               end;
           end;
       end;
end;


procedure TfrmMain.buildTreeView;
var i, j : integer; ss, ms : TStringList;
    MyTreeNode : TTreeNode;
    moduleName, sigStr : string;
    MethodRecord : TMethodRecord;
    ServiceRecord : TServiceRecord;
    PServiceDescriptor : PSBWServiceDescriptor;
    ModuleId : integer;
    methodName : string;
    sp_index : integer;
    bt_index : integer;
    serviceName : string;
    index : integer;
begin
  try
  try
    Screen.Cursor := crAppStart;
    if (lbModuleList.ItemIndex <> - 1) then
     begin
     moduleName := lbModuleList.items[lbModuleList.ItemIndex];
     if lbModuleList.ItemIndex = 0 then
        moduleId := -1
     else moduleId := integer (lbModuleList.items.Objects[lbModuleList.ItemIndex]);
     lblModuleId.caption := inttostr (moduleId);

     if not moduleList.find (moduleName, index) then
        index := moduleList.Add (moduleName, moduleId);
     displayTree (index);
     exit;


     ss := sbw.getServiceList (moduleId);
     with TreeView.Items do
          begin
          Clear; // remove any existing nodes

          for i := 0 to ss.Count - 1 do
              begin
              ms := sbw.getMethodList (moduleId, i);  // ServiceIds increase from zero
              MyTreeNode := Add(nil, ss[i]); // Add a root node
              ServiceRecord := TServiceRecord.Create;
              ServiceRecord.ModuleId := moduleId;
              ServiceRecord.ServiceId := i;
              PServiceDescriptor := SBWServiceGetDescriptor (moduleId, i);
              ServiceRecord.helpStr := PServiceDescriptor^.helpStr;
              ServiceRecord.name := PServiceDescriptor^.serviceName;
              MyTreeNode.Data := ServiceRecord;
              SBWFreeServiceDescriptor (PServiceDescriptor);

              // Add children nodes to the node just added
              serviceName := ServiceRecord.name;
              for j := 0 to ms.Count - 1 do
                  begin
                  MethodRecord := TMethodRecord.Create;
                  MethodRecord.ModuleId := ModuleId;
                  MethodRecord.helpStr := sbw.getHelpStr (ModuleId, i, j);
                  MethodRecord.ServiceId := i;
                  MethodRecord.MethodId := j;
                  MethodRecord.moduleName := moduleName;
                  MethodRecord.serviceName := serviceName;
                  MethodRecord.methodName := ms[j];
                  sigStr := sbw.getMethodSignatureString (ModuleId, i, j);
                  AddChildObject (MyTreeNode, ms[j] + ' : ' + sigStr, MethodRecord);
                  end;
              ms.Free;
              end;
          end;
     ss.Free;
     end;
  finally
     Screen.Cursor := crDefault;
  end;
  except
    on e: ESBWException do
       begin
       if e.errorCode <> integer (ModuleNotFoundExceptionCode) then
          showmessage (e.Message); 
       end;
  end;
end;


// User clicks on a module name in the module list box
procedure TfrmMain.lbModuleListClick(Sender: TObject);
begin
  detailsList.Clear;
  BtnDoc.enabled := True;
  buildTreeView();
end;


var
  TreeNode: TTreeNode;


procedure TfrmMain.TreeViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if TreeView.GetNodeAt(X, Y) <> TreeNode then // Are we still inside the last entered node?
  begin
    TreeNode := TreeView.GetNodeAt(X, Y); // Get the new node and store it for later use.
    Application.CancelHint;               // Clean up last shown hint.
    if (TreeNode <> nil) and (TreeNode.Data <> nil) then               // Has we moved outside a node?
       begin
       if TreeNode.Level = 0 then
          TreeView.Hint := TServiceRecord (TreeNode.Data).helpstr;
       if TreeNode.Level = 1 then
          TreeView.Hint := TMethodRecord (TreeNode.Data).helpStr; // : No - Set hint to new node text.
       end
    else
      TreeView.Hint := '';                // : Yes - Clear the hint text.
  end;
end;


// Event fired by SBW Broker
procedure TfrmMain.sbwModuleShutDown(Sender: TObject; Id: Integer);
begin
  sbw.callGUIProcess(handle, SHUT_DOWN_MESSAGE, Id);
end;


// Get all the module names and list them on the list box
procedure TfrmMain.listModules;
var i : integer;
    previsoulySelectedBrokerName : string;
begin
  // Bit of a hack, for some reason I can't free it after I've use it, presumably
  // there is some threading issue.
  //st.Free;
  st := sbw.getModuleList ();
  previsoulySelectedBrokerName := cboBroker.items[cboBroker.ItemIndex];
  // poplate broker list in registration tab
  cboBroker.items.Clear;
  for i := 0 to st.Count - 1 do
      if RightStr (st[i], 6) = 'BROKER' then
         begin
         if Length (st[i]) = 6 then
            cboBroker.AddItem (st[i], TObject (MAXINT))
         else cboBroker.AddItem (st[i], st.Objects[i]);
         end;
  cboBroker.itemIndex := cboBroker.Items.IndexOf(previsoulySelectedBrokerName);
  try
    sbw.callGUIProcess (handle, LIST_MODULES, 0);
  finally
  end;
  while (not finishedGUIWork) do
     Application.ProcessMessages;
end;


procedure TfrmMain.listRegistrySettings (brokerId : integer);
var st : TStringList;
    ss : TStringList;
    MyTreeNode : TTreeNode;
    SubMyTreeNode : TTreeNode;
    SubSubMyTreeNode : TTreeNode;
    i, j, k : integer;
    index : integer;
    md : TModuleDescriptorObj;
    sd : TStringList;
    ServiceRecordPtr : ^TServiceDescriptorRecord;
begin
  try
    st := sbw.getRegisteredModulesEx (brokerId);
  except
    on e:exception do
       showmessage ('Error in list registry settings: ' + e.message);
  end;

  st.Sorted := True;
  st.Sort;
  TreeViewReg.Items.Clear;
  try

  with TreeViewReg.Items do
       begin
       for i := 0 to st.Count - 1 do
           begin
           MyTreeNode := Add(nil, st[i]); // Add a root node

           md := st.Objects[i] as TModuleDescriptorObj;

           AddChild (MyTreeNode, 'DisplayName: ' + md.displayName);
           AddChild (MyTreeNode, 'CommandLine: ' + Copy (md.commandLine, 1, 200));
           AddChild (MyTreeNode, 'Help Str: ' + md.helpStr);
           AddChild (MyTreeNode, 'Management Type: ' + md.managementType);

           sd := sbw.getServiceDescriptor (md);

           SubMyTreeNode := AddChild (MyTreeNode, 'Services');
           for j := 0 to sd.Count - 1 do
               begin
               // Add children nodes to the node just added
               ServiceRecordPtr := AllocMem (sizeof (TServiceDescriptorRecord));
               ServiceRecordPtr^.serviceName := (sd.Objects[j] as TServiceDescriptorObj).serviceName;
               ServiceRecordPtr^.serviceDisplayName := (sd.Objects[j] as TServiceDescriptorObj).serviceDisplayName;
               ServiceRecordPtr^.serviceCategory := (sd.Objects[j] as TServiceDescriptorObj).serviceCategory;
               ServiceRecordPtr^.help := (sd.Objects[j] as TServiceDescriptorObj).help;
               SubSubMyTreeNode := AddChildObject (SubMyTreeNode, sd[j] + ' ', ServiceRecordPtr);
               AddChildObject (SubSubMyTreeNode, ServiceRecordPtr^.serviceDisplayName, nil);
               AddChildObject (SubSubMyTreeNode, 'Categrory: ' + ServiceRecordPtr^.serviceCategory, nil);
               end; 
           for k := sd.count downto 1 do TServiceDescriptorObj(sd.objects[k-1]).free;
           sd.Free;
           end;
       end;
  finally
     for i := st.count downto 1 do TModuleDescriptorObj(st.objects[i-1]).free;
     st.Free;
  end;
end;


procedure TfrmMain.TreeViewRegContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var node : TTreeNode;
begin
  q := TreeViewReg.ClientToScreen (MousePos);
  node := TreeViewReg.GetNodeAt (MousePos.x, MousePos.y);
  if node.Level = 0 then
     PopupMenu.Popup (q.x, q.Y);
  Handled := True;
end;


// Does work because SBW doesn't support changes to registry settings at runtime
procedure TfrmMain.mnuRunModuleClick(Sender: TObject);
var md : TModuleDescriptorObj;
    st : TStringList;
    i : integer;
    p : TPoint;
    node : TTreeNode;
begin
end;


procedure TfrmMain.PageControlChange(Sender: TObject);
begin
  with (Sender as TPageControl) do
       if ActivePage.Caption = 'Registered Modules' then
          begin
          //listRegistrySettings;
          btnStopModule.enabled := False;
          btnStartModule.enabled := True;
          btnRunMethod.Enabled := False;
          end
       else
          begin
          btnStopModule.enabled := True;
          btnStartModule.enabled := False;
          btnRunMethod.Enabled := True;
          end;
end;


function TfrmMain.testAdd2 (ds : TSBWDataStream) : SBWDataBlockWriter;
begin
  result := ds.pack (ds.getScalar + ds.getScalar);
end;


function TfrmMain.testAdd3 (ds : TSBWDataStream) : SBWDataBlockWriter;
begin
  result := ds.pack (ds.getScalar + ds.getScalar + ds.getScalar);
end;

function TfrmMain.testAddLists (ds : TSBWDataStream) : SBWDataBlockWriter;
var lt1, lt2 : TSBWList;
    i : integer;
begin
  lt1 := ds.getList;
  lt2 := ds.getList;
  for i := 0 to lt1.Count - 1 do
      lt1[i].d := lt1[i].d + lt2[i].d;
  result := ds.pack (lt1);
end;


function TfrmMain.testInteger (ds : TSBWDataStream) : SBWDataBlockWriter;
begin
  result := ds.pack (ds.getInteger);
end;

function TfrmMain.testDouble (ds : TSBWDataStream) : SBWDataBlockWriter;
begin
  result := ds.pack (ds.getDouble);
end;


function TfrmMain.testString (ds : TSBWDataStream) : SBWDataBlockWriter;
begin
  result := ds.pack (ds.getString);
end;


function TfrmMain.testBoolean (ds : TSBWDataStream) : SBWDataBlockWriter;
begin
  result := ds.pack (ds.getBoolean);
end;


function TfrmMain.testList (ds : TSBWDataStream) : SBWDataBlockWriter;
var lt : TSBWList;
begin
  lt := ds.getList;
  result := ds.pack (lt);
end;


function TfrmMain.test1DArray (ds : TSBWDataStream) : SBWDataBlockWriter;
var ar : TSBWArray;
begin
  ar := ds.getArray;
  case ar.nDimensions of
     1 : case ar.dType of
            dtInteger : result := ds.pack (ar.Int1);
            dtDouble : result := ds.pack (ar.Double1);
            dtString : result := ds.pack (ar.Str1);
            //dtList   : result := ds.pack (ar.List1);
         end;
     2 : raise Exception.Create ('Expecting one dimensional array but found 2D array');
 end;
end;



function TfrmMain.test2DArray (ds : TSBWDataStream) : SBWDataBlockWriter;
var ar : TSBWArray;
begin
  ar := ds.getArray;
  case ar.nDimensions of
     1 : raise Exception.Create ('Expecting one dimensional array but found 2D array');
     2 : case ar.dType of
            dtInteger : result := ds.pack (ar.Int2);
            dtDouble  : result := ds.pack (ar.Double2);
         end;
  end;
end;



function TfrmMain.getVersion (ds : TSBWDataStream) : SBWDataBlockWriter;
begin
  result := ds.pack (floattoStr (VERSION));
end;


// Just register the module and add a version method
procedure TfrmMain.sbwRegister(Sender: TObject);
begin
  sbw.addService  ('tests', '', '', 'Loopback Test Methods');
  sbw.addFunction ('tests', testList,    '{} testList({})', 'Returns the list in the argument, used for testing');
  sbw.addFunction ('tests', test1DArray, 'int[] testArrayInt1D(int[])', 'Returns the 1D int array in the argument, ''[1, 2, 3]'' used for testing');
  sbw.addFunction ('tests', test2DArray, 'int[][] testArrayInt2D(int[][])', 'Returns the 2D int array in the argument, ''[[1, 2, 3], [4, 5, 6]]'' used for testing');
  sbw.addFunction ('tests', test1DArray, 'double[] testArrayDouble1D(double[])', 'Returns the 1D double array in the argument, ''[1.2, 2.4, 3.5]'' used for testing');
  sbw.addFunction ('tests', test2DArray, 'double[][] testArrayDouble2D(double[][])', 'Returns the 2D double array in the argument, ''[[1.3, 2.3, 3.3],[4.8, 5.8, 6.1]]'' used for testing');
  sbw.addFunction ('tests', testInteger, 'string getInteger(integer)', 'Returns the integer argument');
  sbw.addFunction ('tests', testDouble,  'string getDouble(double)', 'Returns the double argument');
  sbw.addFunction ('tests', testString,  'string getString(string)', 'Returns the string argument');
  sbw.addFunction ('tests', testBoolean, 'boolean getBoolean(boolean)', 'Returns the boolean argument');
  //sbw.addFunction ('tests', test1DArray, 'string[] testArrayStr1D(string[])', 'Returns the 1D string array in the argument, ''["A", "B", "C"]'' used for testing');
  //sbw.addFunction ('tests', test2DArray, 'string[][] testArrayStr2D(string[][])', 'Returns the 2D string array in the argument, ''[["A", "B", "C"], ["A", "B", "C"]]'' used for testing');
  //sbw.addFunction ('tests', text1DListArray, '{}[] test1DListArray({}[])', '', 'used for testing');

  sbw.addService  ('math', '', '', 'Multiple arguments tests');
  sbw.addFunction ('math', testAdd2,  'double add2(double, double)', 'Returns the sum of two numbers');
  sbw.addFunction ('math', testAdd3,  'double add3(double, double, double)', 'Returns the sum of three numbers');
  sbw.addFunction ('math', testAddLists, '{} add3({}, {})', 'Returns the sum of two numbers');

  sbw.addService  ('version', '', '', 'Version Service');
  sbw.addFunction ('version', getVersion, 'string getVersion()', 'Returns the current version of the inspector as a string');
end;


procedure TfrmMain.BtnDocClick(Sender: TObject);
var i, j : integer; ss, ms : TStringList;
    MyTreeNode : TTreeNode;
    ModuleName, sigStr : string;
    ModuleId : integer;
    docFile : TextFile;
    fileName : string;
    tmpDir : string;
begin
  if (lbModuleList.ItemIndex <> - 1) then
     begin
     moduleName := lbModuleList.items[lbModuleList.ItemIndex];
     if lbModuleList.ItemIndex = 0 then
        moduleId := -1
     else moduleId := integer (lbModuleList.items.Objects[lbModuleList.ItemIndex]);
     lblModuleId.caption := inttostr (moduleId);

     fileName := StringReplace (moduleName, ':', '_', [rfReplaceAll]) + '.htm';
     try
       tmpDir := TPath.GetTempPath();

       AssignFile (docFile, tmpDir + '\' + fileName);
       rewrite (docFile);

       writeln (docFile, '<html>');
       writeln (docFile, '<head>');
       writeln (docFile, '<TITLE>SBW Interface Docmentation for ' + moduleName + '</TITLE>');
       writeln (docFile, '</head>');
       writeln (docFile, '<body>');
       writeln (docFile, '<font size="2" face="Verdana">');
       writeln (docFile, '<H1>SBW Interface Docmentation for ' + moduleName + '</H1>');

       writeln (docFile, sbw.getModuleHelpStr (moduleId));

       ss := sbw.getServiceList (ModuleId);
       writeln (docFile, '<UL>');
       for i := 0 to ss.Count - 1 do
           begin
           writeln (docFile, '<LI> Service: ' + ss[i]);
           writeln (docFile, '        <br> <BLOCKQUOTE> <FONT size=3>' + sbw.getServiceHelpStr (moduleId, i) + ' </BLOCKQUOTE>');
           writeln (docFile, '   <UL>');

           ms := sbw.getMethodList (ModuleId, i);  // ServiceIds increase from zero

           // work our way through the method list
           for j := 0 to ms.Count - 1 do
               begin
               sigStr := sbw.getMethodSignatureString (ModuleId, i, j);

               writeln (docFile, '   <LI> <FONT size=4> ' + sigStr);
               writeln (docFile, '        <br> <BLOCKQUOTE> <FONT size=3>' + sbw.getHelpStr (ModuleId, i, j) + ' </BLOCKQUOTE>');
               end;
           writeln (docFile, '   </UL>');
           ms.Free;
           end;
       writeln (docFile, '</UL>');
       ss.Free;
     finally
       writeln (docFile, '</body>');
       writeln (docFile, '</html>');
       closeFile (docFile);
     end;
     end;

{$ifndef LINUX}
     ShellExecute(handle, 'open', pchar (tmpDir + '\' + fileName), nil, nil, SW_SHOWNORMAL);
{$else}
     Showmessage ('HTML viewing not supported under Linux, see file: [' + tmpDir + '\' + fileName + ']');
{$endif}
end;

// FEvent fired by SBW Broker
procedure TfrmMain.sbwModuleStartup(Sender: TObject; name: AnsiString;
  Id: Integer);
begin
  try
    listModules;
  except
    on e:exception do
       begin
       showMsgVariable := 'Exception in sbwModuleStartDown.' + ' Error message: ' + e.message;
       sbw.callGUIProcess(handle, SHOW_MESSAGE, Id);
       end;
  end;
end;


procedure TfrmMain.TreeViewChange(Sender: TObject; Node: TTreeNode);
var moduleId, ServiceId, methodId : integer;
    ListItem: TListItem; helpStr : string;
begin
  case Node.Level of
     0 :
         begin
         btnRunMethod.enabled := False;
         detailsList.Clear;
         helpStr := TServiceRecord (Node.Data).helpStr;
         ListItem := detailsList.Items.Add;
         if length (helpStr) > 112 then
            begin
            ListItem.SubItems.Add (Copy (helpStr, 1, 112));
            ListItem := detailsList.Items.Add;
            ListItem.SubItems.Add ('');
            ListItem.SubItems.Add ('');
            ListItem.SubItems.Add(Copy (helpStr, 112, 1000));
            end
         else
            ListItem.SubItems.Add (helpStr);
         end;

     1 : begin
         detailsList.Clear;
         moduleId := TMethodRecord (Node.Data).ModuleId;
         serviceId := TMethodRecord (Node.Data).ServiceId;
         methodId := TMethodRecord (Node.Data).MethodId;
         ListItem := detailsList.Items.Add;
         ListItem.Caption := TMethodRecord (Node.Data).moduleName;
         ListItem.SubItems.Add (TMethodRecord (Node.Data).serviceName);
         ListItem.SubItems.Add (TMethodRecord (Node.Data).methodName);

         helpStr := TMethodRecord (Node.Data).helpStr;
         if length (helpStr) > 112 then
            begin
            ListItem.SubItems.Add (Copy (helpStr, 1, 112));
            ListItem := detailsList.Items.Add;
            ListItem.SubItems.Add ('');
            ListItem.SubItems.Add ('');
            ListItem.SubItems.Add(Copy (helpStr, 112, 1000));
            end
         else
            ListItem.SubItems.Add (helpStr);
         currentlySelectedMethod.moduleId := moduleId;
         currentlySelectedMethod.serviceId := serviceId;
         currentlySelectedMethod.methodId := methodId;
         btnRunMethod.enabled := True;
         end;
  else
     begin
     btnRunMethod.enabled := False;
     end;
  end;
end;


procedure TfrmMain.btnStopModuleClick(Sender: TObject);
var moduleName : string;
    moduleId : integer;
begin
  if (lbModuleList.ItemIndex <> - 1) then
     begin
     moduleName := lbModuleList.items[lbModuleList.ItemIndex];
     if lbModuleList.ItemIndex = 0 then
        moduleId := -1
     else moduleId := integer (lbModuleList.items.Objects[lbModuleList.ItemIndex]);
     if MessageDlg('Are you sure you want to shutdown this module [' + moduleName + '] ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        sbw.ShutDownModule (moduleId);
     end;
end;


procedure TfrmMain.btnStartModuleClick(Sender: TObject);
var moduleName : string;
    msg : string;
begin
  try
    if TreeViewReg.SelectionCount > 0 then
       sbw.getModuleInstance (TreeViewReg.Selected.Text)
  except
    on e: Exception do
       showmessage ('Exception in Start Menu Option ' + e.Message);
  end;
end;


procedure TfrmMain.TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var p : TPoint; node : TTreeNode;
begin
  if Button = mbRight then
     begin
     MouseX := X;
     MouseY := Y;
     node := TreeView.GetNodeAt(X, Y);
     if node <> nil then
        begin
        node.Selected := True;
        TreeViewChange(Sender, Node);
        p := TreeView.ClientToScreen (point (x, y));
        mnuModulePopup.Popup(p.X, p.Y);
        end;
     end
  else
     begin
     node := TreeView.GetNodeAt(X, Y);
     if node <> nil then
        case node.Level of
          0 : lblServiceId.Caption := inttostr (TServiceRecord (Node.Data).ServiceId);
          1 : lblMethodId.Caption := inttostr (TMethodRecord (Node.Data).MethodId);
        end;
     end;
end;


procedure TfrmMain.mnuPopupCopyClick(Sender: TObject);
var p : TPoint; node : TTreeNode;
begin
  node := TreeView.GetNodeAt(MouseX, MouseY);
  Clipboard.AsText := node.Text;
end;


procedure TfrmMain.sbwShutDown(Sender: TObject);
begin
  sbw.callGUIProcess(handle, TERMINATE_APPLICATION, 0);
end;


procedure TfrmMain.mnuRegisterModuleClick(Sender: TObject);
var moduleName : string;
    cmdLine : string;
    id : integer;
    mt : TModalResult;
    ss, sd : TStringList;
    i : integer;
begin
  if lbModuleList.ItemIndex = -1 then exit;
  
  moduleName := lbModuleList.items[lbModuleList.ItemIndex];
  id := sbw.getModuleId (moduleName);
  cmdLine := sbw.getCommandLine (Id);
  frmRegEdit := TfrmRegEdit.Create (nil);
  frmRegEdit.EdtCommandLine.Text := cmdLine;
  frmRegEdit.edtModuleName.Text := moduleName;
  frmRegEdit.EdtDisplayName.Text := sbw.getDisplayName (id);
  frmRegEdit.EdtHelpStr.Text := sbw.getModuleHelpStr(Id);
  try
    mt := frmRegEdit.ShowModal;
    if mt = mrOK then
       begin
       sbw.broker.registerModule (frmRegEdit.edtModuleName.Text, frmRegEdit.edtDisplayName.Text, frmRegEdit.managementType,
                                  frmRegEdit.edtCommandLine.Text, frmRegEDit.edtHelpStr.Text);

       ss := sbw.getServiceList (Id);
       try
         for i := 0 to ss.Count - 1 do
             begin
             sd := sbw.getServiceDescriptor (moduleName);
             sbw.broker.registerService (frmRegEdit.edtModuleName.Text, ss[i],
                                         (sd.Objects[i] as TServiceDescriptorObj).serviceDisplayName,
                                         (sd.Objects[i] as TServiceDescriptorObj).serviceCategory,
                                         (sd.Objects[i] as TServiceDescriptorObj).help);
             sd.Free;
             end;
       finally
         ss.Free;
       end;
       end;
  finally
    frmRegEdit.Free;
  end;
end;

procedure TfrmMain.mnuUnregisterModuleClick(Sender: TObject);
var moduleName : string;
begin
  if lbModuleList.ItemIndex = -1 then exit;
  
  moduleName := lbModuleList.items[lbModuleList.ItemIndex];
  sbw.broker.unregisterModule (moduleName);
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var numberOfPlugins : integer;
    i : integer;
    pluginList : TList;
    SBWServiceDescriptorArrayPtr : PSBWServiceDescriptorArray;
begin
  SBWServiceDescriptorArrayPtr := SBWFindLocalServices(PAnsiChar ('plugin'), numberOfPlugins, 1);
  pluginList := TList.Create;
  if SBWServiceDescriptorArrayPtr <> nil then
     begin
     for i := 0 to numberOfPlugins - 1 do
         pluginList.Add (TTranslatorPlugin.Create (SBWServiceDescriptorArrayPtr^[i].serviceDisplayName, i));
     end;
end;

procedure TfrmMain.TreeViewRegMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var p : TPoint; node : TTreeNode;
begin
  if Button = mbRight then
     begin
     MouseX := X;
     MouseY := Y;
     node := TreeViewReg.GetNodeAt(X, Y);
     if node <> nil then
        begin
        if node.Level = 0 then
           begin
           node.Selected := True;
           p := TreeViewReg.ClientToScreen (point (x, y));
           PopupMenu.Popup(p.X, p.Y);
           end;
        end;
     end;

  {if Button = mbRight then
     begin
     MouseX := X;
     MouseY := Y;
     node := TreeViewReg.GetNodeAt(X, Y);
     if node <> nil then
        begin
        node.Selected := True;
        TreeViewChange(Sender, Node);
        p := TreeView.ClientToScreen (point (x, y));
        mnuModulePopup.Popup(p.X, p.Y);
        end;
     end; }
end;

procedure TfrmMain.mnuShutdownModuleClick(Sender: TObject);
begin
  if lbModuleList.ItemIndex = -1 then exit;

  btnStopModuleClick(Sender);
end;

procedure TfrmMain.TreeViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var MousePos : TPoint;
begin
  if Key = VK_APPS then
     begin
     GetCursorPos(MousePos);
     mnuModulePopup.Popup (MousePos.x, MousePos.y);
     end;
end;


procedure TfrmMain.sbwRegistrationChange(Sender: TObject);
begin
  if (cboBroker.Items.Count = 1) or (cboBroker.ItemIndex = -1) then
     listRegistrySettings (-1)
  else
     listRegistrySettings (integer (cboBroker.Items.Objects[cboBroker.ItemIndex]));
end;


procedure TfrmMain.mnuCopyModuleClick(Sender: TObject);
var p : TPoint;
    index : integer;
begin
  if lbModuleList.ItemIndex = -1 then exit;

  index := lbModuleList.ItemAtPos(CurrentMousePosition, True);
  if index <> -1 then
     begin
     Clipboard.AsText := lbModuleList.items[index];
     end;
end;

procedure TfrmMain.lbModuleListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var index : integer;
    p : TPoint;
begin
  if Button = mbRight then
     begin
     MouseX := X;
     MouseY := Y;
     index := lbModuleList.ItemAtPos(point (x, y), true);
     if index <> -1 then
        begin
        lbModuleList.ItemIndex := index;
        p := lbModuleList.ClientToScreen (point (x, y));
        registerPopup.Popup(p.X, p.Y);
        end;
     end;
  CurrentMousePosition.x := x;
  CurrentMousePosition.y := y;
end;


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  if not sbw.enable() then
     MessageDlg('Unable to connect to SBW', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0)
  else
     moduleList := TModuleList.Create (sbw);
end;


procedure TfrmMain.mnuUnregisterModule_RegTabClick(Sender: TObject);
var moduleName : string;
begin
  moduleName := TreeViewReg.Selected.Text;
  sbw.broker.unregisterModule (moduleName);
end;


procedure TfrmMain.TreeViewRegChange(Sender: TObject; Node: TTreeNode);
begin
  case Node.Level of
     0 : begin
         end;
  end;
end;


procedure TfrmMain.cboBrokerChange(Sender: TObject);
begin
  try
    if (cboBroker.Items.Count = 0) or (cboBroker.ItemIndex = -1) or (integer (cboBroker.items.Objects[cboBroker.ItemIndex]) = MAXINT) then
        listRegistrySettings (-1)
    else
        listRegistrySettings (integer (cboBroker.Items.Objects[cboBroker.ItemIndex]));
  except
    on exception do
       listRegistrySettings (-1)
  end;
end;

procedure TfrmMain.mnuCopyRegisteredPopupClick(Sender: TObject);
  var p : TPoint; node : TTreeNode;
begin
  node := TreeView.GetNodeAt(MouseX, MouseY);
  Clipboard.AsText := node.Text;
end;

procedure TfrmMain.mnuRunMethodClick(Sender: TObject);
var str : string;
    args : string;
    i : integer;
    moduleIndex : integer;
    serviceObject : TServiceObject;
    methodObject : TMethodObject;
begin
  moduleIndex := -2;
  for i := 0 to moduleList.Count - 1 do
      if moduleList[i].Id = currentlySelectedMethod.moduleId then
         begin
         moduleIndex := i;
         break;
         end;
  if moduleIndex = -2 then
     raise Exception.Create ('Internal Error: Unable to locate module [' + inttostr (currentlySelectedMethod.moduleId) + ']');

  serviceObject := moduleList[moduleIndex].getServiceObject(currentlySelectedMethod.serviceId);
  methodObject := serviceObject.getMethodObject(currentlySelectedMethod.methodId);

  args := sbw.getSignatureArray (currentlySelectedMethod.moduleId, currentlySelectedMethod.serviceId, currentlySelectedMethod.methodId);
  frmRunMethod := TfrmRunMethod.Create (nil);
  frmRunMethod.argList := args;
  frmRunMethod.ModuleId := currentlySelectedMethod.moduleId;
  frmRunMethod.ServiceId := currentlySelectedMethod.serviceId;
  frmRunMethod.MethodId := currentlySelectedMethod.methodId;
  frmRunMethod.lblHelpStr.caption := methodObject.helpStr; 
  frmRunMethod.sbw := sbw;
  try
     frmRunMethod.runSignature := methodObject.sigStr; 
     frmRunMethod.lblRunMethod.Caption := frmRunMethod.runSignature;
     frmRunMethod.numArgs := sbw.getNumArguments (currentlySelectedMethod.moduleId, currentlySelectedMethod.serviceId, currentlySelectedMethod.methodId);
     frmRunMethod.sigStr := sbw.getSignatureArray (currentlySelectedMethod.moduleId, currentlySelectedMethod.serviceId, currentlySelectedMethod.methodId);
     frmRunMethod.createFormLayout();
     frmRunMethod.ShowModal;
  finally
     frmRunMethod.Free;
  end;
end;

procedure TfrmMain.mnuCopySubLevelClick(Sender: TObject);
var node : TTreeNode;
    MousePos : TPoint;
begin
  q := TreeViewReg.ClientToScreen (MousePos);
  node := TreeViewReg.GetNodeAt (MousePos.x, MousePos.y);
  if node.Level = 1 then
     PopupMenu.Popup (q.x, q.Y);
end;


procedure TfrmMain.lbModuleListExit(Sender: TObject);
begin
  TreeView.setFocus;
end;

procedure TfrmMain.btnRunMethodClick(Sender: TObject);
begin
  mnuRunMethodClick (Sender);
end;

procedure TfrmMain.TreeViewDblClick(Sender: TObject);
begin
    if btnRunMethod.enabled then
       mnuRunMethodClick (Sender);
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  moduleList.Free;
end;

procedure TfrmMain.TreeViewDeletion(Sender: TObject; Node: TTreeNode);
begin
  //TObject (Node.Data).Free;
  //Node.Data := nil;
end;


initialization
  ct := TCriticalSection.Create;
finalization
  ct.Free;
end.
