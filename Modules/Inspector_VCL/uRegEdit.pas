unit uRegEdit;

interface

uses 
SysUtils,
Classes,
{$ifdef MSWINDOWS}   Windows,
Messages,
Forms,
Controls,
Dialogs,
Graphics,
StdCtrls,
extctrls,
Buttons,
FileCtrl,
uSBWD,
FMX.Controls;
{$else}
   QForms, QGraphics, QDialogs, QStdCtrls, QExtCtrls,
   QControls, QButtons;
{$endif}

type
  TfrmRegEdit = class(TForm)
    Panel1: TPanel;
    BtnOK: TButton;
    Label1: TLabel;
    Label2: TLabel;
    edtDisplayName: TEdit;
    btnDirectory: TSpeedButton;
    Label3: TLabel;
    edtHelpStr: TEdit;
    cboManagementType: TComboBox;
    Label4: TLabel;
    edtCommandLine: TMemo;
    edtModuleName: TEdit;
    Label5: TLabel;
    BtnCancel: TButton;
    OpenDialog: TOpenDialog;
    procedure btnDirectoryClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure cboManagementTypeChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    managementType : integer;
  end;

var
  frmRegEdit: TfrmRegEdit;

implementation

{$R *.FMX}

procedure TfrmRegEdit.btnDirectoryClick(Sender: TObject);
begin
{$ifndef LINUX}
  if OpenDialog.Execute then
     EdtCommandLine.Text := OpenDialog.FileName + ' -sbwmodule';
 {$endif}
end;

procedure TfrmRegEdit.BtnOKClick(Sender: TObject);
begin
  //sbw.broker.registerModule (moduleName : string; moduleDisplayName : string; moduleType : integer; commandLine : string; helpString : string);
  //SBWModuleImplSetCommandLine (PChar (EdtCommandLine.Text));
  //SBWModuleImplRegister();
end;

procedure TfrmRegEdit.cboManagementTypeChange(Sender: TObject);
begin
  if cboManagementType.ItemIndex = 0 then
     managementType := 0
  else managementType := 1;
end;

end.
