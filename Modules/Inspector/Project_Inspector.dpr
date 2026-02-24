program Project_Inspector;

uses
  System.StartUpCopy,
  FMX.Forms,
  ufMain in 'ufMain.pas' {frmMain},
  SBW.Connection in '..\..\SBW.Connection.pas',
  SBW.DataBlock in '..\..\SBW.DataBlock.pas',
  SBW.List in '..\..\SBW.List.pas',
  SBW.Message in '..\..\SBW.Message.pas',
  SBW.Module in '..\..\SBW.Module.pas',
  SBW.Service in '..\..\SBW.Service.pas',
  SBW.Signature in '..\..\SBW.Signature.pas',
  SBW.Types in '..\..\SBW.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
