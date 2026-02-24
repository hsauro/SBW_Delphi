program ProjectPlotcanvas_SBW;

uses
  System.StartUpCopy,
  FMX.Forms,
  ufMain in 'ufMain.pas' {frmMain},
  SBW.Types in '..\..\SBW.Types.pas',
  SBW.DataBlock in '..\..\SBW.DataBlock.pas',
  SBW.Helpers in '..\..\SBW.Helpers.pas',
  SBW.Message in '..\..\SBW.Message.pas',
  SBW.Signature in '..\..\SBW.Signature.pas',
  SBW.Module in '..\..\SBW.Module.pas',
  SBW.Service in '..\..\SBW.Service.pas',
  SBW.List in '..\..\SBW.List.pas',
  SBW.Connection in '..\..\SBW.Connection.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
