program Inspector;



uses
  Forms,
  uMain in 'uMain.pas' {frmMain},
  uRegEdit in 'uRegEdit.pas' {frmRegEdit},
  ufRunMethod in 'ufRunMethod.pas' {frmRunMethod},
  uBroker in '..\DSBWLib\uBroker.pas',
  uModuleList in 'uModuleList.pas';

{$R *.res}

begin
  //memchk;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmRunMethod, frmRunMethod);
  Application.Run;
end.
