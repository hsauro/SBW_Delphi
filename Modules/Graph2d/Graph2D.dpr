program Graph2D;

uses
  Forms,
  uGraph2D in 'uGraph2D.pas' {frmMain},
  uGraphUtils in 'uGraphUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
