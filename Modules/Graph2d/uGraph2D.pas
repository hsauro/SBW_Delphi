unit uGraph2D;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JRChart, Buttons, StdCtrls, ExtCtrls, uSBWCommon,
  ComCtrls, uSBWUtils, uTSBW, uSBWD, uSBWArray;

type
  TfrmMain = class(TForm)
    Image1: TImage;
    BtnFastPlot: TButton;
    StatusBar: TStatusBar;
    sbw: TSBW;
    procedure FormCreate(Sender: TObject);
    procedure sbwCallEvent(Sender: TObject; var ServiceId,
      MethodId: Integer; Data: PChar);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnFastPlotClick(Sender: TObject);
    procedure sbwRegister(Sender: TObject);
    procedure sbwFailedToConnect(Sender: TObject; msg: String);
  private
    { Private declarations }
  public
    { Public declarations }
    SystemId, UpdateId, ServiceId, SetXmaxId, PlotXYId, ClearGraphId, MoveToId : integer;
    LineToId, DrawLineId : smallint;
    Bitmap : TBitmap;
    InspId, mid : integer;
    procedure  PlotXY (ds : TDataStream);
    procedure  DisplayGraph (ds : TDataStream);
    procedure  SetWorld (ds : TDataStream);
    procedure  MoveTo (ds : TDataStream);
    procedure  LineTo (ds : TDataStream);
    //function  UpdateModule (ds : TDataStream) : PDataStream;
    procedure  DrawLine (ds : TDataStream);

    procedure MarkAt (x, y : double);
  end;

  TDPoint = record x, y : double; end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

Uses uGraphUtils;


{function TfrmMain.UpdateModule (d : PChar) : SBWDataBlockWriter;
begin
  Result.ComType := dtTerminator;
end;}


procedure TfrmMain.SetWorld (ds : TDataStream);
begin
  WorldXmin := ds.getScalar();
  WorldXmax := ds.getScalar();
  WorldYmin := ds.getScalar();
  WorldYmax := ds.getScalar();
  ComputeScalingFactors;
end;


procedure TfrmMain.PlotXY (ds : TDataStream);
var nPoints, i, j : integer; x, y : double; ar : TSBWArray;
begin
  ar := ds.getArray();
  for i := 0 to ar.nElements[0] - 1 do
      begin
      x := ar.getScalar (i, 0);
      y := ar.getScalar (i, 1);
      MarkAt (x, y);
      end;
 end;


procedure TfrmMain.MarkAt (x, y : double);
begin
  Bitmap.Canvas.Pixels[fx (x), fy (y)] := clBlack;
end;


procedure TfrmMain.MoveTo (ds : TDataStream);
var x, y : double;
begin
  x := ds.getScalar();
  y := ds.getScalar();
  uGraphUtils.MoveTo (Bitmap.Canvas, x, y);
end;


procedure TfrmMain.LineTo (ds : TDataStream);
var x, y : double;
begin
  x := ds.getScalar();
  y := ds.getScalar();
  uGraphUtils.LineTo (Bitmap.Canvas, x, y);
end;


procedure TfrmMain.DrawLine (ds : TDataStream);
var x1, y1, x2, y2 : double;
begin
  x1 := ds.getScalar();
  y1 := ds.getScalar();
  x2 := ds.getScalar();
  y2 := ds.getScalar();
  uGraphUtils.DrawLine (Bitmap.Canvas, x1, y1, x2, y2);
end;


procedure TfrmMain.DisplayGraph (ds : TDataStream);
begin
  Image1.Canvas.Draw (0, 0, BitMap);
  Bitmap.Canvas.FillRect (rect (0, 0, Bitmap.Width, Bitmap.Height));
  Image1.Repaint;
  Application.ProcessMessages;
end;


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Bitmap := TBitmap.Create;
  Bitmap.Width := Image1.Width;
  Bitmap.Height := Image1.Height;
  Bitmap.Canvas.Pen.Color := clBlack;
  Bitmap.Canvas.Brush.color := clWhite;

  vxmin := 0; vxmax := Bitmap.Width; vymin := 0; vymax := Bitmap.Height;
end;


procedure TfrmMain.sbwCallEvent(Sender: TObject; var ServiceId,
  MethodId: Integer; Data: PChar);
var x, y : double; nPoints, i : integer;
begin
  //case ServiceId of
  // SBWId : begin end;
  // end;
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Bitmap.Free;
end;


procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmMain.BtnFastPlotClick(Sender: TObject);
const n = 6;
var i, j, k, t1 : integer; pts : array[0..n-1] of TDPoint; theta, thinc : double;
begin
  t1 := GetTickCount;
  WorldXMin := -1; WorldXMax := 1;
  WorldYMin := -1; WorldYMax := 1;
  ComputeScalingFactors;
  theta := 0.0; thinc := 2*3.1415/n;
  for k := 0 to 1000 do
      begin
      for i := 0 to n-1 do
          begin
          pts[i].x := cos (theta);
          pts[i].y := sin (theta);
          theta := theta + thinc;
          end;

      for i := 0 to n-2  do
          for j := i+1 to n-1 do
              begin
              uGraphUtils.MoveTo (Bitmap.Canvas, pts[i].x, pts[i].y);
              uGraphUtils.LineTo (Bitmap.Canvas, pts[j].x, pts[j].y);
              end;

      Image1.Canvas.Draw (0, 0, BitMap);
      Bitmap.Canvas.FillRect (rect (0, 0, Bitmap.Width, Bitmap.Height));
      Image1.Repaint;
      theta := theta + 0.05;
      end;
  showmessage ('Time:' + inttostr (GetTickCount - t1));
end;


procedure TfrmMain.sbwRegister(Sender: TObject);
begin
  sbw.addService ('graph2d', 'Simple Graphing Service', '', '');
  sbw.addMethod ('graph2d', SetWorld, 'void setworld(double, double, double, double)', '');
  sbw.addMethod ('graph2d', plotxy, 'void plotxy(double[][])', '');
  sbw.addMethod ('graph2d', moveto, 'void moveto(double, double)', '');
  sbw.addMethod ('graph2d', lineto, 'void lineto(double, double)', '');
  sbw.addMethod ('graph2d', DrawLine, 'void drawline(double, double, double, double)', '');
  sbw.addMethod ('graph2d',  DisplayGraph, 'void displaygraph()', '');
end;


procedure TfrmMain.sbwFailedToConnect(Sender: TObject; msg: String);
begin
  StatusBar.Panels[0].Text := ' Failed to Connect!';
end;

end.
