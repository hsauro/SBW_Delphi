unit ufMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Skia,
  FMX.Skia,
  FMX.Layouts,
  Generics.Collections,
  SBW.Types,
  SBW.Module,
  SBW.Service,
  SBW.DataBlock,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TLine = class
    p1, p2 : TPointF;
    FColor : TAlphaColor;

    constructor Create (x1, y1, x2, y2 : Double);

  end;

  TfrmMain = class(TForm)
    Layout1: TLayout;
    SkPaintBox1: TSkPaintBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas; const
        ADest: TRectF; const AOpacity: Single);
  private
    { Private declarations }
  public
    { Public declarations }
    Host : String;
    Port: Word;
    Server : TSBWModuleImpl;
    Lines : TList<TLine>;
    FCurrentColor: TAlphaColor;
    DrawService: TSBWService;
    SBWBeginUpdate : Boolean;
    function AddLines (FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;
    function ForceRedraw (FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;
    function HandleClear(FromModuleID: SBWModuleID;  Args: TSBWDataBlockReader): TSBWDataBlockWriter;
    function HandleBeginUpdate(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;
    function HandleEndUpdate(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

constructor TLine.Create (x1, y1, x2, y2 : Double);
begin
  p1.x := x1; p1.y := y1;
  p2.x := x2; p2.y := y2;
end;


function TfrmMain.AddLines (FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;
var x1, y1, x2, y2 : Double;
    R, G, B: Integer;
    Index: Integer;
begin
  x1 := Args.ReadDouble;
  y1 := Args.ReadDouble;
  x2 := Args.ReadDouble;
  y2 := Args.ReadDouble;

  R := Args.ReadInteger;
  G := Args.ReadInteger;
  B := Args.ReadInteger;

  Index := Lines.Add(TLine.Create (x1, y1, x2, y2));
  Lines[Index].FColor := TAlphaColor($FF000000 or (R shl 16) or (G shl 8) or B);

  if not SBWBeginUpdate then
     SkPaintBox1.Redraw;
  Result := TSBWDataBlockWriter.Create;
end;

function TfrmMain.HandleBeginUpdate(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;
begin
  SBWBeginUpdate := True;
  Result := TSBWDataBlockWriter.Create;
end;

function TfrmMain.HandleEndUpdate(FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;
begin
  SBWBeginUpdate := False;
  SkPaintBox1.Redraw;
  Result := TSBWDataBlockWriter.Create;
end;

function TfrmMain.HandleClear(FromModuleID: SBWModuleID;  Args: TSBWDataBlockReader): TSBWDataBlockWriter;
begin
  // Update UI on main thread
  TThread.Queue(nil,
    procedure
    begin
      Lines.Clear;
      SkPaintBox1.Redraw;
      //Log('clear() from module ' + IntToStr(FromModuleID));
    end);

  Result := TSBWDataBlockWriter.Create;
end;

function TfrmMain.ForceRedraw (FromModuleID: SBWModuleID; Args: TSBWDataBlockReader): TSBWDataBlockWriter;
begin
  // Update UI on main thread using TThread.Queue
  // Using Queue (async) instead of Synchronize (blocking) for better performance
  TThread.Queue(nil,
    procedure
    begin
       SBWBeginUpdate := False;
       SkPaintBox1.Redraw;
    end);

  Result := TSBWDataBlockWriter.Create;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Lines := TList<TLine>.Create;
  FCurrentColor := TAlphaColorRec.Black;
  SBWBeginUpdate := False;

  Host := '127.0.0.1';
  Port := SBW_DEFAULT_PORT;

      // Create the module
  Server := TSBWModuleImpl.Create(
      'edu.demo.plottingCanvas',
      'Canvas',
      mmtSelfManaged,
      'A simple canvas for drawing on');

  // Add the draw service
  DrawService := Server.AddService(
        'Draw',
        'Drawing Service',
        'Draw2',
        'Provides basic drawing operations');

  DrawService.AddMethod('void addLine(double, double, double, double)', AddLines, 'Add a line to the canvas');
  DrawService.AddMethod('void redraw()', ForceRedraw, 'Force a redraw');
  DrawService.AddMethod('void clear()',  HandleClear,  'Clear the canvas');
  DrawService.AddMethod('void beginUpdate()', HandleBeginUpdate, 'Pause drawing until forceredraw');
  DrawService.AddMethod('void endUpdate()', HandleEndUpdate, 'Restart drawing');

  Server.Connect(Host, Port, True);
end;

procedure TfrmMain.SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
    const ADest: TRectF; const AOpacity: Single);
var LPaint : ISkPaint;
    i : Integer;
begin
  ACanvas.Save;
  ACanvas.Clear(TAlphaColors.White);
  try
    LPaint := TSkPaint.Create (TSkPaintStyle.Stroke);
    LPaint.StrokeWidth := 2;
    LPaint.AntiAlias := True;

    for i := 0 to Lines.Count -1 do
        begin
        LPaint.Color := Lines[i].FColor;
        ACanvas.DrawLine(Lines[i].p1, Lines[i].p2, LPaint);
        end;

  finally
     ACanvas.Restore;
  end;
end;

end.
