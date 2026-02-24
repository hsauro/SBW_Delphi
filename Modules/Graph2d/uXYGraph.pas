unit uXYGraph;

interface

Uses Windows, Messages, SysUtils, Classes, Controls, Graphics, contnrs;

const
  AllowedLogTickCounts = [0,1,2,5,7,9];
    {InitLogTicks and AdjustLogTickCount must correspond.
     AdjustLogLabelDecs must correspond with the result of InitLogTicks.}
  MaxLogTickCount = 9; {Must equal largest value in AllowedLogTickCounts}
  LogE = 0.4342944818;

type
  TNormCoord = record x, y, w, h : double; end;
  TWRect = record xmin, ymin, xmax, ymax : double; end;
  TAxisType = (axisX, axisY);

  TLogTickInfo = record
         LogTickArray: array[0..MaxLogTickCount+1] of Double;
         LogTickCount: Word;
         LogTickIndex: Word;
         LogStepping: Boolean; {signals linear steps}
         DrawMinorLabels: Boolean;
         end;


  TCoordType = (wLine, dLine);
  TSBWXYGraph = class;
  TGObject = class (TObject)
       coordType : TCoordType;
       viewPort : TSBWXYGraph;
       procedure Paint; virtual; abstract;
  end;

  TMoveToObject = class (TGObject)
        x, y : double;
        procedure Paint; override;
        procedure moveTo (x, y : double);
        constructor Create (viewPort : TSBWXYGraph; x, y : double);
  end;


  TLineToObject = class (TGObject)
        x, y : double;
        LineColor : TColor;
        LineWidth : integer;
        LineStyle : TPenStyle;
        procedure Paint; override;
        procedure lineTo (x, y : double);
        constructor Create (viewPort : TSBWXYGraph; x, y : double);
  end;


  TLineObject = class (TGObject)
        x1, y1, x2, y2 : double;
        ix1, iy1, ix2, iy2 : integer;  // Used when drawing device specificed lines
        LineColor : TColor;
        LineWidth : integer;
        LineStyle : TPenStyle;
        procedure Paint; override;
        procedure drawLine (x1, y1, x2, y2, Thickness : integer);
        constructor Create (viewPort : TSBWXYGraph; x1, y1, x2, y2 : double); overload;
        constructor Create (viewPort : TSBWXYGraph; x1, y1, x2, y2 : integer); overload;
  end;


  TMarkerType = (mtPoint, mtCircle, mtSquare);
  TMarkerObject = class (TGObject)
        x, y, r : double;
        w, h : double;
        marker : TMarkerType;
        FillColor : TColor;
        OutLineColor : TColor;
        procedure Paint; override;
        procedure DrawPoint (x, y : integer);
        procedure DrawCircle (x, y, r : integer);
        procedure DrawSquare (x, y, w, h : integer);
        constructor Create (viewPort : TSBWXYGraph; x, y, r : double; marker : TMarkerType); overload;
        constructor Create (viewPort : TSBWXYGraph; x, y, w, h : double); overload;
  end;

  TTextObject = class (TGObject)
       x, y : double;
       ix, iy: integer;
       str : string;
       procedure Paint; override;
       constructor Create (viewPort : TSBWXYGraph; x, y : double; str: string); overload;
       constructor Create (viewPort : TSBWXYGraph; x, y : integer; str: string); overload;
  end;


  TGObjectList = TObjectList;


 TSBWXYGraph = class (TGraphicControl)
  private
      FBitMap : TBitmap;

      xscale, xfactor : double;
      yscale, yfactor : double;

      bolUpDate : boolean;

      FLeft, FRight : integer;
      FBottom, FTop : integer;
      FStepX, FStepY : double;
      FLabelDec : integer;

      function  normalisedToDevice (const wr : TWRect) : TRect;

      function  LabelString(tick: Double; dw : double) : string;
      procedure draw;
      procedure setUpDate (value : boolean);
      function  GetFirstTick(var logTickInfo: TLogTickInfo; axisType : TAxisType) : Double;
      function  GetNextTick(tick: Double; var logTickInfo: TLogTickInfo; var drawThisLabel: Boolean; axisType : TAxisType): Double;

      procedure WmEraseBkgnd(VAR Msg: TWmEraseBkgnd); message Wm_EraseBkgnd;

  protected
      procedure Paint; override;
      procedure Resize; override;
  public
      XOrigin, YOrigin : double;
      WorldXmax, WorldXmin : double;
      WorldYMax, WorldYMin : double;
      // Device coords for area inside graph axes (i.e the clipped area)
      vxmin, vxmax, vymin, vymax : integer;

      GObjectList : TGObjectList;

      tickLength : integer;
      constructor Create (AOwner : TComponent); override;
      destructor  Destroy; override;

      procedure DetermineOrigin;
      procedure ClearData; overload;
      procedure ClearImage;

      function  xf (dx : integer) : double;
      function  yf (dy : integer) : double;

      function  fx (wx : double) : integer;
      function  fy (wy : double) : integer;

      procedure ComputeScalingFactors;

      procedure setWorld (xmin, ymin, xmax, ymax : double);

      procedure drawAxes;
      procedure DrawXTickMarks (reverse : boolean);
      procedure DrawYTickMarks (reverse : boolean);

      function  DrawXLabels (reverse : boolean) : integer;
      function  DrawYLabels (reverse : boolean) : integer;

      procedure addMark (x, y : double); overload;
      procedure addPoint  (x, y : double); overload;
      procedure addCircle (x, y, r : double); overload;
      procedure addSquare (x, y, w, h : double); overload;
      procedure addLine   (x1, y1, x2, y2 : double); overload;

      procedure addwLine (x1, y1, x2, y2 : double);

      procedure moveTo (x, y : double); overload;
      procedure lineTo (x, y : double); overload;

      property  updateGraph : boolean read bolUpDate write SetUpDate;
  published
      property Align;
      property OnResize;
  end;


procedure Register;
function wrect (x1, y1, x2, y2 : double) : TWRect;

implementation

procedure Register;
begin
  RegisterComponents ('JLib', [TSBWXYGraph]);
end;


function wrect (x1, y1, x2, y2 : double) : TWRect;
begin
  result.xmin := x1; result.xmax := x2;
  result.ymin := y1; result.ymax := y2;
end;


// Drawing Objects

constructor TMoveToObject.Create (viewPort : TSBWXYGraph; x, y : double);
begin
  inherited Create;
  self.x := x; self.y := y;
  self.viewPort := viewPort;
end;


procedure TMoveToObject.moveTo (x, y : double);
begin
  viewPort.FBitmap.Canvas.moveTo (viewPort.fx (x), viewPort.fy (y));
end;


procedure TMoveToObject.Paint;
begin
  viewPort.FBitmap.Canvas.moveTo (viewPort.fx(x), viewPort.fy(y));
end;


// --------------------------------------------------------------


constructor TLineToObject.Create (viewPort : TSBWXYGraph; x, y : double);
begin
  inherited Create;
  self.x := x; self.y := y;
  self.viewPort := viewPort;
end;


procedure TLineToObject.lineTo (x, y : double);
begin
  viewPort.FBitmap.Canvas.lineTo (viewPort.fx (x), viewPort.fy (y));
end;


procedure TLineToObject.Paint;
begin
  with viewPort do
       begin
       FBitmap.Canvas.Pen.Color := LineColor;
       FBitmap.Canvas.Pen.Width := LineWidth;
       FBitmap.Canvas.Pen.Style := LineStyle;
       FBitmap.Canvas.lineTo (fx(x), fy(y));
       end;
end;


// --------------------------------------------------------------


// x1,y1... are in world coordinates
constructor TLineObject.Create (viewPort : TSBWXYGraph; x1, y1, x2, y2 : double);
begin
  inherited Create;
  coordType := wLine;
  self.x1 := x1; self.y1 := y1;
  self.x2 := x2; self.y2 := y2;
  self.viewPort := viewPort;
  LineColor := clBlack;
  LineWidth := 1;
  LineStyle := psSolid;
end;


// x1,y1... are in device coordinates
constructor TLineObject.Create (viewPort : TSBWXYGraph; x1, y1, x2, y2 : integer);
begin
  inherited Create;
  coordType := dLine;
  self.ix1 := x1; self.iy1 := y1;
  self.ix2 := x2; self.iy2 := y2;
  self.viewPort := viewPort;
  LineColor := clBlack;
  LineWidth := 1;
  LineStyle := psSolid;
end;



procedure TLineObject.Paint;
begin
  with viewPort do
       begin
       FBitmap.Canvas.Pen.Color := LineColor;
       FBitmap.Canvas.Pen.Width := LineWidth;
       FBitmap.Canvas.Pen.Style := LineStyle;
       if coordType = wLine then
          drawLine (fx(x1), fy(y1), fx(x2), fy(y2), 1)
       else
          drawLine (ix1, iy1, ix2, iy2, 1);
       end;
end;


procedure TLineObject.drawLine (x1, y1, x2, y2, Thickness : integer);
var oldThick : integer;
begin
  oldThick := viewPort.FBitmap.Canvas.pen.width;
  viewPort.FBitmap.Canvas.pen.width := Thickness;
  viewPort.FBitmap.Canvas.moveto (x1, y1);
  viewPort.FBitmap.Canvas.lineto (x2, y2);
  viewPort.FBitmap.Canvas.pen.width := oldThick;
end;


// --------------------------------------------------------------


constructor TMarkerObject.Create (viewPort : TSBWXYGraph; x, y, r : double; marker : TMarkerType);
begin
  inherited Create;
  self.x := x; self.y := y; self.r := r;
  self.viewPort := viewPort;
  self.marker := marker;
  OutLineColor := clBlack;
  FillColor := clWhite;
end;


constructor TMarkerObject.Create (viewPort : TSBWXYGraph; x, y, w, h : double);
begin
  inherited Create;
  self.x := x; self.y := y;
  self.w := w; self.h := h;
  self.viewPort := viewPort;
  self.marker := mtSquare;
  OutLineColor := clBlack;
  FillColor := clWhite;
end;


procedure TMarkerObject.DrawPoint (x, y : integer);
begin
  viewPort.FBitmap.Canvas.Pixels[x, y] := viewPort.FBitmap.Canvas.Pen.Color;
end;


procedure TMarkerObject.DrawCircle (x, y, r : integer);
begin
  viewPort.FBitmap.Canvas.Ellipse (x-r, y-r, x+r, y+r);
end;


procedure TMarkerObject.DrawSquare (x, y, w, h : integer);
begin
  viewPort.FBitmap.Canvas.Rectangle (x, y, x+w, y+h);
end;


procedure TMarkerObject.Paint;
begin
  with viewPort do
       begin
       FBitmap.Canvas.Pen.Color := OutLineColor;
       FBitmap.Canvas.Brush.Color := FillColor;

       case marker of
            mtPoint : DrawPoint (fx(x), fy(y));
            mtCircle : DrawCircle (fx (x), fy(y), fx (r) - fx(0));
            mtSquare : DrawSquare (fx (x), fy(y), fx (w) - fx (0), fy (h) - fy (0));
       end;
       end;
end;


constructor TTextObject.Create (viewPort : TSBWXYGraph; x, y : double; str: string);
begin
  inherited Create;
  coordType := wLine;
  self.x := x; self.y := y;
  self.str := str;
  self.viewPort := viewPort;
end;


constructor TTextObject.Create (viewPort : TSBWXYGraph; x, y : integer; str: string);
begin
  inherited Create;
  coordType := dLine;
  self.ix := x; self.iy := y;
  self.str := str;
  self.viewPort := viewPort;
end;


procedure TTextObject.Paint;
begin
  if coordType = wLine then
     viewPort.FBitmap.Canvas.TextOut (viewport.fx (x), viewport.fy (y), str)
  else viewPort.FBitmap.Canvas.TextOut (ix, iy, str)
end;


// --------------------------------------------------------------
// --------------------------------------------------------------


{--------- DoResize helper functions -------}
{ step size chosen in a 1,2,5,10 squence depending not only on the
  characteristic, but also the mantissa, of the range}
function GetStep(minSteps: Word; FMax, FMin : double): Double;
var
  w, t, B: Double;
begin
  w := FMax - FMin;
  if w <= 0 then raise Exception.Create('GetStep entered with bad range');
  t := ln(w)*Loge;
  if t < 0 then t := t - 1;
  B := exp( trunc(t * 1.001) / Loge );
  if         w/B >= minSteps then Result := B
  else if  2*w/B >= minSteps then Result := B/2
  else if  5*w/B >= minSteps then Result := B/5
  else if 10*w/B >= minSteps then Result := B/10
  else if 20*w/B >= minSteps then Result := B/20
  else if 50*w/B >= minSteps then Result := B/50
  else                            Result := B/100;
  {sufficient for maxSteps <= 125}
end {GetStep};


function TSBWXYGraph.normalisedToDevice (const wr : TWRect) : TRect;
begin
  result.Left := trunc (wr.xmin * Width);
  result.Right := trunc (wr.xmax * Width);
  result.Top := trunc (wr.ymax * Height);
  result.Bottom := trunc (wr.ymin * Height);
end;


// If the viewport or physical port changes then this routines must
// be called to recompute the scaling factors
procedure TSBWXYGraph.ComputeScalingFactors;
var dx, dy : double;
begin
  dx := WorldXmax - WorldXmin;

  vxmin := FLeft;
  vxmax := FBitmap.Width - FRight;

  vymin := FBottom;
  vymax := FBitmap.Height - FTop;

  xscale  := (vxmax - vxmin) / dx;
  xfactor :=(WorldXmax*vxmin - WorldXmin*vxmax) / dx;

  dy := WorldYmax - WorldYmin;
  yscale  := (vymax - vymin) / dy;
  yfactor :=(WorldYmax*vymin - WorldYmin*vymax) / dy;
end;


// Convert device coordinate into world coordinate
function TSBWXYGraph.xf (dx : integer) : double;
begin
  Result := (dx - xfactor)/xscale;
end;


// Convert device coordinate into world coordinate
function TSBWXYGraph.yf (dy : integer) : double;
begin
  Result := (Height - yfactor - dy)/yscale;
end;


// Convert world coordinate into device coordinate
function TSBWXYGraph.fx (wx : double) : integer;
var w : integer;
begin
  //if FLogging then
  //   result := (FCurrWidth - FDimensions.FLeft - FDimensions.FRight) / (Ln(FMax) - Ln(FMin))
  //else
     //result := (Width - FDimensions.FLeft - FDimensions.FRight) / (FMax - FMin);
  w := trunc (xscale * wx + xfactor);
  if abs (w) > 20000 then
     begin if w > 0 then result := 20000 else result := -20000; end
  else
     result := round (w);
end;


function TSBWXYGraph.fy (wy : double) : integer;
var w : integer;
begin
  //if FLogging then
  //  FM := (FCurrHeight - FDimensions.FTop - FDimensions.FBottom) / (Ln(FMax) - Ln(FMin))
  //else
    //result := (Height - FDimensions.FTop - FDimensions.FBottom) / (FMax - FMin);
  w := Height - trunc (yscale * wy + yfactor);

  if abs (w) > 2000 then
     begin if w > 0 then result := 20000 else result := -20000; end
  else result:= round (w);
end;


constructor TSBWXYGraph.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);
  ControlStyle := ControlStyle + [ csOpaque ];

  // Background bitmap onto which we draw the graphics
  FBitmap := TBitmap.Create;
  FBitmap.Width := 100;
  FBitmap.Height := 100;
  FBitmap.Canvas.Pen.Color := clBlack;
  FBitmap.Canvas.Brush.color := clSilver;

  bolUpDate := True;

  FLeft := 50; FRight := 40;
  FBottom := 40; FTop := 40;
  tickLength := 5;
  FLabelDec := 2;

  WorldXmax := 0.0; WorldXmax := 1.0;
  WorldYmax := 0.0; WorldYmax := 1.0;

  GObjectList := TGObjectList.Create;

  Width := 100;
  Height := 100;

  // Set up a default world
  ComputeScalingFactors;
end;


destructor TSBWXYGraph.Destroy;
begin
  GObjectList.Free;
  inherited Destroy;
end;


procedure TSBWXYGraph.WmEraseBkgnd(var Msg: TWmEraseBkgnd);
begin
  Msg.Result := 1;
end;


procedure TSBWXYGraph.ClearData;
begin
  GObjectList.Clear;
end;


procedure TSBWXYGraph.ClearImage;
begin
  FBitmap.canvas.brush.color := clWhite;
  FBitmap.canvas.FillRect (rect (0, 0, FBitmap.Width, FBitmap.Height));
  Canvas.Draw (0, 0, FBitmap);
end;


procedure TSBWXYGraph.Resize;
var i : integer; ob : TGObject;
begin
  {if Parent <> nil then
     begin
     // delete any absolute coordinate objects because
     // these need to be recomputed
     for i := 0 to GObjectList.Count - 1 do
         if (GObjectList[i] as TGObject).coordType = dLine then
            begin
            ob := GObjectList[i] as TGObject;
            // TObjectList items automatically Free themselves
            // when we set them to nil
            GObjectList[i] := nil;
            end;
     GObjectList.Pack;
     DrawAxes;
     paint();
     end;}
end;


procedure TSBWXYGraph.setUpDate (value : boolean);
begin
  bolUpdate := value;
  if value then paint();
end;


procedure TSBWXYGraph.setWorld (xmin, ymin, xmax, ymax : double);
begin
  WorldXmin := xmin;
  WorldXmax := xmax;
  WorldYmin := ymin;
  WorldYmax := ymax;
  ComputeScalingFactors;
end;


procedure TSBWXYGraph.draw;
var i : integer; ob : TGObjectList;
begin
   ComputeScalingFactors;
   for i := 0 to GObjectList.Count - 1 do
       begin
       ob := GObjectList;
       (ob[i] as TGObject).Paint();
       end;
end;


procedure TSBWXYGraph.Paint;
begin
  inherited Paint;
  FBitmap.Width := Width;
  FBitmap.Height := Height;
  // flood fill appears to be slighly quicker and eliminates the dots bug
  FBitmap.canvas.brush.color := clWhite;
  FBitmap.canvas.FillRect (rect (0, 0, FBitmap.Width, FBitmap.Height));
  FBitmap.canvas.pen.color := clBlue;
  draw;
  if bolupDate then
     Canvas.Draw (0, 0, FBitmap);
end;



function TSBWXYGraph.LabelString(tick: Double; dw : double): string;
begin
  //if not FShowAsTime then
    if (abs(tick) < dw*0.001 {zero}) or
        ((abs(tick) > 0.000999) and (abs(tick) < 9999)) then
      Result := FloatToStrF(tick, ffFixed, 5, FLabelDec)
    else {very small or very large}
      Result := FloatToStrF(tick, ffExponent, 2, 0)
  //else
  // result := FormatDateTime(FDateFormat,tick);
end;


// This procedure decides where the axes will be positioned relative to each other
// Only called when the World coordinates have been assigned }
procedure TSBWXYGraph.DetermineOrigin;
begin
  XOrigin := WorldXmin;
  if WorldXmin = 0 then XOrigin := 0.0;
  { If the x scale is all negative, position the origin on the left side
  not the right side as one might expect, might look odd to the user otherwise }
  { This should be configurable }
  {if AutoScale_Xmax <= 0 then X_Origin := AutoScale_Xmin;}
  if (WorldXmin < 0) and (WorldXmax > 0) then XOrigin := 0.0;

  YOrigin := WorldYmin;
  if WorldYmin = 0 then YOrigin := 0.0;
  {if AutoScale_Ymax <= 0 then Y_Origin := AutoScale_Ymin;}
  if (WorldYmin < 0) and (WorldYmax > 0) then YOrigin := 0.0;
end;


procedure TSBWXYGraph.drawAxes;
var nTicks : integer;
begin
  nTicks := 5;
  FStepX := GetStep(nTicks, WorldXMax, WorldXMin);
  FStepY := GetStep(nTicks, WorldYMax, WorldYMin);

  addLine (WorldXmin, YOrigin, WorldXmax, YOrigin);
  addLine (XOrigin, WorldYmin, XOrigin, WorldYmax);
  DrawXTickMarks (False);
  DrawXLabels (False);

  DrawYTickMarks (False);
  DrawYLabels (False);
end;


function TSBWXYGraph.GetFirstTick(var logTickInfo: TLogTickInfo; axisType : TAxisType): Double;
var
  t, B: Double;
  j: Word;
  FMin, FStep : double;
begin
  //if not FLogging or logTickInfo.LogStepping then
  //if FShowAsTime then result := getfirstdatetick else
  begin
    case axisType of
      axisX : begin
              FMin := WorldXMin;
              FStep := FStepX;
              end;
      axisY : begin
              FMin := WorldYMin;
              FStep := FStepY;
              end;
    end;

    Result := FMin + 0.01*FStep;
    Result := trunc( Result / FStep ) * FStep;
    if (FMin < 0) then Result := Result - FStep;
    if (FMin > Result + 0.01*FStep) then Result := Result + FStep;
    //AdjustLabelDecs;
  end
  //else {logging}
  //begin
    {t := ln(FMin) * Loge;
    if t < 0 then t := t - 1;
    B := exp( trunc(1.001 * t) / Loge ); // OK for FMin < 10^1000
    if B > FMin then B := B/10;

    // pre-condition: the TLogTickInfo has been initialized with 1..10:
    with logTickInfo do
    begin
      for j := 0 to LogTickCount+1 do
        LogTickArray[j] := B * LogTickArray[j];

      LogTickIndex := 0;
      t := FMin*0.999;
      while logTickInfo.LogTickArray[LogTickIndex] < t do
        Inc(LogTickIndex);
      Result := LogTickArray[LogTickIndex];
      AdjustLogLabelDecs(Result);}
    //end;
  //end;
end {GetFirstTick};


function TSBWXYGraph.GetNextTick(tick: Double; var logTickInfo: TLogTickInfo; var drawThisLabel: Boolean; axisType : TAxisType): Double;
var j: Word;
begin
  //if not FLogging or logTickInfo.LogStepping then
   // if FShowAsTime then
    //begin
    //  result := getnextdateTick(tick);
    //  drawthislabel := true;
    //end
    //else
    begin
      if axisType = axisX then
         Result := tick + FStepX
      else result := tick + FStepY;
      drawThisLabel := true;
    end
  {else // logging
    with logTickInfo do
    begin
      Inc(LogTickIndex);
      if LogTickIndex >= LogTickCount + 1 then
      begin
        for j := 0 to LogTickCount + 1 do
          LogTickArray[j] := 10 * LogTickArray[j];
        LogTickIndex := 0;
      end;
      Result := LogTickArray[LogTickIndex + 1];
      if (LogTickIndex = 0) then
        AdjustLogLabelDecs(Result);
      drawThisLabel := DrawMinorLabels or
                       (LogTickIndex = 0) or
                       (LogTickIndex = LogTickCount+1);
    end } //with, else};
end {GetNextTick};



procedure TSBWXYGraph.DrawXTickMarks (reverse : boolean);
var
  tick, maxTick: Double;
  tempLogTickInfo : TLogTickInfo;
  b : boolean;
  tx, ty1, ty2 : integer;
begin
  FBitmap.Canvas.Pen.Color := clBlack;//FAppearance.FAxesColor;
  FBitmap.Canvas.Pen.Style := psSolid;
  SetBkColor(FBitmap.Canvas.handle, clWhite);//FAppearance.FMarginColor);
  maxTick := WorldXMax + 0.001*(WorldXMax-WorldXMin);
  //if FLogging then tempLogTickInfo := FLogTickInfo;
  tick := GetFirstTick(tempLogTickInfo, axisX);
  ty1 := fy (WorldYMin);
  if reverse then
     ty2 := ty1 - tickLength
  else ty2 := ty1 + tickLength;

  while tick < maxTick do
    begin
    tx := fx (tick);
    GObjectList.Add (TLineObject.Create (self, tx, ty1, tx, ty2));
    tick := GetNextTick(tick, tempLogTickInfo, b, axisX);
    end;
end;



procedure TSBWXYGraph.DrawYTickMarks (reverse : boolean);
var
  tick, maxTick: Double;
  tx1, tx2, ty: longint;
  b, wyax: Boolean;
  tempLogTickInfo : TLogTickInfo;
begin
  FBitmap.Canvas.Pen.Color := clBlack;//FAppearance.FAxesColor;
  FBitmap.Canvas.Pen.Style := psSolid;
  SetBkColor(FBitmap.Canvas.handle, clWhite);//FAppearance.FMarginColor);
  maxTick := WorldYMax + 0.001*(WorldYMax-WorldYMin);
  //if FLogging then tempLogTickInfo := FLogTickInfo;
  tick := GetFirstTick(tempLogTickInfo, axisY);
  tx1 := fx (WorldXMin);
  if reverse then
     tx2 := tx1 + tickLength
  else tx2 := tx1 - tickLength;

  while tick < maxTick do
        begin
        GObjectList.Add (TLineObject.Create (self, tx1, fy (tick), tx2, fy (tick)));
        tick := GetNextTick(tick, tempLogTickInfo, b, axisY);
        end;
end;


function TSBWXYGraph.DrawXLabels (reverse : boolean) : integer;
var
  tick, maxTick: Double;
  ty: longint;
  lblStr: string;
  drawIt: Boolean;
  tempLogTickInfo : TLogTickInfo;
begin
{ X-axis labels }
  //FBitmap.Canvas.Font := FAppearance.FLabelFont;
  //FAltCanvas.Font := FAppearance.FLabelFont; {if FIsMetafiling FCanvas <> FAltCanvas}
  FBitmap.Canvas.Brush.Style := bsClear;
  SetBkColor(FBitmap.Canvas.handle, clWhite);//FAppearance.FMarginColor);
  maxTick := WorldXMax + 0.001*(WorldXMax-WorldXMin);   { rounding errors might exclude last point }
  if reverse then
    ty := fy (WorldYMin) - (tickLength + 5)
  else
    ty := fy (WorldYMin) + tickLength + 5;
  begin
    //if FLogging then tempLogTickInfo := FLogTickInfo;
    tick := GetFirstTick(tempLogTickInfo, axisX);
    drawIt := true;
    while tick < maxTick do
    begin
      lblStr := LabelString(tick, WorldXMax - WorldXMin);
      if drawIt or (maxTick - tick < FStepX) {ie, last one} then

        if reverse then
          GObjectList.Add (TTextObject.Create (self, fx(tick) - FBitmap.Canvas.TextWidth(lblStr) div 2,
                        ty - FBitmap.Canvas.TextHeight(lblstr),
                        lblStr))
        else
          GObjectList.Add (TTextObject.Create (self, fx(tick) - FBitmap.Canvas.TextWidth(lblStr) div 2,
                        ty,
                        lblStr));
      tick := GetNextTick(tick, tempLogTickInfo, drawIt, axisX);
    end;
  end;
  //result := FAltcanvas.Textheight(lblstr);
end;


function TSBWXYGraph.DrawYLabels (reverse : boolean) : integer;
var
  tick, maxTick: Double;
  tx, ty: Integer;
  lblStr: string;
  drawIt, wyax: Boolean;
  tempLogTickInfo : TLogTickInfo;
begin
{ Y-axis Labels }
  //FBitmap.Canvas.Font := FAppearance.FLabelFont;
  //FAltCanvas.Font := FAppearance.FLabelFont; {if FIsMetafiling FCanvas <> FAltCanvas}
  SetBkColor(FBitmap.Canvas.handle, clWhite);//FAppearance.FMarginColor);
  maxTick := WorldYMax + 0.001*(WorldYMax-WorldYMin);   { rounding errors might exclude last point }
  if reverse then
    tx := fx (WorldXMin) + (tickLength + 5)
  else
    tx := fx (WorldXMin) - (tickLength + 5);
  begin
    //if FLogging then tempLogTickInfo := FLogTickInfo;
    tick := GetFirstTick(tempLogTickInfo, axisY);
    drawIt := true;
    while tick < maxTick do
    begin
      lblStr := LabelString(tick, WorldYMax - WorldYMin);
      if drawIt or (maxTick - tick < FStepY) then
         if reverse then
            GObjectList.Add (TTextObject.Create (self, tx, fy (tick), lblStr))
         else GObjectList.Add (TTextObject.Create (self, tx - FBitmap.Canvas.TextWidth (lblStr), fy (tick) - FBitmap.Canvas.TextHeight (lblStr) div 2, lblStr));
       tick := GetNextTick(tick, tempLogTickInfo, drawIt, axisY);
    end;
  end;
  //result := FAltCanvas.TextWidth(lblstr);
end;


procedure TSBWXYGraph.addMark (x, y : double);
begin
  GObjectList.Add (TMarkerObject.Create (self, x, y, 0.02, mtCircle));
  paint();
end;


procedure TSBWXYGraph.addPoint (x, y : double);
begin
  GObjectList.Add (TMarkerObject.Create (self, x, y, 0, mtPoint));
end;


procedure TSBWXYGraph.addCircle (x, y, r : double);
begin
  GObjectList.Add (TMarkerObject.Create (self, x, y, r, mtCircle));
  paint();
end;


procedure TSBWXYGraph.addSquare (x, y, w, h : double);
begin
  GObjectList.Add (TMarkerObject.Create (self, x, y, w, h));
  paint();
end;


procedure TSBWXYGraph.addLine (x1, y1, x2, y2 : double);
begin
  GObjectList.Add (TLineObject.Create (self, x1, y1, x2, y2));
  paint();
end;


procedure TSBWXYGraph.addwLine (x1, y1, x2, y2 : double);
begin
  GObjectList.Add (TLineObject.Create (self, x1, y1, x2, y2));
  paint();
end;


procedure TSBWXYGraph.moveTo (x, y : double);
begin
  GObjectList.Add (TMoveToObject.Create (self, x, y));
  paint();
end;


procedure TSBWXYGraph.lineTo (x, y : double);
begin
  GObjectList.Add (TLineToObject.Create (self, x, y));
  paint();
end;


end.
