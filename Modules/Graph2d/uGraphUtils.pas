unit uGraphUtils;

interface

Uses Graphics;

procedure ComputeScalingFactors;
function fx (wx : double) : integer;
function fy (wy : double) : integer;

procedure LineTo (Canvas : TCanvas; x, y : double);
procedure MoveTo (Canvas : TCanvas; x, y : double);
procedure DrawLine (Canvas : TCanvas; x1, y1, x2, y2 : double);

var vxmin, vxmax, vymin, vymax : integer;
    WorldXmax, WorldXmin : double;
    WorldYMax, WorldYMin : double;
    xscale, xfactor : double;
    yscale, yfactor : double;

implementation


// If the viewport or physical port changes then this routines must
// be called to recompute the scaling factors
procedure ComputeScalingFactors;
var dx, dy, sc : double; //vxmin, vxmax, vymin, vymax : integer;
begin
  dx := WorldXmax - WorldXmin;
  xscale  := (vxmax - vxmin) / dx;
  xfactor :=(WorldXmax*vxmin - WorldXmin*vxmax) / dx;

  dy := WorldYmax - WorldYmin;
  yscale  := (vymax - vymin) / dy;
  yfactor :=(WorldYmax*vymin - WorldYmin*vymax) / dy;
end;



// Convert world coordinates to physical coordinates
function fx (wx : double) : integer;
var w : double;
begin
  w := trunc (xscale * wx + xfactor);// + ObjRoot.left;
  if abs (w) > 20000 then
     begin if w > 0 then result := 20000 else result := -20000; end
  else
     result := round (w);
end;


// Convert world coordinates to physical coordinates, note reversal of y axis
function fy (wy : double) : integer;
var Ymin, Ymax : integer; w : double;
begin
  Ymin := vymin; Ymax := vymax;
  w := (Ymax+Ymin) - trunc (yscale * wy + yfactor);// + ObjRoot.top;
  if abs (w) > 20000 then
     begin if w > 0 then result := 20000 else result := -20000; end
  else
     result := round (w);
end;


procedure MoveTo (Canvas : TCanvas; x, y : double);
begin
  Canvas.MoveTo (fx (x), fy (y));
end;

procedure LineTo (Canvas : TCanvas; x, y : double);
begin
  Canvas.LineTo (fx (x), fy (y));
end;

procedure DrawLine (Canvas : TCanvas; x1, y1, x2, y2 : double);
begin
  Canvas.MoveTo (fx (x1), fy (y1));
  Canvas.LineTo (fx (x2), fy (y2));
end;




end.
