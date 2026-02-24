unit uHist;

interface

Uses SysUtils, uXYGraph, Math;

procedure binData (var data, x, y : array of double; datmin, datmax : double; nbin : integer);
procedure plotBin (vp : TSBWXYGraph; nbin : integer; x, y : array of double; center : boolean);

implementation

//-----------------------------------------------------------------------
 // void plotbin()
 //
 // Plot a histogram using the arrays x and y to represent data values
 // and frequencies respectively. If center is false, x values denote the
 // lower edge of the bin, and if center is true, they denote the center
 // of the bin.
//-----------------------------------------------------------------------

procedure plotBin(vp : TSBWXYGraph; nbin : integer; x, y : array of double; center : boolean);
var i : integer; xmin, xmax : double;
begin
  // Check x[i] are in ascending order

  for i := 0 to nbin - 2 do
       begin
       if (x[i] >= x[i + 1]) then
          raise Exception.Create ('plbin: Elements of x array must be increasing');
       end;

    if not center then
       begin
       for i := 0 to nbin - 2 do
           begin
           vp.GObjectList.Add (TLineObject.Create (vp, x[i], vp.WorldYMin, x[i], y[i]));
           vp.GObjectList.Add (TLineObject.Create (vp, x[i], y[i], x[i + 1], y[i]));
           vp.GObjectList.Add (TLineObject.Create (vp, x[i + 1], y[i], x[i + 1], vp.WorldYMin));
	   end;

       if (x[nbin - 1] < vp.WorldXMax) then
          begin
          vp.GObjectList.Add (TLineObject.Create (vp, x[nbin - 1], vp.WorldYMin, x[nbin - 1], y[nbin - 1]));
          vp.GObjectList.Add (TLineObject.Create (vp, x[nbin - 1], y[nbin - 1], vp.WorldXMax, y[nbin - 1]));
          vp.GObjectList.Add (TLineObject.Create (vp, vp.WOrldXMax, y[nbin - 1], vp.WorldXMax, vp.WorldYMin));
          end;
       end
    else
       begin
       if (nbin < 2) then
          exit;
       xmin := vp.WorldXMin;
       xmax := max(0.5 * (x[0] + x[2]), vp.WorldXMin);
       if (xmin < xmax) then
           begin
           vp.GObjectList.Add (TLineObject.Create (vp, xmin, vp.WorldYMin, xmin, y[0]));
           vp.GObjectList.Add (TLineObject.Create (vp, xmin, y[0], xmax, y[0]));
           vp.GObjectList.Add (TLineObject.Create (vp, xmax, y[0], xmax, vp.WorldYMin));
       end;
       for i := 1 to nbin - 2 do
           begin
           xmin := xmax;
           xmax := min(0.5 * (x[i] + x[i + 1]), vp.WorldXmax);
           vp.GObjectList.Add (TLineObject.Create (vp, xmin, vp.WorldYMin, xmin, y[i]));
           vp.GObjectList.Add (TLineObject.Create (vp, xmin, y[i], xmax, y[i]));
           vp.GObjectList.Add (TLineObject.Create (vp, xmax, y[i], xmax, vp.WorldYMin));
           end;
       xmin := xmax;
       xmax := vp.WorldXMax;
       if (xmin < xmax) then
           begin
           vp.GObjectList.Add (TLineObject.Create (vp, xmin, vp.WorldYMin, xmin, y[nbin - 1]));
           vp.GObjectList.Add (TLineObject.Create (vp, xmin, y[nbin - 1], xmax, y[nbin - 1]));
           vp.GObjectList.Add (TLineObject.Create (vp, xmax, y[nbin - 1], xmax, vp.WorldYMin));
           end;
       end;
end;


//-----------------------------------------------------------------------
 // void binData()
 //
 // Bins n values of a variable in array data[0..n-1] in
 // the range datmin to datmax using nbin bins. Returns the bin
 // data in arrays x and y
//-----------------------------------------------------------------------

procedure binData(var data, x, y : array of double; datmin, datmax : double; nbin : integer);
var dx : double; i, bin : integer;
begin
  dx := (datmax - datmin) / nbin;
  for i := 0 to nbin - 1 do
      begin
      x[i] := datmin + i * dx;
      y[i] := 0.0;
      end;

    for i := 0 to High (data) do
        begin
	bin := trunc ((data[i] - datmin) / dx);
        if bin < 0 then bin := 0;
        if bin >= nbin then bin := nbin - 1;
	y[bin] := y[bin] + 1;
    end;
end;


end.
