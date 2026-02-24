unit ufRunMethod;

interface

uses 
Windows,
Messages,
SysUtils,
Variants,
Classes,
Graphics,
Controls,
Forms,
Dialogs,
ExtCtrls,
StdCtrls,
uSBWCommon,
uSBWUtils,
uTSBW,
uSBWCallObject,
uSBWScanner,
uSBWArray,
uSBWList,
uSBWD,
Buttons,
uSBWComplex,
Math,
FMX.Controls;

type
  TDouble2Array = array of array of double;
  TArrayType = (at1D, at2D);
  TfrmRunMethod = class(TForm)
    Panel1: TPanel;
    btnClose: TButton;
    btnRun: TButton;
    Panel2: TPanel;
    resultMemoBox: TMemo;
    Panel3: TPanel;
    lblRunMethod: TLabel;
    pnlArgs: TPanel;
    Splitter1: TSplitter;
    Panel4: TPanel;
    btnCopyToClipboard: TSpeedButton;
    inputScrollBar: TScrollBox;
    Panel5: TPanel;
    lblMethodArguments: TLabel;
    btnHelp: TSpeedButton;
    lblHelpStr: TLabel;
    lblStatus: TLabel;
    btnSave: TSpeedButton;
    SaveDialog: TSaveDialog;
    chkMethodArgString: TCheckBox;
    OpenDialog: TOpenDialog;
    btnRunNTimes: TButton;
    edtRunNTimes: TEdit;
    lblnTimes: TLabel;
    btnStop: TButton;
    chkCSVArrays: TCheckBox;
    btnRunAndClose: TButton;
    edtTiming: TEdit;
    lblTiming: TLabel;
    Label1: TLabel;
    edtSD: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    edtMean: TEdit;
    chkSuppressOutput: TCheckBox;
    procedure btnRunClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure resultMemoBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnRunNTimesClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnRunAndCloseClick(Sender: TObject);
  private
    { Private declarations }
    function getCallArguments() : TSBWDataStream;

    function getIntArrayRow (sc : TSBWScanner; sign : integer) : TSBW1DIntArray;
    function getDoubleArrayRow (sc : TSBWScanner; sign : integer) : TSBW1DDoubleArray;
    function getComplexArrayRow (sc : TSBWScanner) : TSBW1DComplexArray;
    function getStringArrayRow (sc : TSBWScanner) : TSBW1DStrArray;

    function getIntArray (sc : TSBWScanner; sign : integer) : TSBWArray;
    function getDoubleArray (sc : TSBWScanner; sign : integer) : TSBWArray;
    function getComplexArray (sc : TSBWScanner) : TSBWArray;
    function getStringArray (sc : TSBWScanner) : TSBWArray;
    function getListArray (sc : TSBWScanner) : TSBWArray;

    procedure getConstantValue (sc : TSBWScanner; var realValue : double; var complexValue : TSBWComplex; var isComplex : boolean);
    function getArrayRow (sc : TSBWScanner) : TSBW1DDoubleArray;
    //function getArray (sc : TScanner) : TSBW2DDoubleArray; overload;
    function getArrayMain (sc : TSBWScanner) : TSBWArray; overload;
    function renderListItem (lt : TSBWListItem) : string;
    function displayList (lt : TSBWList) : string;
    function displayArray (Ar : TSBWArray) : string;
    procedure editBoxKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnBoxOnClick (Sender : TObject);
  public
    { Public declarations }
    ModuleId, ServiceId, MethodId : integer;
    methodStr : string;
    argList : string;
    sbw : TSBW;
    //resultMemoBox : TMemo;
    numArgs : integer;
    sigStr : string;
    runSignature : string;
    stopNRun : boolean;
    runningNTimes : boolean;
    edtBox : array of TMemo;
    btnBox : array of TButton;
    cumulativeTime : integer;
    timeTakeForOneRun : integer;
    procedure createFormLayout();
    function  getListItem  (sc : TSBWScanner) : TSBWListItem;
    function  getList (sc : TSBWScanner) : TSBWList;
    function  getListMain (str : string) : TSBWList;
    function  getArray (str : string; var a1 : TSBW1DDoubleArray; var a2 : TSBW2DDoubleArray) : TArrayType; overload;
  end;

var
  frmRunMethod: TfrmRunMethod;

implementation

{$R *.FMX}

procedure TfrmRunMethod.createFormLayout();
var i, dispY : integer; lbl : TLabel; btn : TButton;
const LEFT_EDGE = 10;
begin
  dispY := 10;
  setLength (edtBox, Length (argList));
  setLength (btnBox, Length (argList));
  lblMethodArguments.Left := LEFT_EDGE;
  for i := 2 to Length (argList) do
      begin
      lbl := TLabel.Create (self);
      lbl.Top := dispY;
      lbl.left := LEFT_EDGE;

      lbl.Parent := inputScrollBar;
      edtBox[i-2] := TMemo.Create (self);
      edtBox[i-2].Top := 15 + dispY;
      edtBox[i-2].Left := LEFT_EDGE;
      edtBox[i-2].Width := trunc (0.85*pnlArgs.width);
      edtBox[i-2].Parent := inputScrollBar;
      edtBox[i-2].Anchors := [akLeft,akRight,akTop];
      edtBox[i-2].OnKeyDown := editBoxKeyDown;
      case byte (sigStr[i]) of
          dtByte, dtBoolean, dtInteger, dtDouble, dtComplex:
               edtBox[i-2].Height := 20;
          dtString, dtArray, dtList :
               begin
               edtBox[i-2].ScrollBars := ssVertical;
               edtBox[i-2].Height := 130;
               end;
      end;

      btnBox[i-2] := TButton.Create (self);
      btnbox[i-2].Anchors := [akRight,akTop];
      btnBox[i-2].Top := 15 + dispY;
      btnBox[i-2].Left := LEFT_EDGE + trunc (0.85*pnlArgs.width) + 6;
      btnBox[i-2].Width := 24;
      btnBox[i-2].Parent := inputScrollBar;
      btnBox[i-2].Caption := 'L';
      btnBox[i-2].ShowHint := True;
      btnBox[i-2].Hint := 'Load Data From a File';
      btnBox[i-2].Tag := integer (edtBox[i-2]);   // Store the associated edit box ref in the tag field
      btnBox[i-2].OnClick := btnBoxOnClick;

      inc (dispY, 20 + edtBox[i-2].height);

     case byte (sigStr[i]) of
           dtByte: lbl.caption := 'Byte';
           dtBoolean: lbl.caption := 'Boolean';
           dtInteger: lbl.caption := 'Integer';
           dtDouble : lbl.caption := 'Double';
           dtComplex : lbl.caption := 'Complex';
           dtString : lbl.caption := 'String';
           dtArray : begin lbl.caption := 'Array'; edtBox[i-2].Text := '[]'; edtBox[i-2].SelStart := 1; end;
           dtList : begin lbl.caption := 'List';   edtBox[i-2].Text := '{}'; edtBox[i-2].SelStart := 1; end;
      else
           raise Exception.Create ('Unknown data type in creatFormLayout()');
      end;
      end;
end;


procedure TfrmRunMethod.getConstantValue (sc : TSBWScanner; var realValue : double; var complexValue : TSBWComplex; var isComplex : boolean);
var imagVal, realVal : double;
    sign : integer;
begin
  imagVal := 0; realVal := 0;
  if sc.Token in [tInteger, tDoubleToken, tComplexToken] then
     begin
     case sc.Token of
          tInteger : realVal := sc.TokenInteger;
      tDoubleToken : realVal := sc.TokenFloat;
     tComplexToken : begin
                     imagVal := sc.TokenFloat;
                     isComplex := True;
                     complexValue := complex (0.0, imagVal);
                     exit;
                     end;
     end;

     sc.NextToken;
     if (sc.Token = tPlus) or (sc.Token = tMinus) then
        begin
        if sc.Token = tMinus then sign := -1 else sign := 1;
        sc.NextToken;
        if sc.Token = tComplexToken then
           imagVal := sc.TokenFloat*sign
        else
          raise Exception.Create ('Expecting imaginary part tpo complex number: eg 2+5i');
        sc.nextToken;
        isComplex := True;
        end
     else
        begin
        isComplex := False;
        realValue := realVal;
        end;
        
     if imagVal = 0 then
        realValue := realVal
     else complexValue := complex (realVal, imagVal);
     end
end;


function TfrmRunMethod.getArrayRow (sc : TSBWScanner) : TSBW1DDoubleArray;
var value : double; nColumns : integer;
    i : integer;
    bracketFound : boolean;
begin
  nColumns := 1;
  i := 0;
  bracketFound := False;
  if sc.Token = tLBracket then
     begin
     sc.NextToken;
     bracketFound := True;
     end;

  case sc.Token of
   tDoubleToken : value := sc.TokenFloat;
       tInteger : value := sc.TokenInteger;
       tString  :
    else
      showmessage ('Error in scanning array spec');
  end;
  setLength (result, nColumns); inc (nColumns);
  result[i] := value; inc (i);
  sc.NextToken;

  while sc.Token = tCOMMA do
        begin
        sc.NextToken;
        case sc.Token of
    tDoubleToken   : value := sc.TokenFloat;
           tInteger : value := sc.TokenInteger;
        else
          showmessage ('Error in scanning array spec');
        end;
        setLength (result, nColumns); inc (nColumns);
        result[i] := value; inc (i);
        sc.NextToken;
        end;

  if sc.Token <> tRBracket then
    raise ESBWException.Create ('Expecting closing square bracket in array argument');

  if bracketFound then
      sc.NextToken;
end;


function TfrmRunMethod.getIntArrayRow (sc : TSBWScanner; sign : integer) : TSBW1DIntArray;
var i : integer;
    value : integer;
    nColumns : integer;
begin
  nColumns := 1;
  i := 0;

  //sign := 1;
  //if sc.Token = tMinus then
  //   begin
  //   sign := -1;
  //   sc.NextToken;
  //   end;

  case sc.Token of
       tInteger : value := sc.TokenInteger * sign;
    else
      showmessage ('Only integers permitted in integer array');
  end;
  setLength (result, nColumns); inc (nColumns);
  result[i] := value; inc (i);
  sc.NextToken;

  while sc.Token = tCOMMA do
        begin
        sc.NextToken;
        sign := 1;
        if sc.Token = tMinus then
           begin
           sign := -1;
           sc.NextToken;
           end;

        case sc.Token of
           tInteger : value := sc.TokenInteger*sign;
        else
          showmessage ('Only integers permitted in integer array');
        end;
        setLength (result, nColumns); inc (nColumns);
        result[i] := value; inc (i);
        sc.NextToken;
        end;
end;



function TfrmRunMethod.getDoubleArrayRow (sc : TSBWScanner; sign : integer) : TSBW1DDoubleArray;
var i : integer;
    value : double;
    nColumns : integer;
begin
  nColumns := 1;
  i := 0;

  //sign := 1;
  //if sc.Token = tMinus then
  //   begin
  //   sign := -1;
  //   sc.NextToken;
  //   end;

  case sc.Token of
 tDoubleToken   : value := sc.TokenFloat;
       tInteger : value := sc.TokenInteger;
    else
      showmessage ('Expecting integer or double while scaning value, found instead [' + sc.TokenToString (sc.Token) + ']');
  end;
  value := value * sign;
  setLength (result, nColumns); inc (nColumns);
  result[i] := value; inc (i);
  sc.NextToken;

  while sc.Token = tCOMMA do
        begin
        sc.NextToken;
        sign := 1;
        if sc.Token = tMinus then
           begin
           sign := -1;
           sc.NextToken;
           end;

        case sc.Token of
     tDoubleToken   : value := sc.TokenFloat;
           tInteger : value := sc.TokenInteger;
        else
          showmessage ('Expecting integer or double while scaning value, found instead [' + sc.TokenToString (sc.Token) + ']');
        end;
        value := value * sign;
        setLength (result, nColumns); inc (nColumns);
        result[i] := value; inc (i);
        sc.NextToken;
        end;
end;


function TfrmRunMethod.getComplexArrayRow (sc : TSBWScanner) : TSBW1DComplexArray;
begin
  raise Exception.Create ('getComplexArrayRow not implemented');
end;


function TfrmRunMethod.getStringArrayRow (sc : TSBWScanner) : TSBW1DStrArray;
var i : integer;
    value : string;
    nColumns : integer;
begin
  nColumns := 1;
  i := 0;

  case sc.Token of
       tString  : value := sc.TokenString;
  else
      showmessage ('Expecting string item in array');
  end;
  setLength (result, nColumns); inc (nColumns);
  result[i] := value; inc (i);
  sc.NextToken;

  while sc.Token = tCOMMA do
        begin
        sc.NextToken;
        case sc.Token of
             tString  : value := sc.TokenString;
        else
          showmessage ('Expecting string item in array');
        end;
        setLength (result, nColumns); inc (nColumns);
        result[i] := value; inc (i);
        sc.NextToken;
        end;
end;


function TfrmRunMethod.getIntArray (sc : TSBWScanner; sign : integer) : TSBWArray;
var a : TSBW1DIntArray; nRows : integer;
begin
  nRows := 0;
  result := TSBWArray.Create;
  result.dType := dtInteger;
  if sc.Token = tRBracket then
     exit;

  result.nDimensions := 1;
  a := getIntArrayRow (sc, sign);
  sc.nextToken;
  if sc.Token <> tCOMMA then
     begin
     result := TSBWArray.Create (a);
     exit;
     end;

  if sc.Token = tComma then
     begin
     result.nDimensions := 2;
     setLength (result.Int2, 1, length (a));
     result.Int2[0] := a;
     end;

  while sc.Token = tComma do
     begin
     sc.nextToken;
     inc (nRows);
     while sc.Token = tLBracket do
           begin
           sc.NextToken;
           if sc.Token = tMinus then
              begin
              sc.nextToken;
              sign := -1;
              end
           else sign := 1;

           a := getIntArrayRow (sc, sign);
           setLength (result.Int2, nRows+1, length (a));
           result.Int2[nRows] := a;
           end;
     sc.nextToken;
     end;
  case result.nDimensions of
     1 : begin
         setLength (result.nElements, 1);
         result.nElements[0] := length (a);
         end;
     2 : begin
         setLength (result.nElements, 2);
         result.nElements[0] := length (result.Int2);
         result.nElements[1] := length (result.Int2[0]);
         end;
  end;
end;


function TfrmRunMethod.getDoubleArray (sc : TSBWScanner; sign : integer) : TSBWArray;
var a : TSBW1DDoubleArray; nRows : integer;
begin
  nRows := 0;
  result := TSBWArray.Create;
  result.dType := dtDouble;
  if sc.Token = tRBracket then
     exit;

  result.nDimensions := 1;
  a := getDoubleArrayRow (sc, sign);
  sc.nextToken;
  if sc.Token <> tCOMMA then
     begin
     result := TSBWArray.Create (a);
     setLength (a, 0);
     exit;
     end;

  if sc.Token = tComma then
     begin
     result.nDimensions := 2;
     setLength (result.Double2, 1, length (a));
     result.Double2[0] := a;
     end;

  while sc.Token = tComma do
     begin
     sc.nextToken;
     inc (nRows);
     while sc.Token = tLBracket do
           begin
           sc.NextToken;
           if sc.Token = tMinus then
              begin
              sc.nextToken;
              sign := -1;
              end
           else sign := 1;

           a := getDoubleArrayRow (sc, sign);
           setLength (result.Double2, nRows+1, length (a));
           result.Double2[nRows] := a;
           end;
     sc.nextToken;
     end;
  case result.nDimensions of
     1 : begin
         setLength (result.nElements, 1);
         result.nElements[0] := length (a);
         end;
     2 : begin
         setLength (result.nElements, 2);
         result.nElements[0] := length (result.Double2);
         result.nElements[1] := length (result.Double2[0]);
         end;
  end;
end;


function TfrmRunMethod.getComplexArray (sc : TSBWScanner) : TSBWArray;
begin
  raise ESBWException.Create ('Complex Arrays not yet implemented!');
end;


function TfrmRunMethod.getStringArray (sc : TSBWScanner) : TSBWArray;
var a : TSBW1DStrArray; nRows : integer;
begin
  nRows := 0;
  result := TSBWArray.Create;
  result.dType := dtString;
  if sc.Token = tRBracket then
     exit;

  a := getStringArrayRow (sc);
  sc.nextToken;
  if sc.Token <> tCOMMA then
     begin
     result := TSBWArray.Create (a);
     exit;
     end;

  if sc.Token = tLBracket then
     begin
     inc (nRows);
     setLength (result.Str2, nRows, length (a));
     result.Str2[nRows-1] := a;
     while sc.Token = tLBracket do
           begin
           sc.NextToken;
           a := getStringArrayRow (sc); inc (nRows);
           setLength (result.Str2, nRows, length (a));
           result.Str2[nRows-1] := a;
           end;
     end;
end;


// Gets an array of lists
function TfrmRunMethod.getListArray (sc : TSBWScanner) : TSBWArray;
var lt : TSBWList;
begin
  result := TSBWArray.Create;
  result.dType := dtList;
  sc.nextToken;
  lt := TSBWList.Create;
  while sc.Token <> tRcBracket do
      begin
      case sc.Token of
           tString : lt.Add(TSBWListItem.Create (sc.tokenString));
          tInteger : lt.Add (TSBWListItem.Create (sc.TokenInteger));
      tDoubleToken : lt.Add (TSBWListItem.Create (sc.TokenFloat));
          tLBracket :
               begin
               lt.Add (TSBWListItem.Create (getArrayMain(sc)));
               if (sc.token = tRBracket) then
                  sc.nextToken;
               end;
      end;
      sc.nextToken;
      if (sc.token = tComma) then
          sc.nextToken;
      end;
end;


function TfrmRunMethod.getArrayMain (sc : TSBWScanner) : TSBWArray;
var i1 : TSBW1DIntArray;
    i2 : TSBW2DIntArray;
    a1 : TSBW1DDoubleArray;
    a2 : TSBW2DDoubleArray;
    c1 : TSBW1DComplexArray;
    c2 : TSBW2DComplexArray;
    s1 : TSBW1DStrArray;
    s2 : TSBW2DStrArray;
    sign : integer;
begin
  sc.nextToken;
  if sc.Token = tLBracket then
     sc.nextToken;

  if sc.Token = tMinus then
     begin
     sc.nextToken;
     sign := -1;
     end
  else sign := 1;

  case sc.Token of
        tInteger  : result := getIntArray (sc, sign);
   tDoubleToken   : result := getDoubleArray (sc, sign);
    tComplexToken : result := getComplexArray (sc);
         tString  : result := getStringArray (sc);
        tRBracket : result := TSBWArray.Create (i1);
        tLcBracket: result := getListArray (sc);
     else
        raise ESBWException.Create ('Only int, double, complex and string arrays supported');
   end;
end;


function TfrmRunMethod.getListItem (sc : TSBWScanner) : TSBWListItem;
var realVal : double;
    imagVal : double;
    sign : integer;
    complexValue : TSBWComplex;
    realValue : double;
    isComplex : boolean;
begin
  if sc.Token in [tInteger, tDoubleToken, tComplexToken] then
     begin
     getConstantValue (sc, realValue, complexValue, isComplex);
     if isComplex then
        result := TSBWListItem.Create (complexValue)
     else result := TSBWListItem.Create (realValue);
     end
  else
     begin
     case sc.Token of
         tString : result := TSBWListItem.Create(sc.TokenString);
       tLBracket : result := TSBWListItem.Create(getArrayMain (sc));
      tLcBracket : result := TSBWListItem.Create(getList (sc));
     end;
     sc.NextToken;
     end;
end;


function TfrmRunMethod.getList (sc : TSBWScanner) : TSBWList;
begin
  result := TSBWList.Create;
  sc.NextToken;
  if sc.Token = tRcBracket then exit;
  result.Add (getListItem (sc));
  while sc.Token = tCOMMA do
        begin
        sc.NextToken;
        result.Add(getListItem (sc));
        end;
  if sc.Token <> tRcBracket then
     raise ESBWException.Create ('Expecting closing square bracket, '']'' in list argument');
end;


function TfrmRunMethod.getListMain (str : string) : TSBWList;
var sc : TSBWScanner;
begin
  sc := TSBWScanner.create;
  sc.SetStream (TStringStream.Create(str));
  sc.NextToken;
  if sc.Token = tLcBracket then
     result := getList (sc)
  else result := TSBWList.Create;
  sc.free;
end;


// OLD OLD OLD OLD OLD !!!!

function TfrmRunMethod.getArray (str : string; var a1 :  TSBW1DDoubleArray; var a2 : TSBW2DDoubleArray) : TArrayType;
var sc : TSBWScanner; aa : array[0..63] of double; i, j, k, nColumns, nRows, colCount : integer; sign : double;
begin
  sign := 1;
  sc := TSBWScanner.create;
  sc.SetStream (TStringStream.Create(str));
  sc.NextToken;
  k := -1; colCount := -1; nRows := 0;
  result := at1D;
  if sc.Token = tLBracket then
     sc.NextToken
  else raise Exception.Create ('Expecting ''['' bracket to start array');
  
  while (sc.Token <> tEnd) and (sc.Token <> tRBracket) do
     begin
     inc (k); inc (colCount);
     case sc.Token of
       tMinus   : begin sign := -1; dec (colCount); dec (k); end;
 tDoubleToken   : begin aa[k] := sc.TokenFloat*sign; sign := 1; end;
       tInteger : begin aa[k] := sc.TokenInteger*sign; sign := 1; end;
     tSemiColon : begin
                  result := at2D;
                  nColumns := colCount;
                  colCount := -1;
                  dec (k);
                  inc (nRows);
                  end;
     else
       showmessage ('Error in scanning array spec, unknown token in stream: ' + sc.TokenToString (sc.Token));
     end;
     sc.NextToken;
     if sc.Token = tComma then
        sc.NextToken;
     end;
   case result of
       at1D :  begin
               setLength (a1, k+1);
               for i := 0 to k do
                   a1[i] := aa[i];
               end;
       at2D :  begin
               setLength (a2, nRows+1, nColumns);
               for i := 0 to nRows do
                   for j := 0 to nColumns - 1 do
                       a2[i,j] := aa[i*(nRows+1) + j];
               end;
   end;
   sc.free;
end;


function TfrmRunMethod.renderListItem (lt : TSBWListItem) : string;
begin
  case lt.DataType of
        dtInteger : result := inttostr (lt.i);
        dtDouble  : result := floattostr (lt.d);
        dtComplex : result := complexToStr (lt.c);
        dtString  : result := '"' + lt.str + '"';
        dtArray   : result := displayArray (lt.Ar);
        dtList    : result := displayList (lt.Lt);
   else result := 'Unsupported Type: ' + convertSigByte (lt.DataType);
   end;
end;


function TfrmRunMethod.displayArray (Ar : TSBWArray) : string;
var i, j : integer; lt : TSBWList;
begin
  result := '[';
  case Ar.nDimensions of
     1 : begin
         case ar.dType of
            dtInteger :
               begin
               for i := 0 to Length (ar.Int1) - 1 do
                   begin
                   result := result + inttostr (ar.getInt (i));
                   if i <> Length (ar.Int1) - 1 then
                      result := result + ', ';
                   end;
               end;

            dtBoolean :
               begin
               for i := 0 to Length (ar.Bool1) - 1 do
                   begin
                   result := result + booleanToStr (ar.getBoolean (i));
                   if i <> Length (ar.Bool1) - 1 then
                      result := result + ', ';
                   end;
               end;

            dtDouble :
               begin
               for i := 0 to length (Ar.Double1) - 1 do
                   begin
                   result := result + floattostr (Ar.getScalar(i));
                   if i <> Length (ar.Double1) - 1 then
                      result := result + ', ';
                   end;
               end;

            dtComplex :
               begin
               for i := 0 to length (Ar.Complex1) - 1 do
                   begin
                   result := result + complexToStr (Ar.getComplex(i));
                   if i <> Length (ar.Complex1) - 1 then
                      result := result + ', ';
                   end;
               end;

            dtString :
               begin
               for i := 0 to Length (ar.Str1) - 1 do
                   begin
                   result := result + ar.getString(i);
                   if i <> Length (ar.Str1) - 1 then
                      result := result + ', ';
                   end;
               end;

            dtList :
               begin
               for i := 1 to Length (ar.List1) - 1 do
                   begin
                   result := result + displayList (ar.getList(i) as TSBWList);
                   if i <> (ar.List1[i] as TSBWList).Count - 1 then
                      result := result + displayList (lt);
                   end;
               end;
            else
              raise ESBWException.Create('Unsupported type while diplaying 1D array: ' + convertSigByte(ar.dType)); 
         end;
         end;
     2 : begin
         for i := 0 to Ar.getNumRows - 1 do
             begin
             result := result + '[';
             if Ar.getNumCols > 0 then
                result := result + floattostr (Ar.getScalar(i,0));

             for j := 1 to Ar.getNumCols - 1 do
                 begin
                 result := result + ', ' + floattostr (Ar.getScalar(i,j));
                 end;
             result := result + ']';
             if i < Ar.getNumRows - 1 then result := result + ', ';
             end;
         end;
  end;
  result := result + ']';
end;


function TfrmRunMethod.displayList (lt : TSBWList) : string;
var i : integer;
begin
  if lt.Count = 0 then exit;

  result := '{' + renderListItem (lt[0]);
  for i := 1 to lt.Count - 1 do
      result := result + ', ' + renderListItem (lt[i]);
  result := result + '}';
end;


function TfrmRunMethod.getCallArguments : TSBWDataStream;
var i : integer;
    sc : TSBWScanner;
    ar : TSBWArray;
    lt : TSBWList;
    argumentStr : string;
begin
  result := TSBWDataStream.Create;
  for i := 2 to Length (argList) do
      begin
      if chkMethodArgString.Checked then
         argumentStr := edtBox[i-2].Text
      else argumentStr := Trim (StringReplace (edtBox[i-2].Text, #13#10, '', [rfReplaceAll]));
      case integer (argList[i]) of
          dtBoolean : begin
                     if UpperCase (argumentStr) = 'TRUE' then
                        result.add (True)
                     else
                     if UpperCase (argumentStr) = 'FALSE' then
                        result.add(False)
                     else
                     if strtoInt (argumentStr) = 1 then
                        result.add(True)
                     else result.add(False);
                     end;
          dtInteger : result.Add(strtoInt (argumentStr));
          dtDouble  : result.Add(strtofloat (argumentStr));
          dtComplex : result.Add(strToComplex (argumentStr));
          dtString  : result.Add(argumentStr);
          dtArray   : begin
                      sc := TSBWScanner.Create;
                      try
                        sc.SetStream (TStringStream.Create(argumentStr));
                        sc.NextToken;
                        ar := getArrayMain (sc);
                        try
                          result.Add (ar);
                        finally
                          ar.Free;
                        end;
                      finally
                        sc.Free;
                      end;
                    end;
          dtList    : begin
                      lt := getListMain (argumentStr);
                      try
                        result.add(lt);
                      finally
                        lt.Free;
                      end;
                      end;
       else
          showmessage ('Argument type currently not supported');
       end;
       end;
end;


procedure TfrmRunMethod.btnRunClick(Sender: TObject);
var m : TSBWCallObject; r : TSBWCallResult;
    ds : TSBWDataStream;
    a1 : TSBW1DDoubleArray;
    a2 : TSBW2DDoubleArray;
    i, j : integer; ar : TSBWArray;
    arrayType : TArrayType;
    lt : TSBWList;
    str : string;
    sc : TSBWScanner;
    leftArrayDelim, rightArrayDelim : string;
    start : DWORD; // Timing variable
begin
   if chkCSVArrays.checked then
      begin
      leftArrayDelim := '';
      rightArrayDelim := '';
      end
   else
      begin
      leftArrayDelim := '[';
      rightArrayDelim := ']';
      end;

   if (runSignature = 'void shutdownBroker()') and (ModuleId = -1) then
      begin
      sbw.broker.shutdownBroker;
      exit;
      end;

   m := sbw.getMethodObj (ModuleId, ServiceId, MethodId);
   try
     resultMemoBox.Clear;
     try
       ds := getCallArguments;
     except
       on e: Exception do
          begin
          ds.Free;
          lblStatus.Caption := e.message;
          Showmessage (e.message);
          Exit;
          end;
     end;


    if not runningNTimes then cumulativeTime := 0;
    start := GetTickCount();

     r := m.Call(ds);

     timeTakeForOneRun := GetTickCount() - start;
     edtTiming.Text := inttostr (timeTakeForOneRun);

     if not chkSuppressOutput.checked then
     
     case integer (argList[1]) of
          dtVoid    : resultMemoBox.Text := '';
          dtInteger : resultMemoBox.Text := inttostr (r.getInteger);
          dtBoolean : resultMemoBox.Text := booleanToStr (r.getBoolean);
          dtDouble  : resultMemoBox.Text := floattostr (r.getDouble);
          dtComplex : resultMemoBox.Text := complexToStr (r.getComplex);
          dtString  : resultMemoBox.Text := r.getString;
          dtArray   : begin
                      if r.dataType <> dtArray then
                         raise ESBWException.Create ('Expecting an array in the return type for this call but found instead: ' + convertSigByte (r.dataType));
                      ar := r.getArray;
                      case ar.nDimensions of
                         1 : begin
                             str := leftArrayDelim;
                             case ar.dType of
                                dtDouble :
                                   begin
                                   for i := 0 to Length (ar.Double1) - 1 do
                                       begin
                                       str := str + floattostr (ar.getDouble(i));
                                       if i <> Length (ar.Double1) - 1 then
                                          begin
                                          str := str + ', ';
                                          if chkCSVArrays.Checked then
                                             str := str + #13#10;
                                          end;
                                       end;
                                   end;
                                dtComplex :
                                   begin
                                   for i := 0 to Length (ar.Complex1) - 1 do
                                       begin
                                       str := str + complexToStr (ar.getComplex(i));
                                       if i <> Length (ar.Complex1) - 1 then
                                          str := str + ', ';
                                       end;
                                   end;
                                dtInteger :
                                   begin
                                   for i := 0 to Length (ar.Int1) - 1 do
                                       begin
                                       str := str + inttostr (ar.getInt(i));
                                       if i <> Length (ar.Int1) - 1 then
                                          begin
                                          str := str + ', ';
                                          if chkCSVArrays.Checked then
                                             str := str + #13#10;
                                          end;
                                       end;
                                   end;
                                dtBoolean :
                                   begin
                                   for i := 0 to Length (ar.Bool1) - 1 do
                                       begin
                                       str := str + booleanTostr (ar.getBoolean(i));
                                       if i <> Length (ar.Bool1) - 1 then
                                          str := str + ', ';
                                       end;
                                   end;
                                dtString :
                                   begin
                                   if Length (ar.Str1) > 0 then
                                      begin
                                      str := str + ar.getString (0);
                                      for i := 1 to Length (ar.Str1) - 1 do
                                          begin
                                          str := str + ', ' + ar.getString(i);
                                          end;
                                      end;
                                   end;
                                dtList :
                                   begin
                                   if ar.nElements[0] > 0 then
                                      begin
                                      str := str + displayList (ar.getList(0) as TSBWList);
                                      for i := 1 to Length (ar.List1) - 1 do
                                          str := str + ', ' + displayList (ar.getList(i) as TSBWList);
                                      end;
                                   end
                                else
                                  raise ESBWException.Create ('Unsupported type in array: ' + convertSigByte (ar.dType));
                             end;
                             str := str + rightArrayDelim;;
                             resultMemoBox.Lines.Add (str);
                             end;
                         2 : begin
                             str := leftArrayDelim;
                             for i := 0 to ar.getNumRows - 1 do
                                 begin
                                 if i > 0 then
                                    str := leftArrayDelim
                                 else
                                    str := str + leftArrayDelim;
                                 for j := 0 to ar.getNumCols - 1 do
                                     begin
                                     case ar.dType of
                                         dtInteger : str := str + floattostr (ar.getInt(i,j)) + ' ';
                                         dtBoolean : str := str + booleantostr (ar.getBoolean (i, j)) + ' ';
                                         dtDouble  : begin
                                                     if pos ('.', Format ('%g', [ar.getDouble(i,j)])) = 0 then
                                                        str := str + Format ('%g', [ar.getDouble(i,j)]) + '.0'
                                                     else str := str + Format ('%g', [ar.getDouble(i,j)]) + ' ';
                                                     end;
                                         dtString  : str := str + ar.getString(i,j) + ' ';
                                         dtComplex : str := str + complexToStr (ar.getComplex(i,j)) + ' ';
                                     else showmessage ('Type: ' + convertSigByte(ar.dType) + ' not supported for display in 2D arrays');
                                     end;
                                     if j < ar.getNumCols - 1 then
                                        str := str + ', ';
                                     end;
                                 if i < ar.getNumRows - 1 then
                                    str := str + rightArrayDelim + ', '
                                 else str := str + rightArrayDelim + rightArrayDelim;
                                 resultMemoBox.Lines.Add (str);
                                 end;
                             end;
                         else
                            showmessage ('Array Dimensions higher than 2 are not supported');
                      end;
                      end;
          dtList    : begin
                      lt := r.getList;
                      if lt = nil then
                         raise Exception.Create ('Error while extracting list, list is empty?');
                      try
                        resultMemoBox.Lines.Add ('List: ' + displayList (lt));
                      finally
                        lt.Free;
                      end;
                      end;
     else
         showmessage ('Result type currently not supported');
     end;
     lblStatus.Caption := 'Completed Run';
  finally
    ds.Free;
    m.Free;
  end;
end;


procedure TfrmRunMethod.FormShow(Sender: TObject);
begin
  if Length (argList) > 1 then
     edtBox[0].SetFocus
end;


procedure TfrmRunMethod.btnBoxOnClick (Sender : TObject);
var memo : TMemo;
begin
  if OpenDialog.execute then
     begin
     memo := TMemo ((Sender as TButton).Tag);
     memo.Lines.LoadFromFile (openDialog.FileName);
     end;
end;


procedure TfrmRunMethod.editBoxKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  with (Sender as TMemo) do
  begin
    if ssCtrl in Shift then
    begin
      case Key of
        Ord('A'): SelectAll;  // THE IFFY BIT
        Ord('C'): CopyToClipboard;
        Ord('X'): CutToClipboard;
      else
       Key := word (#0);
      end; // case Key of
    end; // if ssCtrl in Shift then
  end; // with memSignature do
end;


procedure TfrmRunMethod.resultMemoBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  with resultMemoBox do
  begin
    if ssCtrl in Shift then
    begin
      case Key of
        Ord('A'): SelectAll;  // THE IFFY BIT
        Ord('C'): CopyToClipboard;
        Ord('X'): CutToClipboard;
      end; // case Key of
    Key := 0;
    end; // if ssCtrl in Shift then
  end; // with memSignature do
end;

procedure TfrmRunMethod.btnCopyToClipboardClick(Sender: TObject);
begin
  resultMemoBox.SelectAll;
  resultMemoBox.CopyToClipboard;
end;


procedure TfrmRunMethod.btnHelpClick(Sender: TObject);
begin
  MessageDlg ('Arrays: [1,2,3], [[1,2], [3,4]]' + #13#10 +
               'Lists: {[1,2i,3], 1, "string", [1,2,3.4+6i]}', mtInformation,
      [mbOk], 0);
end;

procedure TfrmRunMethod.btnSaveClick(Sender: TObject);
begin
  if SaveDialog.execute then
     begin
     resultMemoBox.SelectAll;
     resultMemoBox.Lines.SaveToFile(SaveDialog.FileName);
     end;
end;

procedure TfrmRunMethod.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = vk_escape then Close;
end;

procedure TfrmRunMethod.btnRunNTimesClick(Sender: TObject);
var i, n : integer;
    data : array of double;
    Mean, StdDev : double;
begin
  cumulativeTime := 0;
  btnStop.Visible := true;
  stopNRun := False;
  runningNTimes := True;
  n := strtoint (edtRunNTimes.Text);
  setLength (data, n);
  i := 0;
  if n > 0 then
     begin
     while (n >= 1) and not stopNRun do
         begin
         lblnTimes.Caption := inttostr (i);
         lblnTimes.refresh;
         btnRunClick (Sender);
         data[i] := timeTakeForOneRun;
         cumulativeTime := cumulativeTime + timeTakeForOneRun;
         Application.ProcessMessages;
         dec (n);
         inc (i);
         end;
     end;
   edtTiming.Text := inttostr (cumulativeTime);
   //MeanAndStdDev(const Data: array of Double; var Mean, StdDev: Double);
   MeanAndStdDev (data, Mean, StdDev);
   edtMean.Text := floattostr (Mean);
   edtSD.Text := floattostr (StdDev);
   runningNTimes := False;
   setLength (data, 0);
end;

procedure TfrmRunMethod.btnStopClick(Sender: TObject);
begin
  stopNRun := True;
end;

procedure TfrmRunMethod.FormCreate(Sender: TObject);
begin
  stopNRun := True;
end;

procedure TfrmRunMethod.btnRunAndCloseClick(Sender: TObject);
begin
  btnRunClick(Sender);
  Close;
end;

end.
