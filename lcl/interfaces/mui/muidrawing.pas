{
 *****************************************************************************
 *                             muiObjects.pas                              *
 *                              --------------                               *
 *      Place for wrapper classes which aren't widgets                       *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit muidrawing;

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL, LCL
  Classes, SysUtils, types,
  Graphics, Menus, LCLType,
  // Widgetset
  // aros
  agraphics, intuition, mui;

type

  TMUIRegionType=(eRegionNULL,eRegionSimple,eRegionComplex,eRegionNotCombinableOrError);
  TMUIRegionCombine=(eRegionCombineAnd,eRegionCombineCopy, eRegionCombineDiff, eRegionCombineOr, eRegionCombineXor);

  TMUIWinAPIElement = class(TObject);

  TMUIWinAPIObject = class(TMUIWinAPIElement);

  TMUIColor = longword;

  type tagTmuiBrush= record
    Color: TMUIColor;
  end;

  type tagTmuiPen= record
    Color: TMUIColor;
    Width: Integer;
  end;

  { TMUIColorObj }

  TMUIColorObj = class(TMUIWinAPIObject)
  private
    FLCLColor: TColor;
    function GetLCLColor: TColor;
    procedure SetLCLColor(AValue: TColor);
    function GetColor: LongWord;
  public
    property LCLColor: TColor read GetLCLColor write SetLCLColor;
    property Color: LongWord read GetColor;
  end;

  { TMUIPenObj }

  TMUIPenObj = class(TMUIColorObj)
  public
    constructor Create(const APenData: TLogPen);
  end;

  { TMUIBrushObj }

  TMUIBrushObj = class(TMUIColorObj)
  private
    FStyle: LongWord;
  public
    constructor Create(const ABrushData: TLogBrush);
    property Style: LongWord read FStyle;
  end;

  (*
  { TmuiWinAPIBrush }

  TmuiWinAPIBrush = class (TmuiWinAPIObject)
  private
    FBrush: tagTmuiBrush;
    function GetColor: TMUIColor;
    procedure SetColor(const AValue: TMUIColor);
  public
    property Color: TMUIColor read GetColor Write SetColor;
    Constructor Create;
    Constructor Create(const ABrushData: TLogBrush);
    Destructor Destroy; override;
  end;

  { TmuiWinAPIPen }

  TmuiWinAPIPen = class (TmuiWinAPIObject)
  private
    FPen: tagTmuiPen;
    function GetColor: TMUIColor;
    procedure SetColor(const AValue: TMUIColor);
  public
    property Color: TMUIColor read GetColor Write SetColor;
    Constructor Create;
    Constructor Create(const APenData: TLogPen);
    Destructor Destroy; override;
  end;

  { TmuiWinAPIFont }

  TmuiWinAPIFont = class (TmuiWinAPIObject)
  private
    fpgFont: TfpgFontBase;
    FFontHeight: integer;
    function GetFontHeight: integer;
    function GetFontSize: integer;
    procedure SetFontHeight(const AValue: integer);
    procedure SetFontSize(const AValue: integer);
  public
    FontFace: String;
    Constructor Create;
    Constructor Create(const AFontData: TFontData);
    Constructor Create(const AfpgCanvas: TfpgCanvas);
    Constructor Create(const AFontData: TLogFont);
    Constructor Create(const AFontData: TLogFont; const ALongFontName: string);
    Destructor Destroy; override;
    property muiFont: TfpgFontBase read fpgFont write fpgFont;
    property Size: integer read GetFontSize write SetFontSize;
    property Height: integer read GetFontHeight write SetFontHeight;
  end; *)

 (* { TmuiWinAPIBitmap }

  TmuiWinAPIBitmap = class(TmuiWinAPIObject)
  private
    fpgImage: TfpgImage;
  protected
    SelectedInDC: HDC;
  public
    Constructor Create(const ABitsPerPixel,Width,Height: integer);
    Destructor Destroy; override;
    property Image: TfpgImage read fpgImage;
  end;   *)

  TmuiBasicRegion = class;

  (*
  { TmuiDeviceContext }

  TmuiDeviceContext = class(TmuiWinAPIElement)
  private
    FDCStack: array of TmuiDeviceContext;
    procedure CopyDCToInstance(const ATarget: TmuiDeviceContext);
    procedure SetupFont;
    procedure SetupBrush;
    procedure SetupBitmap;
    procedure SetupClipping;
  public
    fpgCanvas: TfpgCanvas;
    FPrivateWidget: TmuiPrivateWidget;
    FOrg: TPoint;
    FBrush: TmuiWinAPIBrush;
    FPen: TmuiWinAPIPen;
    FFont: TmuiWinAPIFont;
    FTextColor: TMUIColor;
    FBitmap: TmuiWinAPIBitmap;
    FClipping: TmuiBasicRegion;
  public
    constructor Create(AmuiPrivate: TmuiPrivateWidget);
    destructor Destroy; override;
    procedure SetOrigin(const AX,AY: integer);
    function SaveDC: integer;
    function RestoreDC(const Index: SizeInt): Boolean;
    function SelectObject(const AGDIOBJ: HGDIOBJ): HGDIOBJ;
    function SetTextColor(const AColor: TColorRef): TColorRef;
    function PrepareRectOffsets(const ARect: TRect): TfpgRect;
    procedure ClearRectangle(const AfpgRect: TfpgRect);
    procedure ClearDC;
    procedure SetupPen;
  end; *)

  (*
  { TmuiPrivateMenuItem }

  TmuiPrivateMenuItem = class(TObject)
  private
  protected
  public
    MenuItem: TfpgMenuItem;
    LCLMenuItem: TMenuItem;
    procedure HandleOnClick(ASender: TObject);
  end; *)

  { TmuiBasicRegion }

  TmuiBasicRegion=class(TmuiWinAPIObject)
  private
    FRegionType: TmuiRegionType;
    //function GetfpgRectRegion: TfpgRect;
    function GetRegionType: TmuiRegionType;
  protected
    FRectRegion: TRect;
  public
    constructor Create; overload;
    constructor Create(const ARect: TRect); overload;
    destructor Destroy; override;
    procedure CreateRectRegion(const ARect: TRect);
    function CombineWithRegion(const ARegion: TmuiBasicRegion; const ACombineMode: TmuiRegionCombine): TmuiBasicRegion;
    function Debugout: string;
    property RegionType: TmuiRegionType read GetRegionType;

  end;

  { TMUICanvas }

  TMUICanvas = class
  private
    FBrush: TMUIBrushObj;
    FPen: TMUIPenObj;
    FDefaultPen: TMUIPenObj;
    FDefaultBrush: TMUIBrushObj;
  public
    RastPort: PRastPort;
    DrawRect: TRect;
    Position: TPoint;
    RenderInfo: PMUI_RenderInfo;
    Clipping: TMuiBasicRegion;
    Offset: types.TPoint;
    TextColor: LongWord;
    function GetOffset: TPoint;
    // Drawing routines
    procedure MoveTo(x, y: integer);
    procedure LineTo(x, y: integer);
    procedure WriteText(Txt: PChar; Count: integer);
    function TextWidth(Txt: PChar; Count: integer): integer;
    function TextHeight(Txt: PChar; Count: integer): integer;
    procedure FillRect(X1, Y1, X2, Y2: Integer);
    procedure Rectangle(X1, Y1, X2, Y2: Integer);
    // set a Pen as color
    procedure SetAMUIPen(PenDesc: integer);
    procedure SetPenToRP;
    Procedure SetBrushToRP(AsPen: Boolean = FALSE);
    //
    function SelectObject(NewObj: TMUIWinAPIElement): TMUIWinAPIElement;
    procedure InitCanvas;
    constructor Create;
    destructor Destroy; override;

  end;

  //function muiGetDesktopDC(): TmuiDeviceContext;
  function TColorToMUIColor(col: TColor): TMuiColor;

implementation
uses
  muibaseunit, tagsarray;

(*
var
  muiDesktopDC: TmuiDeviceContext=nil;

function muiGetDesktopDC(): TmuiDeviceContext;
begin
  if not Assigned(muiDesktopDC) then
   muiDesktopDC:=TmuiDeviceContext.Create(nil);
  Result:=muiDesktopDC;
end;
*)

function TColorToMUIColor(col: TColor): TMuiColor;
var
  c: LongWord;
  r: LongWord;
  g: LongWord;
  b: LongWord;
begin
  c := col;
  b := (c and $00FF0000) shr 16;
  g := (c and $0000FF00);
  r := (c and $000000FF) shl 16;
  Result := r or g or b;
end;

{ TMUIBrushObj }

constructor TMUIBrushObj.Create(const ABrushData: TLogBrush);
begin
  inherited Create;
  FLCLColor := ABrushData.lbColor;
  case ABrushData.lbStyle of
    BS_SOLID, BS_HATCHED: FStyle := JAM2;
    BS_HOLLOW: FStyle := JAM1;
    else
      FStyle := JAM2;
  end;
  //writeln('Brush created: $', HexStr(Pointer(FLCLColor)));
end;

{ TMUIPenObj }

constructor TMUIPenObj.Create(const APenData: TLogPen);
begin
  inherited Create;
  FLCLColor := APenData.lopnColor;
  //writeln('pen created: $', HexStr(Pointer(FLCLColor)));
end;

{ TMUIColorObj }

function TMUIColorObj.GetLCLColor: TColor;
begin
  Result := FLCLColor;
end;

procedure TMUIColorObj.SetLCLColor(AValue: TColor);
begin
  FLCLColor := AValue;
end;

function TMUIColorObj.GetColor: LongWord;
begin
  Result := TColorToMUIColor(FLCLColor);
end;

(*
{ TmuiWinAPIBitmap }

constructor TmuiWinAPIBitmap.Create(const ABitsPerPixel, Width,
  Height: integer);
begin
  fpgImage:=TfpgImage.Create;
  fpgImage.AllocateImage(ABitsPerPixel,Width,Height);
  fpgImage.UpdateImage;
end;

destructor TmuiWinAPIBitmap.Destroy;
//var
//  Context: TmuiDeviceContext;
begin
//  Context:=TmuiDeviceContext(SelectedInDC);
//  if Assigned(Context) then begin
//    Context.FBitmap:=nil;
//  end;
  fpgImage.Free;
  inherited Destroy;
end;    *)

(*
{ TmuiDeviceContext }

procedure TmuiDeviceContext.CopyDCToInstance(
  const ATarget: TmuiDeviceContext);
begin
  ATarget.fpgCanvas:=fpgCanvas;
  ATarget.FPrivateWidget:=FPrivateWidget;
  ATarget.FBrush:=FBrush;
  ATarget.FPen:=FPen;
  ATarget.FFont:=FFont;
  ATarget.FOrg:=FOrg;
  ATarget.FTextColor:=FTextColor;
  ATarget.FClipping:=FClipping;
end;

procedure TmuiDeviceContext.SetupFont;
begin
  if Assigned(fpgCanvas) then
   if Assigned(FFont) then
    fpgCanvas.Font:=FFont.muiFont;
end;

procedure TmuiDeviceContext.SetupBrush;
begin
  if Assigned(fpgCanvas) then
   if Assigned(FBrush) then
    fpgCanvas.Color:=FBrush.Color;
end;

procedure TmuiDeviceContext.SetupPen;
begin
  if Assigned(fpgCanvas) then
   if Assigned(FPen) then
    fpgCanvas.Color:=FPen.Color;
end;

procedure TmuiDeviceContext.SetupBitmap;
begin
  if Assigned(fpgCanvas) then
    fpgCanvas.DrawImage(0,0,FBitmap.fpgImage);
end;

procedure TmuiDeviceContext.SetupClipping;
var
  r: TfpgRect;
begin
  if Assigned(fpgCanvas) then
    if Assigned(FClipping) then begin
      r:=FClipping.fpgRectRegion;
      AdjustRectToOrg(r,FOrg);
      fpgCanvas.SetClipRect(r);
    end else begin
      fpgCanvas.ClearClipRect;
    end;
end;

constructor TmuiDeviceContext.Create(AmuiPrivate: TmuiPrivateWidget);
begin
  if Assigned(AmuiPrivate) then begin
    fpgCanvas := AmuiPrivate.Widget.Canvas;
    fpgCanvas.BeginDraw(false);
    AmuiPrivate.DC:=HDC(Self);
    FPrivateWidget := AmuiPrivate;
  end else begin
    fpgCanvas := nil;
    FPrivateWidget := nil;
  end;
  with FOrg do begin
    X:=0;
    Y:=0;
  end;
  FBrush:=nil;
  FPen:=nil;
  FFont:=nil;
end;

destructor TmuiDeviceContext.Destroy;
var
  j: integer;
begin
  if Assigned(fpgCanvas) then fpgCanvas.EndDraw;
  for j := 0 to High(FDCStack) do begin
    FDCStack[j].Free;
  end;
  if Assigned(FPrivateWidget) then
    FPrivateWidget.DC:=0;
end;

procedure TmuiDeviceContext.SetOrigin(const AX, AY: integer);
begin
  With FOrg do begin
    X:=AX;
    Y:=AY;
  end;
end;

function TmuiDeviceContext.SaveDC: Integer;
var
  Tmp: TmuiDeviceContext;
begin
  SetLength(FDCStack,Length(FDCStack)+1);
  Tmp:=TmuiDeviceContext.Create(FPrivateWidget);
  FDCStack[High(FDCStack)]:=Tmp;
  Self.CopyDCToInstance(Tmp);
  Result:=High(FDCStack);
end;

function TmuiDeviceContext.RestoreDC(const Index: SizeInt): Boolean;
var
  Tmp: TmuiDeviceContext;
  TargetIndex: SizeInt;
  j: SizeInt;
begin
  Result:=false;
  if Index>=0 then begin
    TargetIndex:=Index;
    if TargetIndex>High(FDCStack) then Exit;
  end else begin
    TargetIndex:=High(FDCStack)-Index+1;
    If TargetIndex<0 then Exit;
  end;
  Tmp:=FDCStack[TargetIndex];
  Tmp.CopyDCToInstance(Self);
  FPrivateWidget.DC:=HDC(Self);
  SetupFont;
  SetupBrush;
  SetupPen;
  SetupClipping;
  for j := TargetIndex to High(FDCStack) do begin
    FDCStack[j].Free;
  end;
  SetLength(FDCStack,TargetIndex);
  Result:=true;
end;

function TmuiDeviceContext.SelectObject(const AGDIOBJ: HGDIOBJ): HGDIOBJ;
var
  gObject: TObject;
begin
  Result:=0;
  gObject:=TObject(AGDIOBJ);
  if AGDIOBJ<5 then begin
    case AGDIOBJ of
      1:  begin
            Result:=HGDIOBJ(FFont);
            FFont:=nil;
          end;
      2:  begin
            Result:=HGDIOBJ(FBrush);
            FBrush:=nil;
          end;
      3:  begin
            Result:=HGDIOBJ(FPen);
            FPen:=nil;
          end;
      4:  begin
            Result:=HGDIOBJ(FBitmap);
            FBitmap:=nil;
          end;
      5:  begin
            Result:=HGDIOBJ(FClipping);
            FClipping:=nil;
          end;
    end;
    Exit;
  end;
  if gObject is TmuiWinAPIFont then begin
    Result:=HGDIOBJ(FFont);
    FFont:=TmuiWinAPIFont(gObject);
    SetupFont;
    if Result=0 then Result:=1;
  end else if gObject is TmuiWinAPIBrush then begin
    Result:=HGDIOBJ(FBrush);
    FBrush:=TmuiWinAPIBrush(gObject);
    SetupBrush;
    if Result=0 then Result:=2;
  end else if gObject is TmuiWinAPIPen then begin
    Result:=HGDIOBJ(FPen);
    FPen:=TmuiWinAPIPen(gObject);
    SetupPen;
    if Result=0 then Result:=3;
  end else if gObject is TmuiWinAPIBitmap then begin
    Result:=HGDIOBJ(FBitmap);
    FBitmap:=TmuiWinAPIBitmap(gObject);
    FBitmap.SelectedInDC:=HDC(Self);
    SetupBitmap;
    if Result=0 then Result:=4;
  end else if gObject is TmuiBasicRegion then begin
    Result:=HGDIOBJ(FClipping);
    FClipping:=TmuiBasicRegion(gObject);
    SetupClipping;
    if Result=0 then Result:=5;
  end;
end;

function TmuiDeviceContext.SetTextColor(const AColor: TColorRef): TColorRef;
begin
  Result:=FTextColor;
  FTextColor:=AColor;
  fpgCanvas.TextColor:=FTextColor;
end;

function TmuiDeviceContext.PrepareRectOffsets(const ARect: TRect): TfpgRect;
begin
  TRectTofpgRect(ARect,Result);
  AdjustRectToOrg(Result,FOrg);
  FPrivateWidget.AdjustRectXY(Result);
end;

procedure TmuiDeviceContext.ClearRectangle(const AfpgRect: TfpgRect);
var
  OldColor: TMUIColor;
begin
  OldColor:=fpgCanvas.Color;
  fpgCanvas.Color:= FPrivateWidget.Widget.BackgroundColor;
  fpgCanvas.FillRectangle(AfpgRect);
  if fpgCanvas.Color=0 then writeln(FPrivateWidget.LCLObject.Name);
  fpgCanvas.Color:=OldColor;
end;

procedure TmuiDeviceContext.ClearDC;
begin
  ClearRectangle(fpgCanvas.GetClipRect);
end;

{ TmuiPrivateMenuItem }

procedure TmuiPrivateMenuItem.HandleOnClick(ASender: TObject);
begin
  if Assigned(LCLMenuItem) and Assigned(LCLMenuItem.OnClick) then
   LCLMenuItem.OnClick(LCLMenuItem);
end;
*)

{ TmuiWinAPIFont }
(*
function TmuiWinAPIFont.GetFontHeight: integer;
begin
  Result:=FFontHeight;
end;

function TmuiWinAPIFont.GetFontSize: integer;
begin
  Result:=(-FFontHeight * 72) div 96;
end;

procedure TmuiWinAPIFont.SetFontHeight(const AValue: integer);
begin
  FFontHeight:=AValue;
end;

procedure TmuiWinAPIFont.SetFontSize(const AValue: integer);
begin
  FFontHeight:=(-96 * AValue) div 72;
end;

constructor TmuiWinAPIFont.Create;
begin
  FontFace:='';
  Size:=8;
end;

constructor TmuiWinAPIFont.Create(const AFontData: TFontData);
begin
  FontFace:=AFontData.Name;
  Height:=AFontData.Height;
  fpgFont:=fpgGetFont(format('%s-%d',[AFontData.Name,Size]));
end;

constructor TmuiWinAPIFont.Create(const AfpgCanvas: TfpgCanvas);
begin
  fpgFont:=AfpgCanvas.Font;
end;

constructor TmuiWinAPIFont.Create(const AFontData: TLogFont);
begin
  FontFace:=AFontData.lfFaceName;
  Height:=AFontData.lfHeight;
  if FontFace='' then begin
    fpgFont:=fpgGetFont(''); //Default
  end else begin
    fpgFont:=fpgGetFont(format('%s-%d',[FontFace,Size]));
  end;
end;

constructor TmuiWinAPIFont.Create(const AFontData: TLogFont;
  const ALongFontName: string);
begin
  FontFace:=ALongFontName;
  Height:=AFontData.lfHeight;
  if FontFace='' then begin
    fpgFont:=fpgGetFont(''); //Default
  end else begin
    fpgFont:=fpgGetFont(format('%s-%d',[FontFace,Size]));
  end;
end;

destructor TmuiWinAPIFont.Destroy;
begin
  FreeAndNIL(fpgFont);
  inherited Destroy;
end;

{ TmuiWinAPIPen }

function TmuiWinAPIPen.GetColor: TMUIColor;
begin
  Result:=FPen.Color;
end;

procedure TmuiWinAPIPen.SetColor(const AValue: TMUIColor);
begin
  FPen.Color:=AValue;
end;

constructor TmuiWinAPIPen.Create;
begin
end;

constructor TmuiWinAPIPen.Create(const APenData: TLogPen);
begin
  Create;
  FPen.Color:=APenData.lopnColor;
end;

destructor TmuiWinAPIPen.Destroy;
begin
  FreeAndNil(FPen);
  inherited Destroy;
end;

{ TmuiWinAPIBrush }

function TmuiWinAPIBrush.GetColor: TMUIColor;
begin
  if Assigned(Self) then
    Result:=FBrush.Color
  else
    Result:=0;
end;

procedure TmuiWinAPIBrush.SetColor(const AValue: TMUIColor);
begin
  FBrush.Color:=AValue;
end;

constructor TmuiWinAPIBrush.Create;
begin
  FBrush.Color:=TColorToMUIColor(clBtnFace);
end;

constructor TmuiWinAPIBrush.Create(const ABrushData: TLogBrush);
begin
  FBrush.Color:=TColorToMUIColor(ABrushData.lbColor);
end;

destructor TmuiWinAPIBrush.Destroy;
begin
  inherited Destroy;
end;  *)

{ TmuiBasicRegion }

function TmuiBasicRegion.GetRegionType: TmuiRegionType;
begin
  Result:=FRegionType;
end;

{function TmuiBasicRegion.GetfpgRectRegion: TfpgRect;
begin
  //TRectTofpgRect(FRectRegion,Result);
end;}

constructor TmuiBasicRegion.Create;
var
  ARect: TRect;
begin
  FillByte(ARect,sizeof(ARect),0);
  CreateRectRegion(ARect);
end;

constructor TmuiBasicRegion.Create(const ARect: TRect);
begin
  CreateRectRegion(ARect);
end;

destructor TmuiBasicRegion.Destroy;
begin
  inherited Destroy;
end;

procedure TmuiBasicRegion.CreateRectRegion(const ARect: TRect);
begin
  FRectRegion:=ARect;
  if (FRectRegion.Left=FRectRegion.Top) and (FRectRegion.Right=FRectRegion.Bottom) and
    (FRectRegion.Top=FRectRegion.Bottom) then begin
    FRegionType:=eRegionNULL;
  end else begin
    FRegionType:=eRegionSimple;
  end;
end;

function TmuiBasicRegion.Debugout: string;
begin
  Result := '('+IntToStr(FRectRegion.Left) + ', ' + IntToStr(FRectRegion.Top) + ' ; ' + IntToStr(FRectRegion.Right) + ', ' + IntToStr(FRectRegion.Bottom) + ')';
end;

function TmuiBasicRegion.CombineWithRegion(const ARegion: TmuiBasicRegion;
  const ACombineMode: TmuiRegionCombine): TmuiBasicRegion;
  function Min(const V1,V2: SizeInt): SizeInt;
  begin
    if V1<V2 then Result:=V1 else Result:=V2;
  end;
  function Max(const V1,V2: SizeInt): SizeInt;
  begin
    if V1>V2 then Result:=V1 else Result:=V2;
  end;
  procedure CombineAnd(const TargetRegion: TmuiBasicRegion; const r1,r2: TRect);
  var
    Intersect: Boolean;
  begin
    if (r2.Left>r1.Right) or
       (r2.Right<r1.Left) or
       (r2.Top>r1.Bottom) or
       (r2.Bottom<r1.Top) then begin
      Intersect:=false;
    end else begin
      Intersect:=true;
    end;
  	if Intersect then begin
      TargetRegion.CreateRectRegion(
        classes.Rect(
          Max(r1.Left,r2.Left),
          Max(r1.Top,r2.Top),
          Min(r1.Right,r2.Right),
          Min(r1.Bottom,r2.Bottom)
        )
      );
   	end else begin
      TargetRegion.CreateRectRegion(classes.Rect(0,0,0,0));
    end;
  end;
begin
  Result:=TmuiBasicRegion.Create;
  Case ACombineMode of
    eRegionCombineAnd:  CombineAnd(Result,ARegion.FRectRegion,Self.FRectRegion);
    eRegionCombineCopy,
    eRegionCombineDiff:
      begin
        Result.CreateRectRegion(rect(0,0,0,0));
      end;
    eRegionCombineOr,
    eRegionCombineXor:
      begin
        Raise Exception.CreateFmt('Region mode %d not supported',[integer(ACombineMode)]);
      end;
  end;
end;

{ TMUICanvas }

function TMUICanvas.GetOffset: TPoint;
begin
  Result.X := DrawRect.Left + Offset.X;
  Result.Y := DrawRect.Top + Offset.Y;
  //writeln('  GetOffset: ', Result.X);
  if Assigned(Clipping) then
  begin
    Result.X := Result.X + Clipping.FRectRegion.Left;
    Result.Y := Result.Y + Clipping.FRectRegion.Top;
  end;
end;

procedure TMUICanvas.MoveTo(x, y: integer);
begin
  if Assigned(RastPort) then
  begin
    //writeln('MoveTo: ', Assigned(Clipping), ' -> ', GetOffset.X + x,', ', GetOffset.Y + Y);
    GfxMove(RastPort, GetOffset.X + x, GetOffset.Y + y);
    Position.X := X;
    Position.Y := Y;
  end;
end;

procedure TMUICanvas.LineTo(x, y: integer);
begin
  if Assigned(RastPort) then
  begin
    Draw(RastPort, GetOffset.X + x, GetOffset.Y + y);
    Position.X := X;
    Position.Y := Y;
  end;
end;

procedure TMUICanvas.FillRect(X1, Y1, X2, Y2: Integer);
var
  T: TPoint;
begin
  if Assigned(RastPort) then
  begin
    T := GetOffset;
    RectFill(RastPort, T.X + X1, T.Y + Y1, T.X + X2, T.Y + Y2);
  end;
end;

procedure TMUICanvas.Rectangle(X1, Y1, X2, Y2: Integer);
var
  T: TPoint;
begin
  if Assigned(RastPort) then
  begin
    T := GetOffset;
    SetBrushToRP(True);
    FillRect(X1, Y1, X2, Y2);
    SetPenToRP;
    MoveTo(X1, Y1);
    LineTo(X2, Y1);
    LineTo(X2, Y2);
    LineTo(X1, Y2);
    LineTo(X1, Y1);
  end;
end;

procedure TMUICanvas.WriteText(Txt: PChar; Count: integer);
var
  Tags: TTagsList;
  Col: LongWord;
begin
  if Assigned(RastPort) then
  begin
    Col := TColorToMUIColor(TextColor);
    AddTags(Tags, [LongInt(RPTAG_PenMode), LongInt(False), LongInt(RPTAG_FGColor), LongInt(col), LongInt(TAG_DONE), 0]);
    SetRPAttrsA(RastPort, GetTagPtr(Tags));
    GfxText(RastPort, Txt, Count);
    SetPenToRP;
  end;
end;

function TMUICanvas.TextWidth(Txt: PChar; Count: integer): integer;
begin
  Result := 0;
  if Assigned(RastPort) then
  begin
    Result := TextLength(RastPort, Txt, Count);
  end;
end;

function TMUICanvas.TextHeight(Txt: PChar; Count: integer): integer;
var
  TE: TTextExtent;
begin
  Result := 0;
  if Assigned(RastPort) then
  begin
    TextExtent(RastPort, Txt, Count, @TE);
    Result := TE.te_Height;
  end;
end;

procedure TMUICanvas.SetAMUIPen(PenDesc: integer);
begin
  if (PenDesc >= 0) then
    SetAPen(RastPort, RenderInfo^.mri_Pens[PenDesc]);
end;


constructor TMUICanvas.Create;
var
  APenData: TLogPen;
  ABrushData: TLogBrush;
begin
  ABrushData.lbColor := clBlack;
  APenData.lopnColor := clBlack;
  FDefaultBrush := TMUIBrushObj.Create(ABrushData);
  FDefaultPen := TMUIPenObj.Create(APenData);
  FBrush := FDefaultBrush;
  FPen := FDefaultPen;
  TextColor := 0;
end;

destructor TMUICanvas.Destroy;
begin
  FDefaultBrush.Free;
  FDefaultPen.Free;
  inherited;
end;

procedure TMUICanvas.InitCanvas;
begin
  SetPenToRP;
  SetBrushToRP;
end;

procedure TMUICanvas.SetPenToRP;
var
  Col: TColor;
  Tags: TTagsList;
begin
  if Assigned(RastPort) then
  begin
    if Assigned(FPen) then
    begin
      Col := FPen.Color;
      AddTags(Tags, [LongInt(RPTAG_PenMode), LongInt(False), LongInt(RPTAG_FGColor), LongInt(Col), LongInt(TAG_DONE), 0]);
      SetRPAttrsA(RastPort, GetTagPtr(Tags));
    end;
  end;
end;

procedure TMUICanvas.SetBrushToRP(AsPen: Boolean = FALSE);
var
  Col: TColor;
  Tags: TTagsList;
begin
  if Assigned(RastPort) then
  begin
    if Assigned(FBrush) then
    begin
      Col := FBrush.Color;
      if AsPen then
      begin
        AddTags(Tags, [LongInt(RPTAG_PenMode), LongInt(False), LongInt(RPTAG_FGColor), LongInt(Col), LongInt(TAG_DONE), 0]);
        SetDrMd(RastPort, JAM1);
      end else
      begin
        AddTags(Tags, [LongInt(RPTAG_PenMode), LongInt(False), LongInt(RPTAG_BGColor), LongInt(Col), LongInt(TAG_DONE), 0]);
        SetDrMd(RastPort, FBrush.Style);
      end;
      SetRPAttrsA(RastPort, GetTagPtr(Tags));
    end;
  end;
end;

function TMUICanvas.SelectObject(NewObj: TMUIWinAPIElement): TMUIWinAPIElement;
begin
  Result := nil;
  if not Assigned(NewObj) then
    Exit;
  if NewObj is TMUIPenObj then
  begin
    Result := FPen;
    FPen := TMUIPenObj(NewObj);
    SetPenToRP;
  end;
  if NewObj is TMUIBrushObj then
  begin
    Result := FBrush;
    FBrush := TMUIBrushObj(NewObj);
    SetBrushToRP;
  end;
end;

finalization

end.

