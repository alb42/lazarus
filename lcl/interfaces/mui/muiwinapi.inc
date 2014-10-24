{%MainUnit muiint.pp}

{******************************************************************************
  All MUI Winapi implementations.
  This are the implementations of the overrides of the MUI Interface for the
  methods defined in the
  lcl/include/winapi.inc


  !! Keep alphabetical !!


 ******************************************************************************
 Implementation
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{.$define VERBOSEAROS}

//##apiwiz##sps##   // Do not remove, no wizard declaration before this line

{------------------------------------------------------------------------------
  Function: BeginPaint
  Params:
  Returns:

  This function is Called:
  - Once on every OnPaint event
 ------------------------------------------------------------------------------}
function TMUIWidgetSet.BeginPaint(Handle: hWnd; Var PS : TPaintStruct): hdc;
(*var
  PrivateWidget: TFPGUIPrivateWidget absolute Handle;
  DC: TFpGuiDeviceContext;*)
begin
  {$ifdef VERBOSEAROS}
  writeln('begin paint');
  {$endif}
  Result := 0;
(*  {$ifdef VerboseFPGUIWinAPI}
    WriteLn('Trace:> [WinAPI BeginPaint] Handle=', dbghex(Handle));
  {$endif}
  {$WARNING TMUIWidgetSet.BeginPaint Temporary Fix to prevent Crashing}
  try
  if PrivateWidget <> nil then
    DC := TFpGuiDeviceContext.Create(PrivateWidget)
  else
    DC := TFpGuiDeviceContext.Create(nil);
    {$ifdef VerboseFPGUIWinAPI}
    if PrivateWidget <> nil then
    WriteLn(PrivateWidget.ClassName);
    {$endif}
  except
    DC := TFpGuiDeviceContext.Create(nil);
  end;
  PS.hdc := HDC(DC);

  Result := PS.hdc;

  {$ifdef VerboseFPGUIWinAPI}
    WriteLn('Trace:< [WinAPI BeginPaint] Result=', dbghex(Result));
  {$endif}*)
end;

function TMUIWidgetSet.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
begin
  {$ifdef VERBOSEAROS}
  writeln('BitBlt');
  {$endif}
  Result := True;
end;

function TMUIWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint): Boolean;
//var
//  O: TFPGUIPrivateWidget;
begin
  {$ifdef VERBOSEAROS}
  writeln('client to screen');
  {$endif}
(*  O:=GlobalMouseCursorPosWidget;
  while O<>nil do begin
    P.X:=P.X+O.Widget.Left;
    P.Y:=P.Y+O.Widget.Top;
    if O.LCLObject.Parent<>nil then begin
      O:=TFPGUIPrivateWidget(O.LCLObject.Parent.Handle);
    end else begin
      O:=nil;
    end;
  end;
  Result:=true; *)
  Result := False;
end;

function TMUIWidgetSet.CombineRgn(Dest, Src1, Src2: HRGN;
  fnCombineMode: Longint): Longint;
var
  R1: TMUIBasicRegion absolute Src1;
  R2: TMUIBasicRegion absolute Src2;
  DR: TMUIBasicRegion absolute Dest;
  Combine: TMUIRegionCombine;
begin
  {$ifdef VERBOSEAROS}
  writeln('combine region $', HexStr(Pointer(src1)), ' + $', HexStr(Pointer(src2)), ' to $', HexStr(Pointer(Dest)));
  writeln('src1: ', R1.Debugout);
  writeln('src2: ', R2.Debugout);
  {$endif}
  case fnCombineMode of
    RGN_AND:  Combine:=eRegionCombineAnd;
    RGN_COPY: Combine:=eRegionCombineCopy;
    RGN_DIFF: Combine:=eRegionCombineDiff;
    RGN_OR:   Combine:=eRegionCombineOr;
    RGN_XOR:  Combine:=eRegionCombineXor;
  end;
  if DR<>nil then DR.Free;
  DR:=R1.CombineWithRegion(R2,Combine);
  Case dr.RegionType of
    eRegionNULL: Result:=NullRegion;
    eRegionSimple: Result:=SimpleRegion ;
    eRegionComplex: Result:=ComplexRegion;
    eRegionNotCombinableOrError: Result:=Region_Error;
  end;
  {$ifdef VERBOSEAROS}
  writeln('Dest: ', DR.Debugout);
  {$endif}
end;

function TMUIWidgetSet.CreateBitmap(Width, Height: Integer; Planes,
  BitCount: Longint; BitmapBits: Pointer): HBITMAP;
//var
//  img: TFPGUIWinAPIBitmap;
begin
  {$ifdef VERBOSEAROS}
  writeln('Create Bitmap');
  {$endif}
(*  if BitCount>0 then begin
    img:=TFPGUIWinAPIBitmap.Create(BitCount,Width,Height);
    Result:=HBITMAP(img);
  end else begin
    Result:=0;
  end;*)
  Result := 0;
end;

function TMUIWidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush
  ): HBRUSH;
begin
  Result:=HBRUSH(TMUIBrushObj.Create(LogBrush));
end;

function TMUIWidgetSet.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer
  ): HBITMAP;
//var
//  img: TFPGUIWinAPIBitmap;
begin
  {$ifdef VERBOSEAROS}
  writeln('Create Compatible Bitmap');
  {$endif}
  //writeln('CreateCompatibleBitmap');
  //img:=TFPGUIWinAPIBitmap.Create(32,Width,Height);
  //Result:=HBITMAP(img);
  Result := 0;
end;

function TMUIWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
var
  ADC: TMUICanvas absolute DC;
  NewDC: TMUICanvas;
begin

  Result := 0;
  //if DC <> 0 then
  begin
    NewDC := TMUICanvas.Create;
    NewDC.RastPort := nil;
    NewDC.InitCanvas;
    NewDC.RenderInfo := nil;
    if DC <> 0 then
      NewDC.RenderInfo := ADC.RenderInfo;
    Result := HDC(NewDC);
  end;
  {$ifdef VERBOSEAROS}
  writeln('CreateCompatibleDC ' , HexStr(Pointer(Result)));
  {$endif}

(*  {$ifdef VerboseFPGUIWinAPI}
    if DC=0 then begin
      WriteLn(Self.ClassName,'.CreateCompatibleDC ','NULL');
    end else begin
      if ADC.FPrivateWidget<>nil then begin
        WriteLn(Self.ClassName,'.CreateCompatibleDC ',ADC.FPrivateWidget.LCLObject.Name);
      end else begin
        WriteLn(Self.ClassName,'.CreateCompatibleDC ','Desktop');
      end;
    end;
  {$endif}
  if DC=0 then begin
    //Create DC desktop compatible, or retrieve the destop one to avoid memory leask.
    Result:=HDC(FPGUIGetDesktopDC());
  end else begin
    //Create DC widget compatible
    Result:=HDC(TFpGuiDeviceContext.Create(ADC.FPrivateWidget));
  end;*)
end;

function TMUIWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
var
  FontObj: TMUIFontObj;
begin
  {$ifdef VERBOSEAROS}
  writeln('CreateFontIndirect');
  {$endif}
  FontObj := TMUIFontObj.Create(LogFont);
  if Assigned(FontObj.TextFont) then
  begin
    Result := HFont(FontObj);
  end else
  begin
    FontObj.Free;
    Result := 0;
  end;
end;

function TMUIWidgetSet.CreateFontIndirectEx(const LogFont: TLogFont; const LongFontName: string): HFONT;
var
  FontObj: TMUIFontObj;
begin
  {$ifdef VERBOSEAROS}
  writeln('CreateFontIndirectEx ', LongFontName);
  {$endif}
  FontObj := TMUIFontObj.Create(LogFont, LongFontName);
  if Assigned(FontObj.TextFont) then
  begin
    Result := HFont(FontObj);
  end else
  begin
    FontObj.Free;
    Result := 0;
  end;
end;

function TMUIWidgetSet.CreatePenIndirect(const LogPen: TLogPen): HPEN;
begin
  Result := HPEN(TMUIPenObj.Create(LogPen));
end;

function TMUIWidgetSet.CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN;
var
  Reg: TMUIBasicRegion;
begin
  Reg:=TMUIBasicRegion.Create(Rect(X1,Y1,X2,Y2));
  Result:=HRGN(Reg);
  {$ifdef VERBOSEAROS}
  writeln('Create Rect Region ', x1,', ', y1, ', ', x2, ', ', y2,' $',HexStr(Reg));
  {$endif}
end;

procedure TMUIWidgetSet.DeleteCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.DoneCriticalsection(ACritSec^);
  Dispose(ACritSec);
  CritSection:=0;
end;

function TMUIWidgetSet.DeleteDC(hDC: HDC): Boolean;
var
  ADC: TMUICanvas absolute hDC;
begin
  {$ifdef VERBOSEAROS}
  writeln('Delete DC $', HexStr(Pointer(hdc)));
  {$endif}
  ADC.Free;
end;

function TMUIWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
var
  Obj: TObject absolute GDIObject;
begin
  {$ifdef VERBOSEAROS}
  writeln('DeleteObject $', HexStr(Pointer(GDIObject)));
  {$endif}
  if (GDIObject > $100) and Assigned(Obj) then
  begin
   if Obj is TMUIWinAPIObject then
     Obj.Free;
  end;
  Result:=True;
end;

function TMUIWidgetSet.DrawFocusRect(DC: HDC; const Rect: TRect): boolean;
//var
//  ADC: TFpGuiDeviceContext absolute DC;
//  r: TfpgRect;
begin
  //ADC.fpgCanvas.DrawFocusRect(ADC.PrepareRectOffsets(Rect));
  Result:=true;
end;

function TMUIWidgetSet.DrawText(DC: HDC; Str: PChar; Count: Integer;
  var ARect: TRect; Flags: Cardinal): Integer;
var
  ADC: TMUICanvas absolute DC;
  Left, Top: Integer;
begin
  {$ifdef VERBOSEAROS}
  writeln('Draw Text ', str, ' DC: ', HexStr(Pointer(DC)) , ' to ', ARect.Left, ',  ', ARect.Top, '; ', ARect.Right, ', ', ARect.Bottom);
  {$endif}
  if Assigned(ADC) then
  begin
    if Assigned(ADC.RastPort) then
    begin
      if (Flags and DT_CALCRECT) <> 0 then
      begin
        ARect.Right := ARect.Left + ADC.TextWidth(Str, Count);
        ARect.Bottom := ARect.Top + ADC.TextHeight(Str, Count);
        Result := ADC.TextHeight(Str, Count);
        Exit;
      end;
      {$ifdef VERBOSEAROS}
      if (Flags and DT_BOTTOM) <> 0 then
      begin
        writeln('Bottom');
      end;
      if (Flags and DT_Right) <> 0 then
      begin
        writeln('Right');
      end;
      if (Flags and DT_Center) <> 0 then
      begin
        writeln('Center');
      end;
      if (Flags and DT_Left) <> 0 then
      begin
        writeln('Left');
      end;
      if (Flags and DT_Top) <> 0 then
      begin
        writeln('Top');
      end;
      if (Flags and DT_VCenter) <> 0 then
      begin
        writeln('VCenter');
      end;
      {$endif}
      SetDrMd(ADC.RastPort, JAM1);
      ADC.MoveTo(ARect.Left, ARect.Top + ADC.TextHeight(str, Count) div 2);
      ADC.WriteText(str, Count);
      Result := ADC.TextHeight(Str, Count);
      ADC.ResetPenBrushFont;
    end;
  end;
end;

function TMUIWidgetSet.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
var
  Widget: TMUIObject absolute hWnd;
begin
  Widget.Enabled:=bEnable;
  Result:=true;
end;

function TMUIWidgetSet.EndPaint(Handle: hwnd; var PS: TPaintStruct): Integer;
//var
//  DC: TFpGuiDeviceContext;
begin
  {$ifdef VERBOSEAROS}
   writeln('end paint');
  {$endif}
//  DC := TFpGuiDeviceContext(PS.hdc);
//  DC.Free;
//  Result:=1; //Any non zero value.
  Result := 0;
end;

procedure TMUIWidgetSet.EnterCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.EnterCriticalsection(ACritSec^);
end;

{
function TMUIWidgetSet.ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint) : Integer;
var
  Region: TMUIBasicRegion absolute RGN;
begin
  writeln('ExtSelectClip ', Region.Debugout, ': ', Mode);
end; }

function TMUIWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  ADC: TMUICanvas absolute DC;
(*var
  ADC: TFpGuiDeviceContext absolute DC;
  AStr: string;
  r: TfpgRect;
  rClip,OldClip: TfpgRect;
  RestoreClip: Boolean;*)
begin
  {$ifdef VERBOSEAROS}
  writeln('Ext Text out ', str);
  {$endif}
  if Assigned(ADC) then
  begin
    ADC.MoveTo(X, Y);
    ADC.WriteText(Str, Count);
  end;
(*  SetLength(AStr,Count);
  move(Str[0],AStr[1],Count);
  r:=ADC.PrepareRectOffsets(classes.Rect(X,Y,0,0));
  RestoreClip:=false;
  if Rect<>nil then begin
    rClip:=fpgRect(Rect^.Left,Rect^.Top,Rect^.Right-Rect^.Left,Rect^.Bottom-Rect^.Top);
    if (ETO_CLIPPED or ETO_OPAQUE) and Options <> 0 then begin
      OldClip:=ADC.fpgCanvas.GetClipRect;
      ADC.fpgCanvas.SetClipRect(rClip);
      RestoreClip:=true;
    end;
    if ETO_OPAQUE and Options = ETO_OPAQUE then begin
      ADC.ClearRectangle(rClip);
    end;
  end;

  ADC.fpgCanvas.DrawText(r.Left, r.Top, AStr);

  if RestoreClip then begin
    ADC.fpgCanvas.SetClipRect(OldClip);
  end;
  Result:=true; *)
  Result:=False;
end;

function TMUIWidgetSet.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH
  ): Boolean;
var
  ADC: TMUICanvas absolute DC;
  MUIBrush: TMUIWinAPIElement absolute Brush;
  OBrush: TMUIWinAPIElement;
begin
  if Assigned(MUIBrush) then
  begin
    OBrush := ADC.SelectObject(MUIBrush);
  end;
  ADC.SetBrushToRP(True);
  ADC.FillRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
  ADC.SetPenToRP;
  if Assigned(MUIBrush) then
  begin
    MUIBrush := ADC.SelectObject(OBrush);
  end;
  Result:=False;
end;

{function TMUIWidgetSet.Frame(DC: HDC; const ARect: TRect): Integer;
begin

end;}

function TMUIWidgetSet.Frame3d(DC: HDC; var ARect: TRect; const FrameWidth : integer; const Style : TBevelCut): Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  Result := False;
  inherited;
  if Assigned(ADC) and Assigned(ADC.RastPort) then
  begin
    //writeln('Frame3D ', ARect.Left, ', ', ARect.Right, ' ; ', ARect.Top, ' ', ARect.Bottom,' w:', FrameWidth, ' style: ', Ord(Style));
    if Style = bvRaised then
    begin
      ADC.SetAMUIPen(MPEN_SHINE);
      ADC.MoveTo(ARect.Left, ARect.Bottom - 1);
      ADC.LineTo(ARect.Left, ARect.Top);
      ADC.LineTo(ARect.Right - 1, ARect.Top);
      ADC.SetAMUIPen(MPEN_SHADOW);
      ADC.MoveTo(ARect.Right - 1, ARect.Top);
      ADC.LineTo(ARect.Right - 1, ARect.Bottom - 1);
      ADC.LineTo(ARect.Left, ARect.Bottom - 1);
    end;
    if Style = bvLowered then
    begin
      ADC.SetAMUIPen(MPEN_SHADOW);
      ADC.MoveTo(ARect.Right - 1, ARect.Top);
      ADC.LineTo(ARect.Left, ARect.Top);
      ADC.LineTo(ARect.Left, ARect.Bottom - 1);
      ADC.SetAMUIPen(MPEN_SHINE);
      ADC.MoveTo(ARect.Left, ARect.Bottom - 1);
      ADC.LineTo(ARect.Right - 1, ARect.Bottom - 1);
      ADC.LineTo(ARect.Right - 1, ARect.Top);
    end;
    Inc(ARect.Left);
    Inc(ARect.Top);
    Dec(ARect.Right);
    Dec(ARect.Bottom);
    ADC.ResetPenBrushFont;
    Result := True;
  end;
end;

function TMUIWidgetSet.FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH): Integer;
begin
  //writeln('FrameRect');
end;

function TMUIWidgetSet.GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint;
begin
  {$ifdef VERBOSEAROS}
  writeln('GetBitmapBits');
  {$endif}
end;

function TMUIWidgetSet.GetClientBounds(handle : HWND; var ARect : TRect) : Boolean;
var
  Widget: TMUIObject absolute handle;
begin
  ARect.Left := Widget.Left;
  ARect.Right := Widget.Left + Widget.Width;
  ARect.Top := Widget.Top;
  ARect.Bottom := Widget.Top + Widget.Height;
  Result := True;
end;

function TMUIWidgetSet.GetClientRect(handle: HWND; var ARect: TRect
  ): Boolean;
var
  Widget: TMUIObject absolute handle;
begin
  ARect.Left := 0;
  ARect.Right := Widget.Width;
  ARect.Top := 0;
  ARect.Bottom := Widget.Height;
  {$ifdef VERBOSEAROS}
  writeln(Widget.classname, '################Get ClientRect ', ARect.Left, ', ', ARect.Right);
  {$endif}
  Result:=True;
end;

function TMUIWidgetSet.GetClipRGN(DC: hDC; RGN: hRGN): Longint;
var
  ADC: TMUICanvas absolute DC;
  Region: TMUIBasicRegion absolute RGN;
begin
  {$ifdef VERBOSEAROS}
  writeln('Get Clip region ', HexStr(Pointer(Rgn)));
  {$endif}
  if Region=nil Then
    Exit;                   //
  Region.CreateRectRegion(Rect(ADC.DrawRect.Left,ADC.DrawRect.Top,ADC.DrawRect.Right,ADC.DrawRect.Bottom));
  if Region.RegionType=eRegionNULL then begin
    Result:=0;
  end else if Region.RegionType=eRegionNotCombinableOrError then begin
    Result:=-1;
  end else begin
    Result:=1;
  end;
end;

function TMUIWidgetSet.GetCursorPos(var lpPoint: TPoint): Boolean;
//var
//  O: TFPGUIPrivateWidget;
begin
(*  O:=GlobalMouseCursorPosWidget;
  lpPoint:=GlobalMouseCursorPos;
  while O<>nil do begin
    lpPoint.X:=lpPoint.X+O.Widget.Left;
    lpPoint.Y:=lpPoint.Y+O.Widget.Top;
    if O.LCLObject.Parent<>nil then begin
      O:=TFPGUIPrivateWidget(O.LCLObject.Parent.Handle);
    end else begin
      O:=nil;
    end;
  end;
  Result:=true;*)
end;

function TMUIWidgetSet.GetDC(hWnd: HWND): HDC;
var
  PrivateWidget: TMUIObject absolute hWnd;
  fpForm: PWindow;
  ri: PMUI_RenderInfo;
begin
  Result := 0;
  if Assigned(PrivateWidget) then
  begin
    {$ifdef VERBOSEAROS}
    writeln('Get DC ', PrivateWidget.classname,' ', Assigned(PrivateWidget.MuiCanvas.RastPort));
    {$endif}
    if not Assigned(PrivateWidget.MuiCanvas.RastPort) then
    begin
      if PRivateWidget is TMuiWindow then
      begin
        ri := MUIRenderInfo(TMuiWindow(PrivateWidget).Grpobj);
        if Assigned(ri) then
        begin
          PrivateWidget.MUICanvas.RastPort := ri^.mri_RastPort;
          PrivateWidget.MUICanvas.DrawRect := Rect(0,0,0,0);
        end;
      end;
    end;
    Result := THandle(PrivateWidget.MuiCanvas);
  end;
end;

function TMUIWidgetSet.GetDeviceCaps(DC: HDC; Index: Integer): Integer;
begin
  //Desktop device caps
  { TODO : Create real data for GetDeviceCaps }
  Case Index of
    LOGPIXELSX: Result:=96; //Hardcoded by now
    BITSPIXEL : Result:=32; //Hardcoded by now
    else
        WriteLn(Self.ClassName,'.GetDeviceCaps Index ',Index,' Desktop');
  end;
end;

function TMUIWidgetSet.GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer;
begin
  {$ifdef VERBOSEAROS}
  writeln('GetDIBits');
  {$endif}
end;

function TMUIWidgetSet.GetProp(Handle: hwnd; Str: PChar): Pointer;
//var
//  PrivateWidget: TFPGUIPrivateWidget absolute Handle;
begin
   WriteLn('Trace:Unknown Window property: ',Str);
(*  if Str='WinControl' then begin
    Result:=PrivateWidget.LCLObject;
  end else begin
    {$ifdef VerboseFPGUIWinAPI}
      WriteLn('Trace:Unknown Window property: ',Str);
    {$endif}
    Result:=nil;
  end;*)
end;

function TMUIWidgetSet.GetSysColor(nIndex: Integer): DWORD;
begin
  //writeln('Get Syscolor ', nIndex);
(*  if (nIndex < 0) or (nIndex > MAX_SYS_COLORS) then
  begin
    {$ifdef VerboseFPGUIWinAPI}
      WriteLn('Trace:Unknown lcl system color: [TMUIWidgetSet.GetSysColor]');
    {$endif}
    Result:=clRed;
    exit;
  end;
  Result:=GetSysColorRGB(nIndex); *)
end;

function TMUIWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
var
  Sc: PScreen;
begin
  //writeln('get system metrics ', nIndex);
  Sc := LockPubscreen('Workbench');
  if Assigned(Sc) then
  begin
    case nIndex of
      //Current screen size
      SM_CXSCREEN: Result:= Sc^.Width;
      SM_CYSCREEN: Result:= Sc^.Height;
      //Desktop size
      SM_CXVIRTUALSCREEN: Result:=Sc^.Width;
      SM_CYVIRTUALSCREEN: Result:=Sc^.Height;
    end;
    UnlockPubScreen('Workbench', Sc);
  end;
end;

function TMUIWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar;
  Count: Integer; var Size: TSize): Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  Result := False;
  inherited;
  if Assigned(ADC) then
  begin
    if Assigned(ADC.RastPort) then
    begin
      Size.cx := ADC.TextWidth(Str, Count);
      Size.cy := ADC.TextHeight(Str, Count);
      Result := True;
    end;
  end;
end;

function TMUIWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  {$ifdef VERBOSEAROS}
  writeln('Get TextMetric');
  {$endif}
  FillByte(TM, SizeOf(TM), 0);
  TM.tmAscent := 2;
  TM.tmDescent := 2;
  //Defined usually in MSDN as the average of 'x' char.
  TM.tmAveCharWidth := ADC.TextWidth('x', 1);
  TM.tmHeight := ADC.TextHeight('X', 1);
  Result := True;
end;

function TMUIWidgetSet.GetWindowOrgEx(dc: hdc; P: PPoint): Integer;
var
  ADC: TMUICanvas absolute DC;
begin
  {$ifdef VERBOSEAROS}
  writeln('Get Window org Ex');
  {$endif}
  if Assigned(P) then
  begin
    P^.X := ADC.Offset.X;//ADC.Left;
    P^.y := ADC.Offset.Y;//ADC.Top;
  end;
  //writeln('Get Window Org');
//  P^:=ADC.FOrg;
  Result:=1;
end;

function TMUIWidgetSet.GetWindowRect(Handle: hwnd; var ARect: TRect
  ): Integer;
var
  PrivateWidget: TMUIObject absolute Handle;
begin
  {$ifdef VERBOSEAROS}
  writeln('Get Window rect');
  {$endif}
  ARect:=Rect(PrivateWidget.Left, PrivateWidget.Top, PrivateWidget.Width, PrivateWidget.Height);
  Result:=1;
end;

function TMUIWidgetSet.GetWindowSize(Handle: hwnd; var Width, Height: Integer
  ): boolean;
var
  Widget: TMUIObject absolute Handle;
begin
  Width := Widget.Width;
  Height := Widget.Height;
  Result := True;
end;

function TMUIWidgetSet.InvalidateRect(aHandle: HWND; Rect: pRect;
  bErase: Boolean): Boolean;
var
  PrivateWidget: TMUIObject absolute aHandle;
begin
  {$ifdef VERBOSEAROS}
  writeln('-->invalidate');
  if Assigned(Rect) then
  begin
    writeln('Rect: ', Rect^.Left, ', ', Rect^.Top, ' - ', Rect^.Right,', ', Rect^.Bottom);
  end else
    writeln('Rect = nil;');
  {$endif}
  Result := False;
  if Assigned(PrivateWidget) then
  begin
    PrivateWidget.DoMUIDraw;
    Result := True;
  end;
end;

procedure TMUIWidgetSet.InitializeCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  New(ACritSec);
  System.InitCriticalSection(ACritSec^);
  CritSection:=TCriticalSection(ACritSec);
end;

procedure TMUIWidgetSet.LeaveCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.LeaveCriticalsection(ACritSec^);
end;

function TMUIWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  Result := False;
  inherited;
  if Assigned(ADC) and Assigned(ADC.RastPort) then
  begin
    ADC.LineTo(X, Y);
    Result := True;
  end;
end;

{ Most of the functionality is implemented. As described in MSDN:
  http://msdn.microsoft.com/en-us/library/windows/desktop/ms645505%28v=vs.85%29.aspx }
function TMUIWidgetSet.MessageBox(hWnd: HWND; lpText, lpCaption: PChar;
  uType: Cardinal): integer;
(*var
  Str: AnsiString;
  TitleStr: AnsiString;
  Buttons : TfpgMsgDlgButtons;
  BtnType: Cardinal;
  DlgType: Cardinal;*)
begin
(*  BtnType := (uType and $0000000F); { mask the button type }

  if (BtnType = MB_OKCANCEL) then
    Buttons := mbOKCancel
  else
  if (BtnType = MB_ABORTRETRYIGNORE) then
    Buttons := mbAbortRetryIgnore
  else
  if (BtnType = MB_YESNOCANCEL) then
    Buttons := mbYesNoCancel
  else
  if (BtnType = MB_YESNO) then
    Buttons := mbYesNo
  else
  if (BtnType = MB_RETRYCANCEL) then
    Buttons := [mbRetry, mbCancel]
  else
  if (BtnType = MB_CANCELTRYCONTINUE) then
    Buttons := mbAbortRetryIgnore
  else
    Buttons := [mbOK];

  { shoud we had a Help button too? - again as per MSDN }
  if (uType and MB_HELP) = MB_HELP then
    Include(Buttons, mbHelp);

  Str := lpText;
  TitleStr := lpCaption;
  if lpCaption = nil then
    TitleStr := 'Error'; // as per MSDN

  DlgType := (uType and $000000F0); { mask the dialog type }

  if (DlgType and MB_ICONINFORMATION) = MB_ICONINFORMATION then
    TfpgMessageDialog.Information(TitleStr, Str, Buttons)
  else
  if (DlgType and MB_ICONWARNING) = MB_ICONWARNING then
    TfpgMessageDialog.Warning(TitleStr, Str, Buttons)
  else
  if (DlgType and MB_ICONQUESTION) = MB_ICONQUESTION then
    TfpgMessageDialog.Question(TitleStr, Str, Buttons)
  else
  if (DlgType and MB_ICONERROR) = MB_ICONERROR then
    TfpgMessageDialog.Critical(TitleStr, Str, Buttons)
  else
    TfpgMessageDialog.Information(TitleStr, Str, Buttons);*)
end;

function TMUIWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  Result := False;
  {$ifdef VERBOSEAROS}
  writeln('Move to : ', x, ', ', y);
  {$endif}
  inherited;
  if Assigned(ADC) and Assigned(ADC.RastPort) then
  begin
    if Assigned(OldPoint) then
    begin
      OldPoint^.X := ADC.Position.X;
      OldPoint^.Y := ADC.Position.Y;
    end;
    ADC.MoveTo(X, Y);
    Result := True;
  end;
end;

function TMUIWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  {$ifdef VERBOSEAROS}
  writeln('Rectangle ', X1, ', ', Y1, ' - ', X2, ', ', Y2);
  {$endif}
  Result := False;
  if Assigned(ADC) then
  begin
    ADC.Rectangle(X1, Y1, X2, Y2);
    Result := True;
  end;

end;

function TMUIWidgetSet.RectVisible(dc : hdc; const ARect: TRect) : Boolean;
begin
  {$ifdef VERBOSEAROS}
  writeln('Rect Visible ', ARect.Left, ',', ARect.Right);
  {$endif}
  Result := Boolean(1);
end;

function TMUIWidgetSet.ReleaseDC(hWnd: HWND; DC: HDC): Integer;
var
  PrivateWidget: TMUIObject absolute hWnd;
begin
  {$ifdef VERBOSEAROS}
  writeln('release dc');
  {$endif}
//  if MyDC<>FPGUIGetDesktopDC then begin //DesktopDC can not be freed
//    MyDC.Free;
//  end;
  Result:=0;
end;

function TMUIWidgetSet.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
//var
//  ADC: TFPGUIDeviceContext absolute DC;
begin
  //Result:=ADC.RestoreDC(SavedDC);
  Result := False;
end;

function TMUIWidgetSet.SaveDC(DC: HDC): Integer;
//var
//  ADC: TFPGUIDeviceContext absolute DC;
begin
  //Result:=ADC.SaveDC;
  Result := 0;
end;

function TMUIWidgetSet.SelectClipRGN(DC: hDC; RGN: HRGN): Longint;
var
  ADC: TMUICanvas absolute DC;
  Reg: TMUIBasicRegion absolute RGN;
begin
  {$ifdef VERBOSEAROS}
  writeln('select Clip Rgn $', HexStr(Pointer(RGN)));
  {$endif}
  if Assigned(ADC) then
  begin
    ADC.SetClipping(Reg)
  end;
  Result:=SimpleRegion;
end;

function TMUIWidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
var
  MyDC: TMUICanvas absolute DC;
  GDI: TMUIWinAPIObject absolute GDIObj;
begin
  {$ifdef VERBOSEAROS}
  writeln('select object ', HexStr(Pointer(GDIObj)));
  {$endif}
  Result := 0;
  if Assigned(GDI) and (GDIObj > $100) then
  begin
    if (TObject(GDIObj) is TMUIWinAPIObject) then
      Result := HGDIOBJ(MyDC.SelectObject(GDI));
  end;
end;

function TMUIWidgetSet.SetFocus(hWnd: HWND): HWND;
//var
//  Widget: TFPGUIPrivateWidget absolute hWnd;
begin
  //Result:=LCLType.HWND(Widget.Widget.ActiveWidget);
  //Widget.SetFocus;
  Result := 0;
end;

function TMUIWidgetSet.SetParent(hWndChild: HWND; hWndParent: HWND): HWND;
//var
//  PrivateWidgetParent: TFPGUIPrivateWidget absolute hWndParent;
//  PrivateWidgetChild: TFPGUIPrivateWidget absolute hWndChild;
begin
//  PrivateWidgetChild.Widget.Parent:=PrivateWidgetParent.Widget;
  Result:=0; //???
end;

function TMUIWidgetSet.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
var
  ADC: TMUICanvas absolute DC;
begin
  if Assigned(ADC) then
  begin
    Result := ADC.TextColor;
    ADC.TextColor := Color;
  end;
end;

function TMUIWidgetSet.SetWindowOrgEx(DC: HDC; NewX, NewY: Integer;
  OldPoint: PPoint): Boolean;
var
  ADC: TMUICanvas absolute DC;
begin
  {$ifdef VERBOSEAROS}
  writeln('################set window org ex ', NewX, ', ', NewY);
  {$endif}
  if Assigned(OldPoint) then
  begin
    OldPoint^.X := ADC.Offset.X;
    OldPoint^.Y := ADC.Offset.Y;
  end;
  ADC.Offset.X := ADC.Offset.X - NewX;
  ADC.Offset.Y := ADC.Offset.Y - NewY;
  Result:=True;
end;

function TMUIWidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
var
  Widget: TMUIObject absolute hWnd;
begin
  Result := Widget.Visible;
  Widget.Visible := True;
end;

function TMUIWidgetSet.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; ROp: Cardinal
  ): Boolean;
//var
//  SDC: TFPGUIDeviceContext absolute SrcDC;
//  TDC: TFPGUIDeviceContext absolute DestDC;
begin
  {$ifdef VERBOSEAROS}
  writeln('StretchBlt');
  {$endif}
  Result:=True;
end;

function TMUIWidgetSet.StretchMaskBlt(DestDC: HDC; X, Y, Width,
  Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer;
  Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
var
  Dest: TMUICanvas absolute DestDC;
  Src: TMUICanvas absolute SrcDC;
  i,xs,ys: Integer;
  NImage: Pointer;
  DB,SB: PLongWord;
  FX,FY: Double;
  NX, NY: Integer;
  LineStart: PLongWord;
(*var
  SDC: TFPGUIDeviceContext;
  TDC: TFPGUIDeviceContext;
  R: TRect;
  ClientRect: TfpgRect;
  P: TPoint;*)
begin
  {$ifdef VERBOSEAROS}
  writeln('StretchMaskBlt $', HexStr(Pointer(DestDC)), ', $',HexStr(Pointer(SrcDC)),', $',HexStr(Pointer(Mask)));
  writeln('rop:',rop);
  {$endif}
  if Assigned(Dest) and Assigned(Src) and Assigned(Src.Bitmap) then
  begin
    if (SrcWidth = Width) and (SrcHeight = height) then
      WritePixelArrayAlpha(Src.Bitmap.FImage, XSrc, YSrc, SrcWidth * SizeOf(LongWord), Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, 255)
    else
    begin
      NImage := System.AllocMem(Width * (Height + 10) * SizeOf(LongWord));
      DB := NImage;
      FX := 1;
      FY := 1;
      if SrcWidth > 0 then
        FX := SrcWidth/Width;
      if SrcHeight > 0 then
        FY := SrcHeight/Height;
      for ys := 0 to Height - 5 do
      begin
        NY := Min(Src.Bitmap.FHeight - 1, Round(ys * FY));
        i := NY * SrcWidth;
        LineStart := Src.Bitmap.FImage;
        Inc(LineStart, i);
        for xs := 0 to Width - 1 do
        begin
          NX := Min(Src.Bitmap.FWidth - 1, Round(xs * FX));
          SB := LineStart;
          Inc(SB, NX);
          DB^ := SB^;
          Inc(DB);
        end;
      end;
      WritePixelArrayAlpha(NImage, XSrc, YSrc, Width * SizeOf(LongWord), Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, 255);
      System.FreeMem(NImage);
    end;
    //ScalePixelArray(Src.Bitmap.FImage, SrcWidth, SrcHeight, SrcWidth * SizeOf(LongWord), Dest.RastPort, Dest.GetOffset.X + x, Dest.GetOffset.Y + y, width, height, RECTFMT_RGBA);
    //writeln('wrote: ', i, ', ', Dest.GetOffset.X + x, ', ', Dest.GetOffset.Y + Y, ', ', Width, ', ', Height,' - ', SrcWidth,',', SrcHeight);
  end;

(*  SDC:=TFPGUIDeviceContext(SrcDC);
  TDC:=TFPGUIDeviceContext(DestDC);
  r.Left:=X; r.Right:=Width; R.Top:=Y; r.Bottom:=Height;
  AdjustRectToOrg(r,TDC.FOrg);
  ClientRect:=TDC.FPrivateWidget.Widget.GetClientRect;
  P.x:=-ClientRect.Left;
  P.y:=-ClientRect.Top;
  AdjustRectToOrg(r,P);
  TDC.ClearDC;
  TDC.fpgCanvas.DrawImage(r.Left,r.Top,SDC.FBitmap.Image);
  Result:=true;*)
  Result := True;
end;

function TMUIWidgetSet.WindowFromPoint(Point: TPoint): HWND;
begin
  { TODO : Temporal hack while not real WindowFromPoint implementation }
  //Result:=HWND(GlobalMouseCursorPosWidget);
  Result := 0;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

