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
  //writeln('begin paint');
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

function TMUIWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint): Boolean;
//var
//  O: TFPGUIPrivateWidget;
begin
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
(*var
  R1: TFPGUIBasicRegion absolute Src1;
  R2: TFPGUIBasicRegion absolute Src2;
  DR: TFPGUIBasicRegion absolute Dest;
  Combine: TFPGUIRegionCombine;*)
begin
(*  case fnCombineMode of
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
  end;*)
  Result := 0;
end;

function TMUIWidgetSet.CreateBitmap(Width, Height: Integer; Planes,
  BitCount: Longint; BitmapBits: Pointer): HBITMAP;
//var
//  img: TFPGUIWinAPIBitmap;
begin
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
  //Result:=HBRUSH(TFPGUIWinAPIBrush.Create(LogBrush));
  Result := 0;
end;

function TMUIWidgetSet.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer
  ): HBITMAP;
//var
//  img: TFPGUIWinAPIBitmap;
begin
  //writeln('CreateCompatibleBitmap');
  //img:=TFPGUIWinAPIBitmap.Create(32,Width,Height);
  //Result:=HBITMAP(img);
  Result := 0;
end;

function TMUIWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
//var
//  ADC: TFpGuiDeviceContext absolute DC;
begin
  //writeln('CreateCompatibleDC');
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
  Result := 0;
end;

function TMUIWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  //Result:=HFONT(TFPGUIWinAPIFont.Create(LogFont));
  Result := 0;
end;

function TMUIWidgetSet.CreateFontIndirectEx(const LogFont: TLogFont;
  const LongFontName: string): HFONT;
begin
  //Result:=HFONT(TFPGUIWinAPIFont.Create(LogFont,LongFontName));
  Result := 0;
end;

function TMUIWidgetSet.CreatePenIndirect(const LogPen: TLogPen): HPEN;
begin
  //Result:=HPEN(TFPGUIWinAPIPen.Create(LogPen));
  Result := 0;
end;

function TMUIWidgetSet.CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN;
//var
//  Reg: TFPGUIBasicRegion;
begin
  //Reg:=TFPGUIBasicRegion.Create(Rect(X1,Y1,X2,Y2));
  //Result:=HRGN(Reg);
  Result := 0;
end;

function TMUIWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
begin
  //if IsValidGDIObject(GDIObject) then begin
  //  TObject(GDIObject).Free;
  //  Result:=true;
  //end else begin
  Result:=false;
  //end;
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
  inherited;
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
      ADC.MoveTo(ARect.Left, ARect.Top + ADC.TextHeight(str, Count) div 2);
      ADC.WriteText(str, Count);
      Result := ADC.TextHeight(Str, Count);
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
   //writeln('end paint');
//  DC := TFpGuiDeviceContext(PS.hdc);
//  DC.Free;
//  Result:=1; //Any non zero value.
  Result := 0;
end;

function TMUIWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
(*var
  ADC: TFpGuiDeviceContext absolute DC;
  AStr: string;
  r: TfpgRect;
  rClip,OldClip: TfpgRect;
  RestoreClip: Boolean;*)
begin
  //writeln('textout');
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
(*var
  ADC: TFpGuiDeviceContext absolute DC;
  NewBrush: TFPGUIWinAPIBrush absolute Brush;
  OldColor: TfpgColor;
  TheRect: TfpgRect;*)
begin
  //writeln('fillrect');
(*  OldColor:=ADC.fpgCanvas.Color;
  ADC.fpgCanvas.Color:=NewBrush.Color;
  TRectTofpgRect(Rect,TheRect);
  AdjustRectToOrg(TheRect,ADC.FOrg);
  ADC.fpgCanvas.FillRectangle(TheRect);
  ADC.fpgCanvas.Color:=OldColor;*)
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
    SetAPen(ADC.RastPort, 1);
    Result := True;
  end;
end;

function TMUIWidgetSet.FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH): Integer;
begin
  //writeln('FrameRect');
end;

function TMUIWidgetSet.GetClientRect(handle: HWND; var ARect: TRect
  ): Boolean;
var
  Widget: TMUIObject absolute handle;
begin
//  fpguiPrivate.GetClientRect(ARect);
  ARect.Left := 0;
  ARect.Right := Widget.Width;
  ARect.Top := 0;
  ARect.Bottom := Widget.Height;
  //writeln('################Get ClientRect ', ARect.Left, ', ', ARect.Right);
  Result:=True;
end;

function TMUIWidgetSet.GetClipRGN(DC: hDC; RGN: hRGN): Longint;
(*var
  ADC: TFpGuiDeviceContext absolute DC;
  Clip: TfpgRect;
  Region: TFPGUIBasicRegion absolute RGN;*)
begin
(*  ADC.fpgCanvas.GetWinRect(Clip); { TODO : Should be a clip region, but use this by now }
  if Region<>nil Then FreeAndNil(Region);
  Region:=TFPGUIBasicRegion.Create(Rect(Clip.Left,Clip.Top,Clip.Right,Clip.Bottom));
  RGN:=HRGN(Region);
  if Region.RegionType=eRegionNULL then begin
    Result:=0;
  end else if Region.RegionType=eRegionNotCombinableOrError then begin
    Result:=-1;
  end else begin
    Result:=1;
  end;*)
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
begin
  Result := 0;
  if Assigned(PrivateWidget) then
    Result := THandle(PrivateWidget.MuiCanvas)
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

function TMUIWidgetSet.GetProp(Handle: hwnd; Str: PChar): Pointer;
//var
//  PrivateWidget: TFPGUIPrivateWidget absolute Handle;
begin
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
begin
  case nIndex of
    //Current screen size
    SM_CXSCREEN: Result:=1400; //Hardcoded by now
    SM_CYSCREEN: Result:=400;
    //Desktop size
    SM_CXVIRTUALSCREEN: Result:=1400;
    SM_CYVIRTUALSCREEN: Result:=400;
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
//var
//  ADC: TFpGuiDeviceContext absolute DC;
begin
(*  FillByte(TM,sizeof(TM),0);
  TM.tmAscent:=ADC.FFont.fpguiFont.Ascent;
  TM.tmDescent:=ADC.FFont.fpguiFont.Descent;
  //Defined usually in MSDN as the average of 'x' char.
  TM.tmAveCharWidth:=ADC.FFont.fpguiFont.TextWidth('x');
  TM.tmHeight:=ADC.FFont.Height;
  Result:=true;*)
  Result := False;
end;

function TMUIWidgetSet.GetWindowOrgEx(dc: hdc; P: PPoint): Integer;
//var
//  ADC: TFpGuiDeviceContext absolute DC;
begin
//  P^:=ADC.FOrg;
  Result:=0;
end;

function TMUIWidgetSet.GetWindowRect(Handle: hwnd; var ARect: TRect
  ): Integer;
//var
//  PrivateWidget: TFPGUIPrivateBin absolute Handle;
begin
//  ARect:=fpgRectToRect(PrivateWidget.Widget.GetBoundsRect);
  Result:=0;
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
//var
//  PrivateWidget: TFPGUIPrivateWidget absolute aHandle;
begin
  //writeln('invalidate');
  //PrivateWidget.Widget.Canvas.BeginDraw(false);
  //PrivateWidget.Paint;
  //PrivateWidget.Widget.Canvas.EndDraw;
  //Result:=true;
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
  inherited;
  if Assigned(ADC) and Assigned(ADC.RastPort) then
  begin
    //writeln('Frame3D ', ARect.Left, ', ', ARect.Right, ' ; ', ARect.Top, ' ', ARect.Bottom,' w:', FrameWidth, ' style: ', Ord(Style));
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
//var
//  ADC: TFpGuiDeviceContext absolute DC;
//  r: TfpgRect;
begin
  //writeln('Rectangle');
  (*r:=ADC.PrepareRectOffsets(Rect(X1,Y1,X2,Y2));
  ADC.fpgCanvas.BeginDraw(true);
  ADC.ClearRectangle(r);
  ADC.fpgCanvas.DrawRectangle(r);
  ADC.fpgCanvas.EndDraw;
  Result:=true;*)
  Result := False;
end;

function TMUIWidgetSet.ReleaseDC(hWnd: HWND; DC: HDC): Integer;
//var
//  MyDC: TFpGuiDeviceContext absolute DC;
begin
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
//var
//  ADC: TFPGUIDeviceContext absolute DC;
//  Reg: TFPGUIBasicRegion absolute RGN;
begin
  (*if Reg.RegionType=eRegionSimple then begin
    ADC.SelectObject(HGDIObj(Reg));
    Result:=SimpleRegion;
  end else begin
    Result:=NullRegion;
  end;*)
  Result:=NullRegion;
end;

function TMUIWidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
//var
//  MyDC: TFpGuiDeviceContext absolute DC;
begin
  //Result:=MyDC.SelectObject(GDIObj);
  Result := GDIObj;
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
//var
//  ADC: TFpGuiDeviceContext absolute DC;
begin
  //Result:=ADC.SetTextColor(Color);
  Result := Color;
end;

function TMUIWidgetSet.SetWindowOrgEx(DC: HDC; NewX, NewY: Integer;
  OldPoint: PPoint): Boolean;
//var
//  ADC: TFpGuiDeviceContext absolute DC;
begin
  //ADC.SetOrigin(NewX,NewY);
  Result:=False;
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
  Result:=false;
end;

function TMUIWidgetSet.StretchMaskBlt(DestDC: HDC; X, Y, Width,
  Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer;
  Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
(*var
  SDC: TFPGUIDeviceContext;
  TDC: TFPGUIDeviceContext;
  R: TRect;
  ClientRect: TfpgRect;
  P: TPoint;*)
begin
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
  Result := False;
end;

function TMUIWidgetSet.WindowFromPoint(Point: TPoint): HWND;
begin
  { TODO : Temporal hack while not real WindowFromPoint implementation }
  //Result:=HWND(GlobalMouseCursorPosWidget);
  Result := 0;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

