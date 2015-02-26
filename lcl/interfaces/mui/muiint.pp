{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 }

unit MUIInt;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

uses
  {$IFDEF TraceGdiCalls}
  LineInfo,
  {$ENDIF}
  // rtl+fcl
  agraphics, Types, Classes, SysUtils, FPCAdds, Math,
  // interfacebase
  InterfaceBase,
  // LCL
  Dialogs, Controls, Forms, LCLStrConsts, LMessages, stdctrls,
  LCLProc, LCLIntf, LCLType, GraphType, Graphics, Menus, Themes, muithemes,
  //AROS
  //Aroswinunit,
  MUIBaseUnit, MUIFormsUnit, muidrawing,
  {$ifdef HASAMIGA}
  exec, intuition, gadtools, mui, utility, AmigaDos, tagsarray, cybergraphics,
  {$endif}
  // widgetset
  WSLCLClasses, LCLMessageGlue;

const
  IdButtonTexts: array[idButtonOk..idButtonShield] of string = (
 { idButtonOk       } 'OK',
 { idButtonCancel   } 'Cancel',
 { idButtonHelp     } 'Help',
 { idButtonYes      } 'Yes',
 { idButtonNo       } 'No',
 { idButtonClose    } 'Close',
 { idButtonAbort    } 'Abort',
 { idButtonRetry    } 'Retry',
 { idButtonIgnore   } 'Ignore',
 { idButtonAll      } 'All',
 { idButtonYesToAll } 'YesToAll',
 { idButtonNoToAll  } 'NoToAll',
 { idButtonOpen     } 'Open',
 { idButtonSave     } 'Save',
 { idButtonShield   } 'Shield'
  );
type
  { TMUIWidgetSet }

  TMUIWidgetSet = class(TWidgetSet)
  protected
    function CreateThemeServices: TThemeServices; override;
    function GetAppHandle: THandle; override;
  public
    procedure PassCmdLineOptions; override;
  public
    function LCLPlatform: TLCLPlatform; override;
    function GetLCLCapability(ACapability: TLCLCapability):PtrUInt; override;
    // Application
    procedure AppInit(var ScreenInfo: TScreenInfo); override;
    procedure AppProcessMessages; override;
    procedure AppWaitMessage; override;
    procedure AppTerminate; override;
    procedure AppMinimize; override;
    procedure AppRestore; override;
    procedure AppBringToFront; override;
    procedure AppSetTitle(const ATitle: string); override;
    function EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont; Callback: FontEnumExProc; Lparam: LParam; Flags: dword): longint; override;
    //function MessageBox(hWnd: HWND; lpText: PChar; lpCaption: PChar;  uType: Cardinal): Integer; override;
    function PromptUser(const DialogCaption: String; const DialogMessage: String; DialogType: LongInt; Buttons: PLongint; ButtonCount: LongInt; DefaultIndex: LongInt; EscapeResult: LongInt):LongInt; override;
    function RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap: HBITMAP; out AMask: HBITMAP; ASkipMask: Boolean = false):Boolean; override;
    function RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): boolean; override;
    function RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean; override;
    function RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean; override;
    function RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean; override;
    function RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean; override;
    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    // create and destroy
    function CreateTimer(Interval: integer; TimerFunc: TWSTimerProc) : THandle; override;
    function DestroyTimer(TimerHandle: THandle) : boolean; override;
    procedure DestroyLCLComponent(Sender: TObject);virtual;

    {$I muiwinapih.inc}
  public
  end;

//var
//  MUIWidgetSet: TMUIWidgetSet;

implementation

uses
  MUIWSFactory, MUIWSForms, VInfo;


{$I muiwinapi.inc}

{ TMUIWidgetSet }

function TMUIWidgetSet.GetAppHandle: THandle;
begin
  Result := THandle(MUIApp);
end;

procedure TMUIWidgetSet.PassCmdLineOptions;
begin
  inherited PassCmdLineOptions;
end;

function TMUIWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:=lpMUI;
end;

function TMUIWidgetSet.GetLCLCapability(ACapability: TLCLCapability): PtrUInt;
begin
  case ACapability of
    lcCanDrawOutsideOnPaint: Result := LCL_CAPABILITY_NO;
    lcDragDockStartOnTitleClick: Result := LCL_CAPABILITY_NO;
    lcNeedMininimizeAppWithMainForm: Result := LCL_CAPABILITY_NO;
    lcAsyncProcess: Result := LCL_CAPABILITY_NO;
    lcApplicationTitle: Result := LCL_CAPABILITY_YES;
    lcApplicationWindow:Result := LCL_CAPABILITY_YES;
    lcFormIcon: Result := LCL_CAPABILITY_NO;
    lcModalWindow: Result := LCL_CAPABILITY_NO;
    lcAntialiasingEnabledByDefault: Result := LCL_CAPABILITY_NO;
    lcLMHelpSupport: Result := LCL_CAPABILITY_NO;
  else
    Result := inherited GetLCLCapability(ACapability);
  end;
end;

var
  AppTitle, FinalVers, Vers, CopyR, Comment, prgName, Author: string;

procedure TMUIWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
type
  TVerArray = array[0..3] of Word;
var
  Info: TVersionInfo;
  i,j: Integer;
  TagList: TTagsList;

  function PV2Str(PV: TVerArray): String;
   begin
     Result := Format('%d.%d.%d.%d', [PV[0],PV[1],PV[2],PV[3]])
   end;

begin
  Vers := '';
  CopyR := '';
  Comment := '';
  prgName := Application.title;
  AppTitle := Application.title;
  try
    Info := TVersionInfo.Create;
    Info.Load(HINSTANCE);
    Vers := PV2Str(Info.FixedInfo.FileVersion);
    for i := 0 to Info.StringFileInfo.Count - 1 do
    begin
      for j := 0 to Info.StringFileInfo.Items[i].Count - 1 do
      begin
        if Info.StringFileInfo.Items[i].Keys[j] = 'LegalCopyright' then
        begin
          CopyR := Info.StringFileInfo.Items[i].Values[j];
        end else
        if Info.StringFileInfo.Items[i].Keys[j] = 'Comments' then
        begin
          Comment := Info.StringFileInfo.Items[i].Values[j];
        end else
        if Info.StringFileInfo.Items[i].Keys[j] = 'CompanyName' then
        begin
          Author := Info.StringFileInfo.Items[i].Values[j];
        end else
        if Info.StringFileInfo.Items[i].Keys[j] = 'ProductName' then
        begin
          if Length(Trim(Info.StringFileInfo.Items[i].Values[j])) > 0  then
            PrgName := Info.StringFileInfo.Items[i].Values[j];
        end;
      end;
    end;
    Info.Free;
  except
  end;
  FinalVers := '$VER: ' + PrgName + ' ' + Vers;
  AddTags(TagList, [
    //LongInt(MUIA_Application_Base), PChar(AppTitle),
    LongInt(MUIA_Application_Title), PChar(AppTitle),
    LongInt(MUIA_Application_Version), PChar(FinalVers),
    LongInt(MUIA_Application_Copyright), PChar(CopyR),
    LongInt(MUIA_Application_Description), PChar(Comment),
    LongInt(MUIA_Application_Author), PChar(Author)
    ]);
  MUIApp := TMuiApplication.create(GetTagPtr(TagList));
end;

procedure TMUIWidgetSet.AppProcessMessages;
begin;
  MuiApp.ProcessMessages;
end;

procedure TMUIWidgetSet.AppWaitMessage;
begin
  MuiApp.WaitMessages;
end;

procedure TMUIWidgetSet.AppTerminate;
begin

end;

procedure TMUIWidgetSet.AppMinimize;
begin
  MuiApp.Iconified := True;
end;

procedure TMUIWidgetSet.AppRestore;
begin
  MuiApp.Iconified := False;
end;

procedure TMUIWidgetSet.AppBringToFront;
begin

end;

procedure TMUIWidgetSet.AppSetTitle(const ATitle: string);
begin

end;

function TMUIWidgetSet.EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont;
  Callback: FontEnumExProc; Lparam: LParam; Flags: dword): longint;
begin
  Result:=0;
end;

(*
function TMUIWidgetSet.MessageBox(hWnd: HWND; lpText: PChar; lpCaption: PChar;
  uType: Cardinal): Integer;
begin
end;*)

function TMUIWidgetSet.PromptUser(const DialogCaption: String;
  const DialogMessage: String; DialogType: LongInt; Buttons: PLongint;
  ButtonCount: LongInt; DefaultIndex: LongInt; EscapeResult: LongInt): LongInt;
var
  ES: PEasyStruct;
  BtnText: string;
  Res: LongInt;
  BtnIdx : LongInt;
  BtnId: LongInt;
begin
  New(ES);
  ES^.es_StructSize := SizeOf(TEasyStruct);
  ES^.es_Flags := 0;
  ES^.es_Title := PChar(DialogCaption);
  ES^.es_TextFormat := PChar(DialogMessage);
  for BtnIdx := 0 to ButtonCount-1 do
  begin
    BtnID := Buttons[BtnIdx];
    if (BtnID >= Low(IdButtonTexts)) and (BtnID <= High(IdButtonTexts)) then
    begin
      if BtnIdx = 0 then
        BtnText := IdButtonTexts[BtnID]
      else
        BtnText := BtnText + '|'+ IdButtonTexts[BtnID];
    end else
    begin
      if BtnIdx = 0 then
        BtnText := IntToStr(BtnID)
      else
        BtnText := BtnText + '|'+ IntToStr(BtnID);
    end;
  end;
  ES^.es_GadgetFormat := PChar(BtnText);
  Res := EasyRequestArgs(nil, ES, nil, nil);
  Result := EscapeResult;
  if (Res >= 0) and (Res < ButtonCount) then
    Result := Buttons[Res];
  Dispose(ES);
end;

type
  TARGBPixel = packed record
    A: Byte;
    R: Byte;
    G: Byte;
    B: Byte;
  end;
  PARGBPixel = ^TARGBPixel;

  {TABGRPixel = packed record
    R: Byte;
    G: Byte;
    B: Byte;
    A: Byte;
  end;}
  TABGRPixel = array[0..3] of Byte;
  PABGRPixel = ^TABGRPixel;



function TMUIWidgetSet.RawImage_CreateBitmaps(const ARawImage: TRawImage; out
  ABitmap: HBITMAP; out AMask: HBITMAP; ASkipMask: Boolean): Boolean;
var
  Bit: TMUIBitmap;  
begin
  Bit := TMUIBitmap.create(ARawImage.Description.Width, ARawImage.Description.Height, ARawImage.Description.Depth);
  Move(ARawImage.Data^, Bit.FImage^, ARawImage.DataSize); 
  ABitmap := HBITMAP(Bit);
  AMask := 0;
  Result := True;
  //writeln(' create image: ', ARawImage.Description.Width,'x', ARawImage.Description.Height,' : ',ARawImage.Description.Depth, ' - ', ARawImage.DataSize, ' $', HexStr(Bit));
end;

function RawImage_DescriptionFromDrawable(out
  ADesc: TRawImageDescription; ACustomAlpha: Boolean
  ): boolean;
var
  Width, Height: integer;
  IsBitmap: Boolean;
begin
  Width := 0;
  Height := 0;
  IsBitMap := False;

  ADesc.Init;
  ADesc.Width := cardinal(Width);
  ADesc.Height := cardinal(Height);
  ADesc.BitOrder := riboBitsInOrder;
  ADesc.PaletteColorCount := 0;
  if ACustomAlpha then
  begin
    // always give pixbuf description for alpha images
    ADesc.Format:=ricfRGBA;
    ADesc.Depth := 32;
    ADesc.BitsPerPixel := 32;
    ADesc.LineEnd := rileDWordBoundary;
    ADesc.ByteOrder := riboLSBFirst;

    ADesc.RedPrec := 8;
    ADesc.RedShift := 0;
    ADesc.GreenPrec := 8;
    ADesc.GreenShift := 8;
    ADesc.BluePrec := 8;
    ADesc.BlueShift := 16;
    ADesc.AlphaPrec := 8;
    ADesc.AlphaShift := 24;

    ADesc.MaskBitsPerPixel := 1;
    ADesc.MaskShift := 0;
    ADesc.MaskLineEnd := rileByteBoundary;
    ADesc.MaskBitOrder := riboBitsInOrder;

    Exit(True);
  end;

  // Format
  if IsBitmap then
  begin
    ADesc.Format := ricfGray;
  end else
  begin
    ADesc.Format:=ricfRGBA;
    ADesc.RedPrec := 8;
    ADesc.RedShift := 0;
    ADesc.GreenPrec := 8;
    ADesc.GreenShift := 8;
    ADesc.BluePrec := 8;
    ADesc.BlueShift := 16;
    ADesc.AlphaPrec := 8;
    ADesc.AlphaShift := 24;

    ADesc.MaskBitsPerPixel := 1;
    ADesc.MaskShift := 0;
    ADesc.MaskLineEnd := rileByteBoundary;
    ADesc.MaskBitOrder := riboBitsInOrder;
  end;

  // Palette
  ADesc.PaletteColorCount:=0;

  // Depth
  if IsBitmap then
    ADesc.Depth := 1
  else
    ADesc.Depth := 32;

  if IsBitmap then
    ADesc.ByteOrder := riboMSBFirst
  else
    ADesc.ByteOrder := riboLSBFirst;

  ADesc.LineOrder := riloTopToBottom;

  case ADesc.Depth of
    0..8:   ADesc.BitsPerPixel := ADesc.Depth;
    9..16:  ADesc.BitsPerPixel := 16;
    17..32: ADesc.BitsPerPixel := 32;
  else
    ADesc.BitsPerPixel := 64;
  end;

  if IsBitmap then
  begin
    ADesc.LineEnd  := rileByteBoundary;
    ADesc.RedPrec  := 1;
    ADesc.RedShift := 0;
  end else
  begin
    // Try retrieving the lineend
    ADesc.LineEnd := rileDWordBoundary;
    ADesc.MaskBitsPerPixel := 1;
    ADesc.MaskShift := 0;
    ADesc.MaskLineEnd := rileByteBoundary;
    ADesc.MaskBitOrder := riboBitsInOrder;
  end;

  Result := True;
end;

function TMUIWidgetSet.RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): boolean;
begin
  ADesc.Init_BPP32_A8R8G8B8_BIO_TTB(0,0);
  ADesc.PaletteColorCount := 0;
  RawImage_DescriptionFromDrawable(ADesc, False);
  {$ifdef VERBOSEAROS}
  writeln('RawImage_DescriptionFromBitmap ', HexStr(Pointer(ABitmap)));
  {$endif}
  Result := True;
end;

function TMUIWidgetSet.RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean;
begin
  {$ifdef VERBOSEAROS}
  writeln('RawImage_DescriptionFromDevice ', HexStr(Pointer(ADC)));
  {$endif}
  RawImage_QueryDescription([riqfUpdate,riqfRGB], ADesc);
  Result := True;
end;

function TMUIWidgetSet.RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean;
begin
  ARawImage.Init;
  {$ifdef VERBOSEAROS}
  writeln('RawImage_FromBitmap');
  {$endif}
  RawImage_QueryDescription([riqfUpdate,riqfRGB], ARawImage.Description);
  Result := True;
end;

function TMUIWidgetSet.RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean;
begin
  ARawImage.Init;
  {$ifdef VERBOSEAROS}
  writeln('RawImage_FromDevice ', ARect.Right, ' x ', ARect.Bottom);
  {$endif}
  RawImage_QueryDescription([riqfUpdate,riqfRGB], ARawImage.Description);
  Result := True;
end;

function TMUIWidgetSet.RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean;
begin
  //if riqfAlpha in AFlags
  //then 
  begin
    //always return rgba description
    if not (riqfUpdate in AFlags)
    then ADesc.Init;

    ADesc.Format := ricfRGBA;
    ADesc.Depth := 32;
    ADesc.BitOrder := riboReversedBits;
    ADesc.ByteOrder := riboLSBFirst;
    ADesc.LineOrder := riloTopToBottom;
    ADesc.LineEnd := rileDWordBoundary;
    ADesc.BitsPerPixel := 32;

    ADesc.AlphaPrec := 8;
    ADesc.AlphaShift := 0;

    if riqfRGB in AFlags
    then begin
      ADesc.RedPrec := 8;
      ADesc.GreenPrec := 8;
      ADesc.BluePrec := 8;
      ADesc.RedShift := 8;
      ADesc.GreenShift := 16;
      ADesc.BlueShift := 24;
    end;


    {ADesc.AlphaPrec := 8;
    ADesc.AlphaShift := 24;

    if riqfRGB in AFlags
    then begin
      ADesc.RedPrec := 8;
      ADesc.GreenPrec := 8;
      ADesc.BluePrec := 8;
      ADesc.RedShift := 16;
      ADesc.GreenShift := 8;
      ADesc.BlueShift := 0;
    end;
    }
    AFlags := AFlags - [riqfRGB, riqfAlpha, riqfUpdate];
    if AFlags = [] then Exit(True);
    
    // continue with default
    Include(AFlags, riqfUpdate);
  end;
  //Result := inherited RawImage_QueryDescription(AFlags, ADesc);
  // reduce mem
  if Result and (ADesc.Depth = 24) 
  then ADesc.BitsPerPixel := 24;
end;

function TMUIWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
begin
  Result := 0;
end;

procedure TMUIWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
var
  Canvas: TMUICanvas;
  Tags: TTagsList;
begin
  Canvas := TMUICanvas(CanvasHandle);
  if Assigned(Canvas) then
  begin
    Canvas.SetPixel(X, Y, AColor);  
  end;
end;

constructor TMUIWidgetSet.Create;
begin
  inherited Create;
end;

destructor TMUIWidgetSet.Destroy;
begin
  inherited Destroy;
end;

function TMUIWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): THandle;
begin
  Result := 0;
  if Assigned(MUIApp) then
  begin
    Result := MUIApp.CreateTimer(Interval, TimerFunc);
  end;
end;

function TMUIWidgetSet.DestroyTimer(TimerHandle: THandle): boolean;
begin
  Result:=false;
  if Assigned(MUIApp) then
  begin
    Result := MUIApp.DestroyTimer(TimerHandle);
  end;
end;

procedure TMUIWidgetSet.DestroyLCLComponent(Sender: TObject);
begin

end;

function TMUIWidgetSet.CreateThemeServices: TThemeServices;
begin
  Result := TMUIThemeServices.Create;
end;


end.
