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
  Types, Classes, SysUtils, FPCAdds,
  // interfacebase
  InterfaceBase,
  // LCL
  Dialogs, Controls, Forms, LCLStrConsts, LMessages, stdctrls,
  LCLProc, LCLIntf, LCLType, GraphType, Graphics, Menus, Themes,
  //AROS
  Aroswinunit,
  MUIBaseUnit, exec, intuition, gadtools, mui, utility, AmigaDos,
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
    function MessageBox(hWnd: HWND; lpText: PChar; lpCaption: PChar;  uType: Cardinal): Integer; override;
    function PromptUser(const DialogCaption: String; const DialogMessage: String; DialogType: LongInt; Buttons: PLongint; ButtonCount: LongInt; DefaultIndex: LongInt; EscapeResult: LongInt):LongInt; override;
    function RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap: HBITMAP; out AMask: HBITMAP; ASkipMask: Boolean = false):Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    // create and destroy
    function CreateTimer(Interval: integer; TimerFunc: TWSTimerProc) : THandle; override;
    function DestroyTimer(TimerHandle: THandle) : boolean; override;
    procedure DestroyLCLComponent(Sender: TObject);virtual;

    function GetDC(hWnd: HWND):HDC; override;
    function MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean; override;
    function LineTo(DC: HDC; X, Y: Integer): Boolean; override;
  public
  end;

var
  MUIWidgetSet: TMUIWidgetSet;

implementation

uses
  MUIWSFactory, MUIWSForms, VInfo;

{ TMUIWidgetSet }

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

procedure TMUIWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
type
  TVerArray = array[0..3] of Word;
var
  Info: TVersionInfo;
  FinalVers, Vers, CopyR, Comment, prgName: string;
  i,j: Integer;

  function PV2Str(PV: TVerArray): String;
   begin
     Result := Format('%d.%d.%d.%d', [PV[0],PV[1],PV[2],PV[3]])
   end;

begin
  Vers := '';
  CopyR := '';
  Comment := '';
  prgName := Application.title;
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
  FinalVers := PrgName + ' ' + Vers;
  MUIApp := TMuiApplication.create([
    LongInt(MUIA_Application_Title), PChar(Application.title),
    LongInt(MUIA_Application_Version), PChar(FinalVers),
    LongInt(MUIA_Application_Copyright), PChar(CopyR),
    LongInt(MUIA_Application_Description), PChar(Comment),
    TAG_END]);
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

function TMUIWidgetSet.MessageBox(hWnd: HWND; lpText: PChar; lpCaption: PChar;
  uType: Cardinal): Integer;
begin
end;

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

function TMUIWidgetSet.RawImage_CreateBitmaps(const ARawImage: TRawImage; out
  ABitmap: HBITMAP; out AMask: HBITMAP; ASkipMask: Boolean): Boolean;
begin
  //writeln('create image');
  ABitmap := 1;
  AMask := 1;
  ASkipMask := True;
  Result := True;
end;

constructor TMUIWidgetSet.Create;
begin
  inherited Create;
end;

destructor TMUIWidgetSet.Destroy;
begin
  inherited Destroy;
end;

function TMUIWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc
  ): THandle;
begin
  Result:=0;
end;

function TMUIWidgetSet.DestroyTimer(TimerHandle: THandle): boolean;
begin
  Result:=false;
end;

procedure TMUIWidgetSet.DestroyLCLComponent(Sender: TObject);
begin

end;


function TMUIWidgetSet.GetDC(hWnd: HWND): HDC;

begin
  //writeln('getDC ', inttostr(hwnd));
  if TObject(hWnd) is TMUIObject then
  begin
    //writeln('ok is muiboj', TObject(hWnd).classname);
    Result := HDC(TArosWindow.create(TMUIObject(hwnd).obj));
  end else
    Result := 0;
  // TODO: Get Window to Draw!
end;

function TMUIWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint
  ): Boolean;
var
  NAW: TArosWindow;
begin
  //writeln('MoveTo', x, ' , ',y);
  Result := False;
  if (DC<>0) and (TObject(DC) is TArosWindow) then
  begin
    NAW := TArosWindow(DC);
    NAW.MoveTo(X,Y);
    Result := True;
  end;
  //Result:=inherited MoveToEx(DC, X, Y, OldPoint);
end;

function TMUIWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
var
  NAW: TArosWindow;
begin
  //writeln('LineTo', x, ' , ',y);
  Result := False;
  if (DC<>0) and (TObject(DC) is TArosWindow) then
  begin
    NAW := TArosWindow(DC);
    NAW.LineTo(X,Y);
    Result := True;
  end;
end;

end.
