unit MUIBaseUnit;

{$mode objfpc}{$H+}
interface

uses
  Classes, dos, SysUtils, Controls, Contnrs, Types,
  {$ifdef HASAMIGA}
  Exec, AmigaDos, agraphics, Intuition, Utility,Mui, inputevent, KeyMap,
  {$endif}
  Forms, LCLMessageGlue, lcltype, LMessages, interfacebase, muidrawing;

type
  TEventFunc = procedure(Hook: PHook; Obj: PObject_; Msg: Pointer); cdecl;
  TMUICaret = class
    Shown: Boolean;
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
  end;
  
  
  { TMUIObject }

  TMUIObject = class(TObject)
  private
    //EventHooks
    ButtonUp: THook;
    ButtonDown: THook;
    //Parent
    FParent: TMUIObject;
    // AWinControl lcl-Object
    FPasObject: TControl;
    FOnDraw: TNotifyEvent;
    FMuiCanvas: TMUICanvas;
    function GetEnabled: boolean;
    procedure SetEnabled(AValue: boolean);
  protected
    //Position
    FLeft, FTop, FWidth, FHeight: longint;
    //
    LayoutHook: THook;

    FGrpObj: pObject_;
    procedure SetAttribute(const Tags: array of const);
    function GetAttribute(tag: longword): longword;
    procedure SetAttObj(obje: pObject_; const Tags: array of const);
    function GetAttObj(obje: pObject_; tag: longword): longword;
    // DoMethod(Params = [MethodID, Parameter for Method ...])
    function DoMethodObj(Obje: pObject_; const Params: array of const): longint;
    function DoMethod(const Params: array of IPTR): longint;

    procedure SetParent(const AValue: TMUIObject); virtual;

    procedure AddChild(Child: TMUIObject); virtual;
    procedure RemoveChild(Child: TMUIObject); virtual;
    procedure SetVisible(const AValue: boolean); virtual;
    function GetVisible: boolean; virtual;

    procedure SetLeft(ALeft: integer); virtual;
    procedure SetTop(ATop: integer); virtual;
    procedure SetWidth(AWidth: integer); virtual;
    procedure SetHeight(AHeight: integer); virtual;

    function GetTop(): Integer; virtual;
    function GetLeft(): Integer; virtual;
    function GetWidth(): integer; virtual;
    function GetHeight(): integer; virtual;
    procedure InstallHooks; virtual;
    procedure DoReDraw(); virtual;
    //
    procedure BasicInitOnCreate(); virtual;
    procedure SetScrollbarPos;
    function GetParentWindow: TMUIObject; virtual;
  public
    EHNode: PMUI_EventHandlerNode;
    FChilds: TObjectList;
    FObject: pObject_;
    BlockRedraw: boolean;
    MUIDrawing: Boolean;
    Caret: TMUICaret;
    LastClick: Int64; // time of the last click -> for double click events
    NumMoves: Integer; // max 3 movements before lastclick is deleted;
    VScroll, HScroll: TMUIObject;
    VScrollPos, HScrollPos: Integer;
    constructor Create(ObjType: longint; const Params: array of const);
      overload; reintroduce; virtual;
    constructor Create(AClassName: PChar; Tags: PTagItem); overload;
      reintroduce; virtual;
    constructor Create(AClassType: PIClass; Tags: PTagItem); overload;
      reintroduce; virtual;
    destructor Destroy; override;

    procedure SetOwnSize; virtual;
    procedure Redraw; virtual;
    procedure DoMUIDraw; virtual;
    function GetClientRect: TRect; virtual;
    function GetWindowOffset: Types.TPoint; virtual;
    // scrollbars
    procedure CreateScrollbars;

    property Parent: TMUIObject read FParent write SetParent;
    property Left: longint read GetLeft write SetLeft;
    property Top: longint read GetTop write SetTop;
    property Width: longint read GetWidth write SetWidth;
    property Height: longint read GetHeight write SetHeight;
    property Obj: pObject_ read FObject write FObject;
    property GrpObj: pObject_ read FGrpObj;
    property PasObject: TControl read FPasObject write FPasObject;
    property Visible: boolean read GetVisible write SetVisible;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property MUICanvas: TMUICanvas read FMUICanvas;

    property OnDraw: TNotifyEvent read FOnDraw write FOnDraw;
  end;
  
  TMUIWinControl = class
    PasObject: TWinControl;
    Parent: TMUIObject;
  end;

  { TMuiArea }

  TMuiArea = class(TMUIObject)
  protected
    function GetChecked: longbool; virtual;
    procedure SetChecked(const AValue: longbool); virtual;
    function GetCaption: string; virtual;
    function GetDragable: boolean; virtual;
    function GetDropable: boolean; virtual;
    function GetEnabled: boolean; virtual;
    function GetHint: string; virtual;
    function GetTabStop: boolean; virtual;
    procedure SetCaption(const AValue: string); virtual;
    procedure SetDragable(const AValue: boolean); virtual;
    procedure SetDropable(const AValue: boolean); virtual;
    procedure SetEnabled(const AValue: boolean); virtual;
    procedure SetHint(const AValue: string); virtual;
    procedure SetTabStop(const AValue: boolean); virtual;
  public
    FBlockChecked: Boolean;
    property Caption: string read GetCaption write SetCaption;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property Dragable: boolean read GetDragable write SetDragable;
    property Dropable: boolean read GetDropable write SetDropable;
    property Hint: string read GetHint write SetHint;
    property Checked: longbool read GetChecked write SetChecked;
    property TabStop: boolean read GetTabStop write SetTabStop;
  end;

  TMUIGroup = class(TMUIArea)

  end;

  { TMUITimer }

  TMUITimer = class
    Func: TWSTimerProc;
    StartTime: Int64;
    InterVal: Int64;
    Handle: THandle;
    function CheckTimer: Boolean;
  end;

  { TMuiApplication }

  TMuiApplication = class(TMUIObject)
  private
    FTerminated: boolean;
    FSignals: longword;
    FMainWin: pObject_;
    FTimers: TObjectList;
    FInvalidatedObjects: TObjectList;
    FInsidePaint: Boolean;
    InRedrawList: Boolean;
    function GetIconified: boolean;
    procedure SetIconified(const AValue: boolean);
    procedure CheckTimer;
  protected
    procedure AddChild(Child: TMUIObject); override;
    procedure RemoveChild(Child: TMUIObject); override;
    procedure InstallHooks; override;
  public
    constructor Create(Tags: PTagItem); overload; reintroduce; virtual;
    destructor Destroy; override;
    procedure DoMUIDraw; override;
    function NewInput(Signals: PLongword): longword;
    procedure ProcessMessages;
    procedure WaitMessages;
    function CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): THandle;
    function DestroyTimer(TimerHandle: THandle): boolean;
    procedure AddInvalidatedObject(AObj: TMUIObject);
    procedure RemInvalidatedObject(AObj: TMUIObject);
    procedure RedrawList;
    property MainWin: pObject_ read FMainWin;
    property Terminated: boolean read FTerminated write FTerminated;
    property Iconified: boolean read GetIconified write SetIconified;
    property InsidePaint: Boolean read FInsidePaint write FInsidePaint;
  end;

procedure BtnDownFunc(Hook: PHook; Obj: PObject_; Msg: Pointer); cdecl;
procedure BtnUpFunc(Hook: PHook; Obj: PObject_; Msg: Pointer); cdecl;

var
  MUIApp: TMuiApplication;
  LCLGroupClass: PIClass;
  KeyState: Integer = 0;
  CaptureObj: TMUIObject = nil;

implementation

uses
  tagsarray, longarray, muiformsunit, muistdctrls;

var
  GroupSuperClass: PIClass;

procedure BtnDownFunc(Hook: PHook; Obj: PObject_; Msg: Pointer); cdecl;
var
  MuiObject: TMuiObject;
begin
  //writeln('-->btndown');
  if TObject(Hook^.h_Data) is TMuiObject then
  begin
    MuiObject := TMuiObject(Hook^.h_Data);
    LCLSendMouseDownMsg(TControl(MuiObject.PasObject), 0, 0, mbLeft, []);
  end;
  //writeln('<--btndown');
end;

procedure BtnUpFunc(Hook: PHook; Obj: PObject_; Msg: Pointer); cdecl;
var
  MuiObject: TMuiObject;
begin
  //writeln('-->btnup');
  if TObject(Hook^.h_Data) is TMuiObject then
  begin
    MuiObject := TMuiObject(Hook^.h_Data);
    LCLSendMouseUpMsg(TControl(MuiObject.PasObject), 0, 0, mbLeft, []);
    LCLSendClickedMsg(TControl(MuiObject.PasObject));
  end;
  //writeln('<--btnup');
end;

{ TMUITimer }

function TMUITimer.CheckTimer: Boolean;
var
  t: Int64;
begin
  Result := False;
  t := GetMsCount;
  if t - StartTime >= Interval then
  begin
    if Assigned(Func) then
      Func;
    StartTime := t;
    Result := True;
  end;
end;

{ TMUIObject }

// search for the parent window (can be many Parent relations)
// or nil if it is not connected to a Window
function TMUIObject.GetParentWindow: TMUIObject;
begin
  Result := Self;
  while Assigned(Result) and (not (Result is TMUIWindow)) do
    Result := Result.Parent;  
end;

procedure TMUIObject.SetParent(const AValue: TMUIObject);
var
  Win: TMUIObject;
begin
  //Writeln(self.classname, 'Set Parent: ', HexStr(AValue));
  if FParent = AValue then
  begin
    //writeln('same');
    Exit;
  end;
  // Unlink the old Parent
  if Assigned(FParent) then
  begin
    // if the Widget is the Focused Control, we have to remove it
    // or we will earn a crash after it gets destroyed or so
    // destroy always make an SetParent(nil)
    Win := GetParentWindow;
    if Assigned(Win) and (Win is TMUIWindow) then
    begin
      if TMUIWindow(Win).FocusedControl = self then
        TMUIWindow(Win).FocusedControl := nil;
    end;
    FParent.RemoveChild(Self);
    FParent.FChilds.Remove(Self);
    FParent := nil;
  end;
  // Link the new Parent
  if Assigned(AValue) then
  begin
    //write('  New: ', AValue.Classname, ' assigned: ', Assigned(AValue.FChilds));
    AValue.FChilds.Add(Self);
    AValue.AddChild(Self);
    FParent := AValue;
  end;
  //writeln('  done.');
end;

function TMUIObject.GetVisible: boolean;
begin
  //writeln('getvis');
  Result := boolean(GetAttribute(MUIA_ShowMe));
end;

procedure TMUIObject.SetVisible(const AValue: boolean);
begin
  //writeln('setVis ', AValue);
  SetAttribute([longint(MUIA_ShowMe), longint(AValue), TAG_END]);
end;

procedure TMUIObject.SetLeft(ALeft: integer);
begin
  FLeft := ALeft;
  SetScrollbarPos();
  if Assigned(Parent) then
    MUIApp.AddInvalidatedObject(Parent);
end;

procedure TMUIObject.SetTop(ATop: integer);
begin
  FTop := ATop;
  SetScrollbarPos();
  if Assigned(Parent) then
    MUIApp.AddInvalidatedObject(Parent);
end;

procedure TMUIObject.SetWidth(AWidth: integer);
begin
  FWidth := AWidth;
  SetScrollbarPos();
  if Assigned(Parent) then
    MUIApp.AddInvalidatedObject(Parent);
end;

procedure TMUIObject.SetHeight(AHeight: integer);
begin
  FHeight := AHeight;
  SetScrollbarPos();
  if Assigned(Parent) then
    MUIApp.AddInvalidatedObject(Parent);
end;

function TMUIObject.GetTop(): Integer;
begin
  Result := FTop;  
end;

function TMUIObject.GetLeft(): Integer;
begin
  Result := FLeft;
end;

function TMUIObject.GetWidth(): integer;
begin
  Result := FWidth;
end;

function TMUIObject.GetHeight(): integer;
begin
  Result := FHeight;
end;

procedure TMUIObject.DoReDraw();
var
  PS: PPaintStruct;
  i: integer;
begin
  if Assigned(PasObject) then
  begin
    new(PS);
    FillChar(PS^, SizeOf(TPaintStruct), 0);
    PS^.hdc := THandle(Pointer(FMuiCanvas));
    PS^.rcPaint := FMuiCanvas.DrawRect;
    //writeln('Send paintmessage to ', pasobject.classname);
    MUIApp.InsidePaint := True;
    try
      if not MUIDrawing then
        LCLSendEraseBackgroundMsg(TWinControl(PasObject), PS^.hdc);
      LCLSendPaintMsg(TControl(PasObject), PS^.hdc, PS);
    finally
      MUIApp.InsidePaint := False;
    end;  
    Dispose(PS);
  end;
  FMUICanvas.DeInitCanvas;
  for i := 0 to FChilds.Count - 1 do
  begin
    if FChilds.Items[i] is TMUIObject then
    begin
      if TMuiObject(FChilds.Items[i]).Visible then
        TMuiObject(FChilds[i]).DoMuiDraw;
    end;  
  end;
end;

procedure TMUIObject.DoMUIDraw();
begin
  MUI_Redraw(FObject, MADF_DRAWOBJECT);
end;

function TMUIObject.GetClientRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right:= FWidth;
  Result.Bottom := FHeight;
  if Assigned(VSCroll) and Assigned(VScroll) then
  begin
    if VScroll.Visible then    
      Result.Right:= Result.Right - 15;
    if HScroll.Visible then  
      Result.Bottom := Result.Bottom - 15;
  end;
end;

function TMUIObject.GetWindowOffset: Types.TPoint;
var
  P: Types.TPoint;
begin
  Result.X := Left;
  Result.Y := Top;
  if Assigned(Parent) then
  begin
    P := Parent.GetWindowOffset;
    Result.X := Result.X + P.X;
    Result.Y := Result.Y + P.Y;
  end;
end;

procedure TMUIObject.SetAttObj(obje: pObject_; const Tags: array of const);
var
  TagList: TTagsList;
begin
  AddTags(TagList, Tags);
  SetAttrsA(obje, GetTagPtr(TagList));
end;

function TMUIObject.GetAttObj(obje: pObject_; tag: longword): longword;
var
  Res: longword;
begin
  GetAttr(tag, obje, @Res);
  Result := Res;
end;

function TMUIObject.DoMethodObj(Obje: pObject_; const Params: array of const): longint;
var
  Tags: TTagsList;
begin
  AddTags(Tags, Params);
  Result := CallHookPkt(PHook(OCLASS(Obje)), Obje, GetTagPtr(Tags));
end;

function TMUIObject.GetEnabled: boolean;
begin
  Result := not boolean(GetAttribute(MUIA_Disabled));
end;

procedure TMUIObject.SetEnabled(AValue: boolean);
begin
  if AValue then
    SetAttribute([longint(MUIA_Disabled), LFalse, TAG_END])
  else
    SetAttribute([longint(MUIA_Disabled), LTrue, TAG_END]);
end;

procedure TMUIObject.SetAttribute(const Tags: array of const);
var
  TagList: TTagsList;
begin
  AddTags(TagList, Tags);
  SetAttrsA(FObject, GetTagPtr(TagList));
end;

function TMUIObject.GetAttribute(tag: longword): longword;
var
  Res: longword;
begin
  GetAttr(tag, FObject, @Res);
  Result := Res;
end;

function TMUIObject.DoMethod(const Params: array of IPTR): longint;
begin
  Result := CallHookPkt(PHook(OCLASS(FObject)), FObject, @(Params[0]));
end;

procedure TMUIObject.AddChild(Child: TMUIObject);
begin
  if Assigned(Child.Obj) then
  begin
    DoMethod([IPTR(MUIM_Group_InitChange)]);
    DoMethod([IPTR(OM_ADDMEMBER), IPTR(Child.obj)]);
    DoMethod([IPTR(MUIM_Group_ExitChange)]);
  end;
end;

procedure TMUIObject.RemoveChild(Child: TMUIObject);
begin
  if Assigned(Child.obj) then
  begin
    //writeln('Remove Child: ',self.classname,' addr:', inttoHex(Cardinal(FObject),8));
    DoMethod([IPTR(OM_REMMEMBER), IPTR(Child.obj)]);
  end;
end;

procedure TMUIObject.InstallHooks;
begin
  ButtonUp.h_Entry := IPTR(@BtnUpFunc);
  ButtonUp.h_SubEntry := 0;//IPTR(@BtnUpFunc);
  ButtonUp.h_Data := Self;
  ButtonDown.h_Entry := IPTR(@BtnDownFunc);
  ButtonDown.h_SubEntry := 0;//IPTR(@BtnDownFunc);
  ButtonDown.h_Data := Self;


  DoMethod([IPTR(MUIM_Notify), IPTR(MUIA_Pressed), IPTR(True),
    IPTR(MUIV_Notify_Self), 2, IPTR(MUIM_CallHook), IPTR(@ButtonDown)]);
  DoMethod([IPTR(MUIM_Notify), IPTR(MUIA_Pressed), IPTR(False),
    IPTR(MUIV_Notify_Self), 2, IPTR(MUIM_CallHook), IPTR(@ButtonUp)]);
  //if (self is TMUIArea) and not (self is TMUIScrollbar)then  
  //  CreateScrollbars;
end;

procedure TMUIObject.BasicInitOnCreate();
begin
  Caret := nil;
  EHNode := nil;
  MUIDrawing := False;
  FMUICanvas := TMUICanvas.Create;
  FMUICanvas.MUIObject := self;
  BlockRedraw := False;
  FChilds := TObjectList.Create(False);
  FParent := nil;
  VScroll := nil;
  HSCroll := nil;
end;

constructor TMUIObject.Create(ObjType: longint; const Params: array of const);
begin
  inherited Create;
  BasicInitOnCreate;
  //writeln(self.classname, 'create obj ', ObjType);
  FObject := MUI_MakeObject(ObjType, Params);
  //writeln('create obj: ',self.classname,' addr:', inttoHex(Cardinal(FObject),8));
  InstallHooks;
end;

constructor TMUIObject.Create(AClassName: PChar; Tags: PTagItem);
begin
  inherited Create;
  BasicInitOnCreate();
  //writeln(self.classname, 'create class ', classname);
  FObject := MUI_NewObjectA(AClassName, Tags);
  InstallHooks;
  //writeln('create class: ',self.classname,' addr:', inttoHex(Cardinal(FObject),8));
end;

constructor TMUIObject.Create(AClassType: PIClass; Tags: PTagItem);
begin
  inherited Create;
  BasicInitOnCreate();
  //writeln(self.classname, 'create type');
  FObject := NewObjectA(AClassType, nil, Tags);  
  if Assigned(FObject) then
    Pointer(INST_DATA(AClassType, Pointer(FObject))^) := Self;
  InstallHooks;
  //writeln('create classtype: ',self.classname,' addr:', inttoHex(Cardinal(FObject),8));
end;

destructor TMUIObject.Destroy;
begin
  BlockRedraw := True;
  //writeln(self.classname, '--> muiobject destroy');
  SetParent(nil); 
  MUI_DisposeObject(FObject);
  FChilds.Free;
  FMUICanvas.Free;
  if not (self is TMUIApplication) then
    MUIApp.RemInvalidatedObject(Self);
  inherited Destroy;
  //writeln(self.classname, '<-- muiobject destroy');
end;

procedure TMUIObject.CreateScrollbars;
var
  Tags1, Tags2: TTagsList;
begin
  //writeln(pasobject.classname, ' create scrollbars');
  AddTags(Tags1, [MUIA_Group_Horiz, False]);
  VScroll := TMUIScrollBar.Create(Tags1);
  VScroll.PasObject := Self.PasObject;
  VScroll.Parent := self;
  VScroll.Visible := False;
  AddTags(Tags2, [MUIA_Group_Horiz, True]);
  HScroll := TMUIScrollBar.Create(Tags2);
  HScroll.PasObject := Self.PasObject;
  HScroll.Parent := Self;
  HScroll.Visible := False;
  if pasobject is TWIncontrol then
    TWinControl(pasobject).InvalidateClientRectCache(True);
end;

procedure TMUIObject.SetScrollbarPos;
begin
  if Assigned(VScroll) then
  begin
    if VScroll.Visible then
    begin
      VScroll.Width := 15;
      VScroll.Left := FWidth -  VScroll.Width;
      VScroll.Top := 0;
      VScroll.Height := FHeight - 15;
    end;
  end;
  if Assigned(HScroll) then
  begin
    if HScroll.Visible then
    begin
      HScroll.Height := 15;
      HScroll.Top := FHeight - HScroll.Height;
      HScroll.Left := 0;
      HScroll.Width := FWidth - 15;
    end;
  end;
end;

procedure TMUIObject.SetOwnSize;
var
  i: longint;
begin
  //writeln(self.classname, '-->setownsize');
  if not Assigned(FObject) then
    Exit;
  //writeln(self.classname,' setsize ', FLeft, ', ', FTop, ' - ', FWidth, ', ', FHeight,' count: ', Fchilds.Count, ' obj ', HexStr(FObject));
  MUI_Layout(FObject, FLeft, FTop, FWidth, FHeight, 0);    
  //writeln(self.classname, '  setsize done');
  for i := 0 to FChilds.Count - 1 do
  begin
    //writeln(self.classname, '  Child ', i);
    if FChilds.Items[i] is TMUIObject then
      TMuiObject(FChilds.Items[i]).SetOwnSize;
  end;
  //writeln(self.classname, '<--setownsize');
end;



procedure TMUIObject.Redraw;
begin
  if BlockRedraw then
  begin
    Exit;
  end;
  DoMethod([IPTR(MUIM_Group_InitChange)]);
  DoMethod([IPTR(MUIM_Group_ExitChange)]);
end;

{ TMuiApplication }

function TMuiApplication.GetIconified: boolean;
begin
  Result := boolean(GetAttribute(MUIA_Application_Iconified));
end;

procedure TMuiApplication.SetIconified(const AValue: boolean);
begin
  SetAttribute([IPTR(MUIA_Application_Iconified), IPTR(AValue), IPTR(TAG_END)]);
end;

procedure TMuiApplication.CheckTimer;
var
  i: Integer;
begin
  for i := 0 to FTimers.Count - 1 do
  begin
    TMUITimer(FTimers.items[i]).CheckTimer;
  end;
end;

procedure TMuiApplication.AddChild(Child: TMUIObject);
begin
  inherited AddChild(Child);
  if FMainWin = nil then
  begin
    FMainWin := Child.obj;
    SetAttribute([IPTR(MUIA_Application_Window), child.obj, TAG_END]);
    CallHook(PHook(OCLASS(FMainWin)), FMainWin,
      [IPTR(MUIM_Notify), IPTR(MUIA_Window_CloseRequest), True,
      longword(FObject), 2, IPTR(MUIM_Application_ReturnID),
      IPTR(MUIV_Application_ReturnID_Quit)]);
  end;
end;

procedure TMuiApplication.RemoveChild(Child: TMUIObject);
begin
  inherited RemoveChild(Child);
  if Child.obj = FMainWin then
  begin
    FMainWin := nil;
    SetAttribute([IPTR(MUIA_Application_Window), nil, TAG_END]);
  end;
end;

procedure TMuiApplication.InstallHooks;
begin
end;

constructor TMuiApplication.Create(Tags: PTagItem);
begin
  inherited Create(MUIC_Application, Tags);
  FSignals := 0;
  FTimers := TObjectList.Create;
  FTimers.OwnsObjects := True;
  FInvalidatedObjects := TObjectList.Create;
  FInvalidatedObjects.OwnsObjects := False;
  InRedrawList := False;
end;

destructor TMuiApplication.Destroy;
begin
  FTimers.Free;
  FInvalidatedObjects.Free;
  inherited Destroy;
end;

function TMuiApplication.NewInput(Signals: PLongword): longword;
begin
  Result := DoMethod([IPTR(Signals)]);
end;

procedure TMuiApplication.DoMUIDraw;
begin
  //writeln('MUI Draw for application called');
end;

procedure TMuiApplication.ProcessMessages;
begin
  RedrawList; 
  CheckTimer;
  if IPTR(DoMethod([IPTR(MUIM_Application_NewInput), IPTR(@FSignals)])) =
    MUIV_Application_ReturnID_Quit then
  begin    
    //writeln('got terminate1'); // no need to terminate self, LCL will do it for us
    //Application.Terminate;
    Exit;
  end;
end;

procedure TMuiApplication.WaitMessages;
begin
  RedrawList;
  CheckTimer;
  if DoMethod([IPTR(MUIM_Application_NewInput), IPTR(@FSignals)]) =
    MUIV_Application_ReturnID_Quit then
  begin
    //writeln('got terminate2');
    //Application.Terminate;
    Exit;
  end;
  if (FSignals <> 0) then
  begin
    FSignals := CheckSignal(FSignals or SIGBREAKF_CTRL_C);
    if FTerminated or ((FSignals and SIGBREAKF_CTRL_C) <> 0) then
    begin
      //writeln('got terminate3');
      Application.Terminate;
      Exit;
    end;
    Sleep(25);
  end;
  CheckTimer;
end;

function TMuiApplication.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc
  ): THandle;
var
  NewTimer: TMUITimer;
begin
  NewTimer := TMUITimer.create;
  NewTimer.StartTime := GetmsCount;
  NewTimer.Interval := Interval;
  NewTimer.Func := TimerFunc;
  NewTimer.Handle := THandle(NewTimer);
  FTimers.Add(NewTimer);
  Result := NewTimer.Handle;
end;

function TMuiApplication.DestroyTimer(TimerHandle: THandle): boolean;
begin
  Result := True;
  if TimerHandle <> 0 then
    FTimers.Remove(TObject(TimerHandle));
end;

procedure TMuiApplication.AddInvalidatedObject(AObj: TMUIObject);
var
  Index: Integer;
begin
  if not Assigned(AObj) then
    Exit;
  Index := FInvalidatedObjects.IndexOf(AObj);
  if Index >= 0 then 
    Exit;
  FInvalidatedObjects.Add(AObj);  
end;

procedure TMuiApplication.RemInvalidatedObject(AObj: TMUIObject);
var
  Index: Integer;
begin
  if not Assigned(AObj) then
    Exit;
  Index := FInvalidatedObjects.IndexOf(AObj);
  if Index < 0 then 
    Exit;  
  FInvalidatedObjects.Delete(Index);  
end;

procedure TMuiApplication.RedrawList;
var
  ActObj: TMUIObject;
begin
  if InRedrawList then
    Exit;
  InRedrawList := True;
  try
    while FInvalidatedObjects.Count > 0 do
    begin
      ActObj := TMUIObject(FInvalidatedObjects.Items[0]);
      FInvalidatedObjects.Delete(0);
      ActObj.DoMUIDraw;
    end;
  finally
    InRedrawList := False;
  end;
end;


{ TMuiArea }

function TMuiArea.GetChecked: longbool;
begin
  Result := boolean(GetAttribute(MUIA_Selected));
end;

procedure TMuiArea.SetChecked(const AValue: longbool);
begin
  if Checked = AValue then
    Exit;
  FBlockChecked := True;
  SetAttribute([IPTR(MUIA_Selected), IPTR(AValue), TAG_END]);
  FBlockChecked := False;
end;

function TMuiArea.GetCaption: string;
var
  Pc: PChar;
begin
  Result := '';
  Pc := PChar(GetAttribute(MUIA_Text_Contents));
  if Assigned(PC) then
    Result := string(Pc);
end;

function TMuiArea.GetDragable: boolean;
begin
  Result := boolean(GetAttribute(MUIA_Draggable));
end;

function TMuiArea.GetDropable: boolean;
begin
  Result := boolean(GetAttribute(MUIA_Dropable));
end;

function TMuiArea.GetEnabled: boolean;
begin
  Result := not boolean(GetAttribute(MUIA_Disabled));
end;

function TMuiArea.GetHint: string;
begin
  Result := string(PChar(GetAttribute(MUIA_ShortHelp)));
end;

procedure TMuiArea.SetCaption(const AValue: string);
begin
  SetAttribute([IPTR(MUIA_Text_Contents), PChar(AValue), TAG_END]);
end;

procedure TMuiArea.SetDragable(const AValue: boolean);
begin
  SetAttribute([IPTR(MUIA_Draggable), IPTR(AValue), TAG_END]);
end;

procedure TMuiArea.SetDropable(const AValue: boolean);
begin
  SetAttribute([IPTR(MUIA_Dropable), IPTR(AValue), TAG_END]);
end;

procedure TMuiArea.SetEnabled(const AValue: boolean);
var
  NValue: longbool;
begin
  NValue := not AValue;
  SetAttribute([IPTR(MUIA_Disabled), IPTR(NValue), TAG_END]);
end;

procedure TMuiArea.SetHint(const AValue: string);
begin
  SetAttribute([IPTR(MUIA_ShortHelp), PChar(AValue), TAG_END]);
end;

function TMuiArea.GetTabStop: boolean;
begin
  Result := GetAttribute(IPTR(MUIA_CycleChain)) <> 0;
end;

procedure TMuiArea.SetTabStop(const AValue: boolean);
var
  Val: Integer;
begin
  if AValue then
    Val := 1
  else
    Val := 0;  
  SetAttribute([IPTR(MUIA_CycleChain), IPTR(Val)]);
end;

{$PACKRECORDS 4}
type
  TehNode = record
    ehn_Node: TNode;
    ehn_Flags: Word;
    ehn_Object: PObject_;
    ehn_Class: PIClass;
    ehn_Events: ULONG;
    ehn_Priority: Byte;
  end;

function RawKeyToKeycode(RawKey: Byte): Word;
const
  TranslTable: array[Byte] of Integer = (
    -1,		// $00  
    49,		// $01  1
    50,		// $02  2
    51,		// $03  3
    52,		// $04  4
    53,		// $05  5
    54,		// $06  6
    55,		// $07  7
    56,		// $08  8
    57,		// $09  9
    58,		// $0a  0
    59,			// $0b
    187,		// $0c  //VK_CLEAR?
    VK_Return,		// $0d
    -1,			// $0e
    VK_NUMPAD0,			// $0f
    -1,		// $10  
    -1,		        // $11  
    -1,		// $12  e
    -1,		        // $13  
    -1,			// $14  
    -1,			// $15  
    -1,			// $16
    -1,			// $17  
    -1,			// $18  
    -1,			// $19  
    -1,			// $1a
    -1,		  // $1b  
    -1,			// $1c  
    VK_NUMPAD1,			// $1d  
    VK_NUMPAD2,			// $1e  
    VK_NUMPAD3, //keyModeSwitch,	// $1f  
    -1,		// $20  a
    -1,//keyPrior,		// $21  
    -1,		// $22  d
    -1,                 // $23  
    -1,		        // $24  
    -1,		        // $25  
    -1,		        // $26  
    -1,		        // $27  
    -1,		        // $28  
    VK_Select,		// $29  
    145,//keyPrintScreen,	// $2a  
    146, //keyExecute,		// $2b  
    147, //keyPrintScreen,	// $2c  
    VK_NUMPAD4,		// $2d  
    VK_NUMPAD5,		// $2e  
    VK_NUMPAD6,		// $2f  
    -1,		// $30  
    -1,		// $31  
    -1,		// $32  
    -1,		// $33  c
    -1,		// $34  
    -1,		// $35  b
    -1,		// $36  
    -1,		// $37  
    188,		// $38  
    190,		// $39  
    189,			// $3a
    -1,			// $3b
    $6c,			// $3c
    VK_NUMPAD7,			// $3d
    VK_NUMPAD8,			// $3e
    VK_NUMPAD9,			// $3f
    $20,		// $40 
    VK_BACK,			// $41 
    VK_TAB, // $42 
    -1,			// $43 
    -1,			// $44 
    -1,			// $45 
    VK_DELETE,	// $46 
    VK_INSERT,	// $47
    VK_PRIOR,		// $48
    VK_NEXT,	// $49
    -1,			// $4a
    VK_F11,             // $4b  'K'
    VK_Up,		// $4c  'L'
    VK_Down,		// $4d  'M'
    VK_Right,		// $4e  'N'
    VK_Left,		// $4f  'O'
    VK_F1,		// $50  'P'
    VK_F2,		// $51  'Q'
    VK_F3,		// $52  'R'
    VK_F4,		// $53  'S'
    VK_F5,		// $54  'T'
    VK_F6,		// $55  'U'
    VK_F7,		// $56  'V'
    VK_F8,		// $57  'W'
    VK_F9,		// $58  'X'
    VK_F10,		// $59  'Y'
    VK_NumLock,		// $5a  'Z'
    VK_DIVIDE,			// $5b  VK_LWIN
    VK_MULTIPLY,			// $5c  VK_RWIN
    VK_SUBTRACT,		// $5d  VK_APPS
    VK_ADD,			// $5e
    VK_Pause,		// $5f  VK_SLEEP
    VK_LShift,		// $60
    VK_LShift,	        // $61
    VK_CAPITAL,            // $62
    VK_CONTROL,	        // $63
    VK_MENU,	        // $64
    $e6,//VK_RMENU,	        // $65
    VK_LWIN,	        // $66
    VK_RWIN, //VK_P7,		// $67
    -1, //VK_P8,		// $68
    -1, //VK_P9,		// $69
    -1, //VK_PAsterisk,	// $6a
    -1, //VK_PPlus,		// $6b
    -1, //VK_PSeparator,	// $6c
    -1, //VK_PMinus,		// $6d
    -1, //VK_PDecimal,	// $6eL
    VK_F12,		// $6f
    VK_Home,		// $70  VK_F1
    VK_End,		// $71  VK_F2
    -1,		        // $72  VK_F3
    -1,		        // $73  VK_F4
    -1,		        // $74  VK_F5
    -1,		        // $75  VK_F6
    -1,		        // $76  VK_F7
    -1,		        // $77  VK_F8
    -1,		        // $78  VK_F9
    -1,		        // $79  VK_F10
    -1,		        // $7a  VK_F11
    VK_F12,		// $7b  VK_F12
    VK_F13,		// $7c  VK_F13
    VK_F14,		// $7d  VK_F14
    VK_F15,		// $7e  VK_F15
    VK_F16,		// $7f  VK_F16
    VK_F17,		// $80  VK_F17
    VK_F18,		// $81  VK_F18
    VK_F19,		// $82  VK_F19
    VK_F20,		// $83  VK_F20
    VK_F21,		// $84  VK_F21
    VK_F22,		// $85  VK_F22
    VK_F23,		// $86  VK_F23
    VK_F24,		// $87  VK_F24
    -1,			// $88
    -1,			// $89
    -1,			// $8a
    -1,			// $8b
    -1,			// $8c
    -1,			// $8d
    -1,			// $8e
    -1,			// $8f
    VK_NumLock,		// $90  VK_NUMLOCK
    VK_Scroll,		// $91  VK_SCROLL
    -1,			// $92  VK_OEM_NEC_EQUAL
    -1,			// $93  VK_OEM_FJ_MASSHOU
    -1,			// $94  VK_OEM_FJ_TOUROKU
    -1,			// $95  VK_OEM_FJ_LOYA
    -1,			// $96  VK_OEM_FJ_ROYA
    -1,			// $97
    -1,			// $98
    -1,			// $99
    -1,			// $9a
    -1,			// $9b
    -1,			// $9c
    -1,			// $9d
    -1,			// $9e
    -1,			// $9f
    -1, //VK_ShiftL,		// $a0  VK_LSHIFT
    -1, //VK_ShiftR,		// $a1  VK_RSHIFT
    -1, //VK_CtrlL,		// $a2  VK_LCONTROL
    -1, //VK_CtrlR,		// $a3  VK_RCONTROL
    -1,			// $a4  VK_LMENU
    -1,			// $a5  VK_RMENU
    -1,			// $a6  VK_BROWSER_BACK
    -1,			// $a7  VK_BROWSER_FORWARD
    -1,			// $a8  VK_BROWSER_REFRESH
    -1,			// $a9  VK_BROWSER_STOP
    -1,			// $aa  VK_BROWSER_SEARCH
    -1,			// $ab  VK_BROWSER_FAVORITES
    -1,			// $ac  VK_BROWSER_HOME
    -1,			// $ad  VK_VOLUME_MUTE
    -1,			// $ae  VK_VOLUME_DOWN
    -1,			// $af  VK_VOLUME_UP
    -1,			// $b0  VK_MEDIA_NEXT_TRACK
    -1,			// $b1  VK_MEDIA_PREV_TRACK
    -1,			// $b2  VK_MEDIA_STOP
    -1,			// $b3  VK_MEDIA_PLAY_PAUSE
    -1,			// $b4  VK_LAUNCH_MAIL
    -1,			// $b5  VK_LAUNCH_MEDIA_SELECT
    -1,			// $b6  VK_LAUNCH_APP1
    -1,			// $b7  VK_LAUNCH_APP2
    -1,			// $b8
    -1,			// $b9
    -1, {U Umlaut}	// $ba  VK_OEM_1
    -1, {+ char}	// $bb  VK_OEM_PLUS
    -1, {, char}	// $bc  VK_OEM_COMMA
    -1, {- char}	// $bd  VK_OEM_MINUS
    -1, {. char}	// $be  VK_OEM_PERIOD
    -1, {# char}	// $bf  VK_OEM_2
    -1, {O Umlaut}	// $c0  VK_OEM_3
    -1,			// $c1
    -1,			// $c2
    -1,			// $c3
    -1,			// $c4
    -1,			// $c5
    -1,			// $c6
    -1,			// $c7
    -1,			// $c8
    -1,			// $c9
    -1,			// $ca
    -1,			// $cb
    -1,			// $cc
    -1,			// $cd
    -1,			// $ce
    -1,			// $cf
    -1,			// $d0
    -1,			// $d1
    -1,			// $d2
    -1,			// $d3
    -1,			// $d4
    -1,			// $d5
    -1,			// $d6
    -1,			// $d7
    -1,			// $d8
    -1,			// $d9
    -1,			// $da
    -1,			// $db  VK_OEM_4
    -1, //VK_DeadCircumflex,	// $dc  VK_OEM_5
    -1, //VK_DeadAcute,	// $dd  VK_OEM_6
    -1, {A Umlaut}	// $de  VK_OEM_7
    -1,    	        // $df  VK_OEM_8
    -1,			// $e0
    -1,			// $e1  VK_OEM_AX
    -1, {< char}	// $e2  VK_OEM_102
    -1,			// $e3  VK_ICO_HELP
    -1, //VK_P5,		// $e4  VK_ICO_00
    -1,			// $e5  VK_PROCESSKEY
    -1,			// $e6  VK_ICO_CLEAR
    -1,			// $e7  VK_PACKET
    -1,			// $e8
    -1,			// $e9  VK_OEM_RESET
    -1,			// $ea  VK_OEM_JUMP
    -1,			// $eb  VK_OEM_PA1
    -1,			// $ec  VK_OEM_PA2
    -1,			// $ed  VK_OEM_PA3
    -1,			// $ee  VK_OEM_WSCTRL
    -1,			// $ef  VK_OEM_CUSEL
    -1,			// $f0  VK_OEM_ATTN
    -1,			// $f1  VK_OEM_FINISH
    -1,			// $f2  VK_OEM_COPY
    -1,			// $f3  VK_OEM_AUTO
    -1,			// $f4  VK_OEM_ENLW
    -1,			// $f5  VK_OEM_BACKTAB
    -1,			// $f6  VK_ATTN
    -1,			// $f7  VK_CRSEL
    -1,			// $f8  VK_EXSEL
    -1,			// $f9  VK_EREOF
    -1,			// $fa  VK_PLAY
    -1,			// $fb  VK_ZOOM
    -1,			// $fc  VK_NONAME
    -1,			// $fd  VK_PA1
    -1,			// $fe  VK_OEM_CLEAR
    -1			// $ff
  );
begin
  Result := 0;
  if TranslTable[RawKey]  = -1 then
    Result := 0
  else
    Result := TranslTable[RawKey];
  //writeln('tranbslate Key ', RawKey, ' $',IntToHex(RawKey, 2),' -> ', Result);  
end;

function KeyboardShiftState(State: Word): PtrInt;
begin
  Result := 0;
  if State and IEQUALIFIER_LALT <> 0 then
    Result := Result or $20000000;
  if State and IEQUALIFIER_RALT <> 0 then
    Result := Result or $20000000;
end;

function Dispatcher(cl: PIClass; Obj: PObject_; Msg: intuition.PMsg): longword; cdecl;
var
  //AskMsg: PMUIP_AskMinMax;
  ri: PMUI_RenderInfo;
  rp: PRastPort;
  PT: Types.TPoint;
  //Region: PRegion;
  //r: TRectangle;
  clip: Pointer;
  MUIB: TMUIObject;
  MUIParent: TMUIObject;
  p: TMUIObject;
  HIMsg: PMUIP_HandleInput;
  HEMsg: PMUIP_HandleEvent;
  iMsg: PIntuiMessage;
  //ehN: ^TehNode;
  winObj: PObject_;
  Depth: Integer;
  cap: string;
  relX, relY: Integer;
  Buff: array[0..19] of Char;
  Ret: Integer;
  CharCode: Word;
  KeyData: Word;
  KeyUp: Boolean;
  ie: TInputEvent;
  Win: PWindow;
  CurTime: Int64;
  MUIWin: TMUIWindow;
  Buffered: Boolean;
  WithScrollbars: Boolean;
  PaintH, PaintW: Integer;
  IsSysKey: Boolean;
  EatEvent: Boolean;
  Key: Char;
  i: Integer;
begin 
  //write('Enter Dispatcher with: ', Msg^.MethodID);
  case Msg^.MethodID of
    MUIM_SETUP: begin
      //writeln(' setup');
      Result := DoSuperMethodA(cl, obj, msg);
      MUIB := TMUIObject(INST_DATA(cl, Pointer(obj))^);
      if Assigned(MUIB) then
      begin
        New(MUIB.EHNode);
        FillChar(MUIB.EHNode^, SizeOf(MUIB.EHNode^), 0);
        P := MUIB;
        MUIB.EHNode^.ehn_Priority := -100;
        repeat
          Inc(MUIB.EHNode^.ehn_Priority);
          p := p.Parent;
        until P = nil;

        MUIB.EHNode^.ehn_Flags := 0;
        MUIB.EHNode^.ehn_Object := obj;
        MUIB.EHNode^.ehn_Class := cl;
        MUIB.EHNode^.ehn_Events := IDCMP_MOUSEBUTTONS or IDCMP_MOUSEMOVE or IDCMP_RAWKEY;
        winObj := OBJ_win(obj);
        ri := MUIRenderInfo(Obj);
        WinObj := ri^.mri_WindowObject;
        DoMethod(WinObj,MUIM_Window_AddEventHandler,[IPTR(MUIB.EHNode)]);    
      end;
      //MUI_RequestIDCMP(Obj, IDCMP_MOUSEBUTTONS);
    end;
    MUIM_CLEANUP: begin
      //write(' cleanup');
      MUIB := TMUIObject(INST_DATA(cl, Pointer(obj))^);
      if Assigned(MUIB) then
      begin
        DoMethod(OBJ_win(obj),MUIM_Window_RemEventHandler,[IPTR(MUIB.EHNode)]);
        Dispose(MUIB.EHNode);
        MUIB.EHNode := nil;
      end;
      Result := DoSuperMethodA(cl, obj, msg);
      //MUI_RejectIDCMP(Obj, IDCMP_MOUSEBUTTONS);
    end;
    MUIM_Draw:                 // ################# DRAW EVENT #########################
    begin
      //writeln(' DRAW');
      if PMUIP_Draw(msg)^.Flags and MADF_DRAWOBJECT <> 0 then
       Exit;
      
      rp := nil;
      ri := MUIRenderInfo(Obj);
      if Assigned(ri) then
        rp := ri^.mri_RastPort;
      if Assigned(rp) then
      begin
        MUIB := TMUIObject(INST_DATA(cl, Pointer(obj))^);
        clip := MUI_AddClipping(ri, Obj_Left(obj), Obj_top(Obj),
            Obj_Width(Obj), Obj_Height(Obj));                
        try
          if Assigned(MUIB) then
          begin
            //writeln('-->Draw ', muib.classname, ' ', HexStr(MUIB.FMUICanvas));
            if MUIB.MUIDrawing then
              Result := DoSuperMethodA(cl, obj, msg); 
            WithScrollbars := Assigned(MUIB.VScroll) and Assigned(MUIB.HScroll);  
            Buffered := (MUIB.FChilds.Count = 0) or ((MUIB.FChilds.Count = 2) and WithScrollbars);             
            if Buffered then
            begin
              PaintW := Obj_Width(Obj);
              PaintH := Obj_Height(Obj);
              if WithScrollbars then
              begin                              
                if MUIB.VScroll.Visible then
                  PaintW := PaintW - MUIB.VScroll.Width - 1;
                If MUIB.HScroll.Visible then
                  PaintH := PaintH - MUIB.HScroll.Height - 1;                  
                //writeln('-->Draw ', muib.classname, ' ', HexStr(MUIB.FMUICanvas)); 
              end;  
              MUIB.FMUICanvas.DrawRect := Rect(0, 0, PaintW, PaintH);
              MUIB.FMUICanvas.RastPort := CreateRastPort;
              MUIB.FMUICanvas.RastPort^.Layer := nil;
              MUIB.FMUICanvas.RastPort^.Bitmap := AllocBitMap(PaintW, PaintH, rp^.Bitmap^.Depth, BMF_CLEAR, rp^.Bitmap);
              ClipBlit(rp, Obj_Left(Obj), Obj_Top(Obj), MUIB.FMUICanvas.RastPort, 0, 0, PaintW, PaintH, $00C0);
            end else
            begin
              MUIB.FMUICanvas.RastPort := rp;
              MUIB.FMUICanvas.DrawRect :=
                  Rect(Obj_Left(Obj), Obj_Top(Obj), Obj_Right(Obj), Obj_Bottom(Obj));
            end;
            MUIB.FMUICanvas.Offset.X := 0;
            MUIB.FMUICanvas.Offset.Y := 0;
            MUIB.FMUICanvas.Position.X := 0;
            MUIB.FMUICanvas.Position.Y := 0;
            MUIB.FMUICanvas.RenderInfo := ri;
            MUIB.FMUICanvas.DeInitCanvas;
            MUIB.FMUICanvas.InitCanvas;
            //writeln('-->Draw ', MUIB.FMUICanvas.DrawRect.Top, ', ', MUIB.FMUICanvas.DrawRect.Bottom);
            MUIB.DoRedraw;

            if Assigned(MUIB.FOnDraw) then
            begin
              MUIB.FOnDraw(MUIB);
            end;  
            MUIB.FMUICanvas.DeInitCanvas;
            if Buffered then
            begin
              ClipBlit(MUIB.FMUICanvas.RastPort, 0,0, rp, Obj_Left(Obj), Obj_Top(Obj), PaintW, PaintH, $00C0);
              FreeBitmap(MUIB.FMUICanvas.RastPort^.Bitmap);
              FreeRastPort(MUIB.FMUICanvas.RastPort);
              MUIB.FMUICanvas.RastPort := nil;
            end;
            //writeln('<--Draw ', muib.classname);   
          end;
        finally
          MUI_RemoveClipRegion(ri, clip);
          MUIB.FMUICanvas.RastPort := nil;
        end;
      end;
      Result := 0;
    end;
    MUIM_HANDLEEVENT: begin
      //writeln(' HandleEvent');
      MUIB := TMUIObject(INST_DATA(cl, Pointer(obj))^);
      if Assigned(MUIB) then
      begin        
        HEMsg := Pointer(Msg);
        iMsg := HeMsg^.imsg;
        ri := MUIRenderInfo(Obj);
        if Assigned(ri) then
          Win := ri^.mri_Window;
        if Assigned(Win) then
        begin
          // Activate the RMBTrap if no menu -> we can use the Right mousekey
          // get parent window
          MUIParent := MUIB.GetParentWindow;
          MUIWin := nil;
          if MUIParent is TMuiWindow then
            MUIWin := MUIParent as TMuiWindow; 
          if Assigned(MUIWin) then
          begin
            // if Window has a MainMenu do not catch Right MB
            if MUIWin.HasMenu then
              Win^.Flags := Win^.Flags and not WFLG_RMBTrap
            else
              Win^.Flags := Win^.Flags or WFLG_RMBTrap;
          end;
        end;
        // save Keystate for Winapi.GetKeyState
        KeyState := IMsg^.Qualifier;
        // Eat this Event if it is inside our border
        // but not inside of any of my Childs
        EatEvent := OBJ_IsInObject(Imsg^.MouseX, Imsg^.MouseY, obj);
        for i := 0 to MUIB.FChilds.Count - 1 do
        begin
          if OBJ_IsInObject(Imsg^.MouseX, Imsg^.MouseY, TMUIObject(MUIB.FCHilds[i]).Obj) then
            EatEvent := False;  // the mouse is inside of one of my Childs! so do not eat it
        end;        
        if true then
        begin
          //writeln(MUIB.classname,' obj Event ', Imsg^.MouseX, ' ', Imsg^.MouseY);
          // Calc relative mouse coordinates for this Item
          RelX := Imsg^.MouseX - obj_Left(obj);
          RelY := Imsg^.MouseY - obj_Top(obj);
          // Check the EventClass
          case IMsg^.IClass of
            IDCMP_MOUSEMOVE: begin     // Mouse MOVE  ############################################
              LCLSendMouseMoveMsg(MUIB.PasObject, RelX, RelY, []);
              if MUIB.LastClick > 0 then
                if MUIB.NumMoves > 0 then
                  Dec(MUIB.NumMoves)
                else
                  MUIB.LastClick := -1;
            end;
            IDCMP_MOUSEBUTTONS: begin  // MOUSE BUTTON ###########################################
              
              // Check the Mouse Status  
              case iMsg^.Code of
                SELECTDOWN: begin  // Left Button down
                  if not EatEvent then
                    Exit;  // Mouse buttons only send if the mouse is inside the Widget
                  // Check if we have to switch the Focus to the clicked one
                  if MUIWin.FocusedControl <> MUIB then
                  begin
                    if Assigned(MUIWin.FocusedControl) then
                      LCLSendKillFocusMsg(MUIWin.FocusedControl.PasObject); // send 'Unfocus' message
                    LCLSendSetFocusMsg(MUIB.PasObject);                     // send 'Focus' message
                  end;
                  MUIWin.FocusedControl := MUIB;
                  LCLSendMouseDownMsg(MUIB.PasObject, RelX, RelY, mbLeft, []);
                  // Check if it is an Double click < 250 ms and less than 3 move events between
                  CurTime := GetMsCount;
                  if (CurTime - MUIB.LastClick <= 250) and (MUIB.NumMoves > 0) then
                  begin  
                    LCLSendMouseMultiClickMsg(MUIB.PasObject, RelX, RelY, mbLeft, 2, []);  // its a double click
                    MUIB.LastClick := -1;                
                  end else
                  begin
                    MUIB.NumMoves := 3;            // first click, maybe later as Double Click ;)
                    MUIB.LastClick := CurTime;  
                  end;
                end;
                // Left Mouse UP
                SELECTUP: LCLSendMouseUpMsg(MUIB.PasObject, RelX, RelY, mbLeft, []);
                // Middle Mouse Down
                MIDDLEDOWN: begin
                    if not EatEvent then
                      Exit;  // Mouse buttons only send if the mouse is inside the Widget
                    LCLSendMouseDownMsg(MUIB.PasObject, RelX, RelY, mbMiddle, []);
                  end;  
                // Middle Mouse Up  
                MIDDLEUP: LCLSendMouseUpMsg(MUIB.PasObject, RelX, RelY, mbMiddle, []);
                // Right Mouse Down;
                MENUDOWN: begin
                    if not EatEvent then
                      Exit;  // Mouse buttons only send if the mouse is inside the Widget
                    LCLSendMouseDownMsg(MUIB.PasObject, RelX, RelY, mbRight, []);
                  end;
                // Right Mouse Up
                MENUUP: LCLSendMouseUpMsg(MUIB.PasObject, RelX, RelY, mbRight, []);
              end;
            end;
            IDCMP_RAWKEY: begin              // KEYS ###########################################
              // Mouse scroll wheel produce a up/down message
              if (iMsg^.Code = $7A) or (iMsg^.Code = $7B) then
              begin
                if not EatEvent then
                  Exit;
                RelX := Imsg^.MouseX - obj_Left(obj);
                RelY := Imsg^.MouseY - obj_Top(obj); 
                // Mouse wheel with Value 120 (from the other interfaces)
                if iMsg^.Code = $7B then
                  LCLSendMouseWheelMsg(MUIB.PasObject, RelX, RelY, -120, [])
                else
                  LCLSendMouseWheelMsg(MUIB.PasObject, RelX, RelY, +120, [])
              end else
              begin
                // Get the Keyboard Focus (see Mouse Buttons Left Down)
                if Assigned(MUIWin) then
                  if Assigned(MUIWin.FocusedControl) then
                    MUIB := MUIWin.FocusedControl;                    
                // Keyboard events always get eaten -> focussed Control    
                EatEvent := True;
                // Extrace some data and let MapRawKey do the job
                KeyUp := (IMsg^.Code and IECODE_UP_PREFIX) <> 0;
                IMsg^.Code := IMsg^.Code and not IECODE_UP_PREFIX;
                ie.ie_Class := IECLASS_RAWKEY;
                ie.ie_SubClass := 0;
                ie.ie_Code := IMsg^.Code;
                ie.ie_Qualifier := IMsg^.Qualifier and (not IEQUALIFIER_CONTROL);
                ie.ie_NextEvent := nil;
                Buff[0] := #0;
                Ret := MapRawKey(@ie, @Buff[0], 1, nil);
                //writeln('Key: ', MUIB.PasObject.Classname, ' got Key "',Buff[0],'" #', KeyData, ' Ret: ', Ret);
                Key := Buff[0];
                // Shiftstate mainly for ALT
                // TODO: still not working!!!! ssALT is never set
                KeyData := KeyboardShiftState(IMsg^.Qualifier);
                // save KeyState for Winapi.GetKeyState
                KeyState := IMsg^.Qualifier;
                IsSysKey := KeyData <> 0;
                //writeln(' send key: $', IntToHex(KeyData,8) );
                if (Ret = 1) and (IMsg^.Qualifier and IEQUALIFIER_LALT = 0) then
                begin 
                  CharCode := RawKeyToKeycode(IMsg^.Code);
                  if CharCode = 0 then
                    CharCode := Ord(uppercase(Key)[1]);
                  if KeyUp then
                  begin
                    LCLSendKeyUpEvent(MUIB.PasObject, CharCode, KeyData, True, False);
                  end else
                  begin  
                    //writeln('Down ', Char(CharCode), ' ', Charcode, ' ', Ord(''''));
                    LCLSendKeyDownEvent(MUIB.PasObject, CharCode, KeyData, True, False);  
                    if (IMsg^.Qualifier and IEQUALIFIER_CONTROL = 0) then
                    begin
                      CharCode := Ord(Key); 
                      //writeln('Press ', Char(CharCode), '  ', Key,' ' ,charcode);
                      LCLSendCharEvent(MUIB.PasObject, CharCode, KeyData, True, False, True);
                    end;
                  end;  
                end else
                begin
                  CharCode := RawKeyToKeycode(IMsg^.Code);
                  if KeyUp then
                    LCLSendKeyUpEvent(MUIB.PasObject, CharCode, KeyData, True, IsSysKey)
                  else
                    LCLSendKeyDownEvent(MUIB.PasObject, CharCode, KeyData, True, IsSysKey);                  
                end;
              end;
            end;
            else
            begin
              //writeln('IDCMP: ', HexStr(Pointer(IMsg^.IClass)));
            end;
          end;
          Result := 0;
          if EatEvent then
            Result := MUI_EventHandlerRC_Eat;
        end else
        begin
          Result := 0;
        end;
      end;
    end
    else
    begin
      //writeln(Dos.GetMsCount, ' unknown messageID $', HexStr(Pointer(Msg^.MethodID)));
      Result := DoSuperMethodA(cl, obj, msg);
    end;
  end;
end;

procedure DestroyClasses;
begin
  if Assigned(LCLGroupClass) then
    FreeClass(LCLGroupClass);
  if Assigned(GroupSuperClass) then
    MUI_FreeClass(GroupSuperClass);
end;

procedure CreateClasses;
begin
  GroupSuperClass := MUI_GetClass(MUIC_Group);
  if not Assigned(GroupSuperClass) then
  begin
    writeln('Superclass for the new class not found.');
    halt(5);
  end;
  LCLGroupClass := MakeClass(nil, nil, GroupSuperClass, SizeOf(Pointer), 0);
  if not Assigned(LCLGroupClass) then
  begin
    writeln('Cannot make class.');
    DestroyClasses;
    halt(5);
  end;
  LCLGroupClass^.cl_Dispatcher.h_Entry := IPTR(@Dispatcher);
  LCLGroupClass^.cl_Dispatcher.h_SubEntry := 0;
  LCLGroupClass^.cl_Dispatcher.h_Data := nil;
end;



initialization
  CreateClasses;

finalization
  MUIApp.Free;
  DestroyClasses;
end.

