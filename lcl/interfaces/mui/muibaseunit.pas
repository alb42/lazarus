unit MUIBaseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, dos, SysUtils, Controls, Contnrs, Types,
  {$ifdef HASAMIGA}
  Exec, AmigaDos, agraphics, Intuition, Utility,Mui, inputevent, KeyMap,
  {$endif}
  Forms, LCLMessageGlue, lcltype, interfacebase, muidrawing;

type
  TEventFunc = procedure(Hook: PHook; Obj: PObject_; Msg: Pointer); cdecl;

  { TMUIObject }

  TMUIObject = class(TObject)
  private
    //EventHooks
    ButtonUp: THook;
    ButtonDown: THook;
    //Position
    FLeft, FTop, FWidth, FHeight: longint;
    //Parent
    FParent: TMUIObject;
    // AWinControl lcl-Object
    FPasObject: TControl;
    FOnDraw: TNotifyEvent;
    FMuiCanvas: TMUICanvas;
    function GetEnabled: boolean;
    procedure SetEnabled(AValue: boolean);
  protected
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

    function GetWidth(): integer; virtual;
    procedure InstallHooks; virtual;
    procedure DoReDraw(); virtual;
  public
    EHNode: PMUI_EventHandlerNode;
    FObjects: TObjectList;
    FObject: pObject_;
    BlockRedraw: boolean;
    MUIDrawing: Boolean;
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

    property Parent: TMUIObject read FParent write SetParent;
    property Left: longint read FLeft write SetLeft;
    property Top: longint read FTop write SetTop;
    property Width: longint read GetWidth write SetWidth;
    property Height: longint read FHeight write SetHeight;
    property Obj: pObject_ read FObject write FObject;
    property GrpObj: pObject_ read FGrpObj;
    property PasObject: TControl read FPasObject write FPasObject;
    property Visible: boolean read GetVisible write SetVisible;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property MUICanvas: TMUICanvas read FMUICanvas;

    property OnDraw: TNotifyEvent read FOnDraw write FOnDraw;
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
    function GetSelected: boolean; virtual;
    procedure SetCaption(const AValue: string); virtual;
    procedure SetDragable(const AValue: boolean); virtual;
    procedure SetDropable(const AValue: boolean); virtual;
    procedure SetEnabled(const AValue: boolean); virtual;
    procedure SetHint(const AValue: string); virtual;
    procedure SetSelected(const AValue: boolean); virtual;
  public
    property Caption: string read GetCaption write SetCaption;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property Dragable: boolean read GetDragable write SetDragable;
    property Dropable: boolean read GetDropable write SetDropable;
    property Selected: boolean read GetSelected write SetSelected;
    property Hint: string read GetHint write SetHint;
    property Checked: longbool read GetChecked write SetChecked;
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
    function NewInput(Signals: PLongword): longword;
    procedure ProcessMessages;
    procedure WaitMessages;
    function CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): THandle;
    function DestroyTimer(TimerHandle: THandle): boolean;
    property MainWin: pObject_ read FMainWin;
    property Terminated: boolean read FTerminated write FTerminated;
    property Iconified: boolean read GetIconified write SetIconified;
  end;

procedure BtnDownFunc(Hook: PHook; Obj: PObject_; Msg: Pointer); cdecl;
procedure BtnUpFunc(Hook: PHook; Obj: PObject_; Msg: Pointer); cdecl;

var
  MUIApp: TMuiApplication;
  LCLGroupClass: PIClass;

implementation

uses
  tagsarray, longarray, muiformsunit;

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

procedure TMUIObject.SetParent(const AValue: TMUIObject);
begin
  //Writeln(self.classname, 'Set Parent: ', HexStr(AValue));
  if FParent = AValue then
  begin
    //writeln('same');
    Exit;
  end;
  if Assigned(FParent) then
  begin
    FParent.RemoveChild(Self);
    FParent.FObjects.Remove(Self);
    FParent := nil;
  end;
  if Assigned(AValue) then
  begin
    //write('  New: ', AValue.Classname, ' assigned: ', Assigned(AValue.FObjects));
    AValue.FObjects.Add(Self);
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
  //writeln('setVis');
  SetAttribute([longint(MUIA_ShowMe), longint(AValue), TAG_END]);
end;

procedure TMUIObject.SetLeft(ALeft: integer);
begin
  FLeft := ALeft;
  if Assigned(Parent) then
    Parent.ReDraw;
end;

procedure TMUIObject.SetTop(ATop: integer);
begin
  FTop := ATop;
  if Assigned(Parent) then
    Parent.ReDraw;
end;

procedure TMUIObject.SetWidth(AWidth: integer);
begin
  FWidth := AWidth;
  if Assigned(Parent) then
    Parent.ReDraw;
end;

procedure TMUIObject.SetHeight(AHeight: integer);
begin
  FHeight := AHeight;
  if Assigned(Parent) then
    Parent.ReDraw;
end;

function TMUIObject.GetWidth(): integer;
begin
  Result := FWidth;
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
    LCLSendEraseBackgroundMsg(TWinControl(PasObject), PS^.hdc);
    LCLSendPaintMsg(TControl(PasObject), PS^.hdc, PS);
    Dispose(PS);
  end;
  FMUICanvas.DeInitCanvas;
  for i := 0 to FObjects.Count - 1 do
  begin
    TMuiObject(FObjects[i]).DoMuiDraw;
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
  Result.Right:= Width;
  Result.Bottom := Height;
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
end;

constructor TMUIObject.Create(ObjType: longint; const Params: array of const);
begin
  inherited Create;
  MUIDrawing := False;
  FMUICanvas := TMUICanvas.Create;
  BlockRedraw := False;
  FObjects := TObjectList.Create(False);
  FParent := nil;
  //writeln(self.classname, 'create obj ', ObjType);
  FObject := MUI_MakeObject(ObjType, Params);
  //writeln('create obj: ',self.classname,' addr:', inttoHex(Cardinal(FObject),8));
  InstallHooks;
end;

constructor TMUIObject.Create(AClassName: PChar; Tags: PTagItem);
begin
  inherited Create;
  MUIDrawing := False;
  FMUICanvas := TMUICanvas.Create;
  BlockRedraw := False;
  FObjects := TObjectList.Create(False);
  FParent := nil;
  //writeln(self.classname, 'create class ', classname);
  FObject := MUI_NewObjectA(AClassName, Tags);
  InstallHooks;
  //writeln('create obj: ',self.classname,' addr:', inttoHex(Cardinal(FObject),8));
end;

constructor TMUIObject.Create(AClassType: PIClass; Tags: PTagItem);
begin
  inherited Create;
  MUIDrawing := False;
  FMUICanvas := TMUICanvas.Create;
  BlockRedraw := False;
  FObjects := TObjectList.Create(False);
  FParent := nil;
  FObject := NewObjectA(AClassType, nil, Tags);
  //writeln(self.classname, 'create type');
  if Assigned(FObject) then
    Pointer(INST_DATA(AClassType, Pointer(FObject))^) := Self;
  InstallHooks;
end;

destructor TMUIObject.Destroy;
begin
  BlockRedraw := True;
  //writeln(self.classname, '--> muiobject destroy');
  SetParent(nil);
  MUI_DisposeObject(FObject);
  FObjects.Free;
  FMUICanvas.Free;
  inherited Destroy;
  //writeln(self.classname, '<-- muiobject destroy');
end;

procedure TMUIObject.SetOwnSize;
var
  i: longint;
begin
  //writeln(self.classname, '-->setownsize');
  if not Assigned(FObject) then
    Exit;
  //writeln(self.classname,' setsize ', FLeft, ', ', FTop, ' - ', FWidth, ', ', FHeight,' count: ', FObjects.Count, ' obj ', HexStr(FObject));
  MUI_Layout(FObject, FLeft, FTop, FWidth, FHeight, 0);
  //writeln(self.classname, '  setsize done');
  for i := 0 to FObjects.Count - 1 do
  begin
    //writeln(self.classname, '  Child ', i);
    TMuiObject(FObjects.Items[i]).SetOwnSize;
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
  SetAttribute([LongInt(MUIA_Application_Iconified), IPTR(AValue), IPTR(TAG_END)]);
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
    SetAttribute([longint(MUIA_Application_Window), child.obj, TAG_END]);
    CallHook(PHook(OCLASS(FMainWin)), FMainWin,
      [longint(MUIM_Notify), longint(MUIA_Window_CloseRequest), True,
      longword(FObject), 2, longint(MUIM_Application_ReturnID),
      longint(MUIV_Application_ReturnID_Quit)]);
  end;
end;

procedure TMuiApplication.RemoveChild(Child: TMUIObject);
begin
  inherited RemoveChild(Child);
  if Child.obj = FMainWin then
  begin
    FMainWin := nil;
    SetAttribute([longint(MUIA_Application_Window), nil, TAG_END]);
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
end;

destructor TMuiApplication.Destroy;
begin
  FTimers.Free;
  inherited Destroy;
end;

function TMuiApplication.NewInput(Signals: PLongword): longword;
begin
  Result := DoMethod([IPTR(Signals)]);
end;

procedure TMuiApplication.ProcessMessages;
begin
  CheckTimer;
  if integer(DoMethod([IPTR(MUIM_Application_NewInput), IPTR(@FSignals)])) =
    MUIV_Application_ReturnID_Quit then
  begin
    Application.Terminate;
    Exit;
  end;
  CheckTimer;
end;

procedure TMuiApplication.WaitMessages;
begin
  CheckTimer;
  if DoMethod([IPTR(MUIM_Application_NewInput), IPTR(@FSignals)]) =
    MUIV_Application_ReturnID_Quit then
  begin
    Application.Terminate;
    Exit;
  end;
  if (FSignals <> 0) then
  begin
    FSignals := CheckSignal(FSignals or SIGBREAKF_CTRL_C);
    if FTerminated or ((FSignals and SIGBREAKF_CTRL_C) <> 0) then
    begin
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

{ TMuiArea }

function TMuiArea.GetChecked: longbool;
begin
  Result := longbool(GetAttribute(MUIA_Selected));
end;

procedure TMuiArea.SetChecked(const AValue: longbool);
begin
  SetAttribute([longint(MUIA_Selected), longint(AValue), TAG_END]);
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

function TMuiArea.GetSelected: boolean;
begin
  Result := not boolean(GetAttribute(MUIA_Selected));
end;

procedure TMuiArea.SetCaption(const AValue: string);
begin
  SetAttribute([longint(MUIA_Text_Contents), PChar(AValue), TAG_END]);
end;

procedure TMuiArea.SetDragable(const AValue: boolean);
begin
  SetAttribute([longint(MUIA_Draggable), AValue, TAG_END]);
end;

procedure TMuiArea.SetDropable(const AValue: boolean);
begin
  SetAttribute([longint(MUIA_Dropable), AValue, TAG_END]);
end;

procedure TMuiArea.SetEnabled(const AValue: boolean);
var
  NValue: longbool;
begin
  NValue := not AValue;
  SetAttribute([longint(MUIA_Disabled), longint(NValue), TAG_END]);
end;

procedure TMuiArea.SetHint(const AValue: string);
begin
  SetAttribute([longint(MUIA_ShortHelp), PChar(AValue), TAG_END]);
end;

procedure TMuiArea.SetSelected(const AValue: boolean);
begin
  SetAttribute([longint(MUIA_Selected), AValue, TAG_END]);
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

  {TehNode = record
    ehn_Node: TMinNode;
    ehn_Reserved: Byte;
    ehn_Priority: Byte;
    ehn_Flags: Word;
    ehn_Object: PObject_;
    ehn_Class: PIClass;
    ehn_Events: ULONG;
  end;}

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
  p: TMUIObject;
  HIMsg: PMUIP_HandleInput;
  HEMsg: PMUIP_HandleEvent;
  iMsg: PIntuiMessage;
  AE: TMUIP_Window_AddEventHandler;
  ehN: ^TehNode;
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
begin
  //write('Enter Dispatcher with: ');
  case Msg^.MethodID of
    MUIM_SETUP: begin
      Result := DoSuperMethodA(cl, obj, msg);
      MUIB := TMUIObject(INST_DATA(cl, Pointer(obj))^);
      if Assigned(MUIB) then
      begin
        New(ehN);
        FillChar(ehN^, SizeOf(ehN^), 0);
        P := MUIB;
        ehN^.ehn_Priority := -100;
        repeat
          Inc(ehN^.ehn_Priority);
          p := p.Parent;
        until P = nil;

        ehN^.ehn_Flags := 0;
        ehN^.ehn_Object := obj;
        ehN^.ehn_Class := cl;
        ehN^.ehn_Events := IDCMP_MOUSEBUTTONS or IDCMP_MOUSEMOVE or IDCMP_RAWKEY;
        AE.MethodID := MUIM_Window_AddEventHandler;
        AE.ehNode := Pointer(ehN);
        winObj := OBJ_win(obj);
        ri := MUIRenderInfo(Obj);
        WinObj := ri^.mri_WindowObject;
        //DoMethodA(OBJ_win(obj), @AE);
        DoMethod(WinObj,MUIM_Window_AddEventHandler,[IPTR(ehN)]);
        //ri := MUIRenderInfo(Obj); writeln(MUIB.classname, ' addeventhandler ', HexStr(winObj), ' parent: ', Muib.Parent.Classname, ' ', HexStr(MUIB.Parent.Obj),' self ', HexStr(MUIB.Obj));
      end;
      //MUI_RequestIDCMP(Obj, IDCMP_MOUSEBUTTONS);
    end;
    MUIM_CLEANUP: begin
      Result := DoSuperMethodA(cl, obj, msg);
      MUIB := TMUIObject(INST_DATA(cl, Pointer(obj))^);
      if Assigned(MUIB) then
      begin
        DoMethod(OBJ_win(obj),MUIM_Window_RemEventHandler,[IPTR(MUIB.EHNode)]);
        Dispose(MUIB.EHNode);
      end;
      //MUI_RejectIDCMP(Obj, IDCMP_MOUSEBUTTONS);
    end;
    MUIM_Draw:
    begin
      //writeln('DRAW');
      //if PMUIP_Draw(msg)^.Flags and MADF_DRAWOBJECT = 0 then
      //  Exit;
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
            if MUIB.MUIDrawing then
              Result := DoSuperMethodA(cl, obj, msg);
            MUIB.FMUICanvas.RastPort := rp;
            MUIB.FMUICanvas.DrawRect :=
                Rect(Obj_Left(Obj), Obj_Top(Obj), Obj_Right(Obj), Obj_Bottom(Obj));
            MUIB.FMUICanvas.Offset.X := 0;
            MUIB.FMUICanvas.Offset.Y := 0;
            MUIB.FMUICanvas.Position.X := 0;
            MUIB.FMUICanvas.Position.Y := 0;
            MUIB.FMUICanvas.RenderInfo := ri;
            MUIB.FMUICanvas.DeInitCanvas;
            MUIB.FMUICanvas.InitCanvas;
            //writeln('-->Draw ', MUIB.FMUICanvas.DrawRect.Top, ', ', MUIB.FMUICanvas.DrawRect.Bottom);
            if not MUIB.MUIDrawing then
            begin
              SetAPen(rp, ri^.mri_Pens[MPEN_BACKGROUND]);
              RectFill(rp, MUIB.FMUICanvas.DrawRect.Left, MUIB.FMUICanvas.DrawRect.Top,
                MUIB.FMUICanvas.DrawRect.Right, MUIB.FMUICanvas.DrawRect.Bottom);
            end;
            MUIB.DoRedraw;
            if Assigned(MUIB.FOnDraw) then
            begin
              MUIB.FOnDraw(MUIB);
            end;
            MUIB.FMUICanvas.DeInitCanvas;
            //writeln('<--Draw');
          end;
        finally
          MUI_RemoveClipRegion(ri, clip);
          MUIB.FMUICanvas.RastPort := nil;
        end;
      end;
      Result := 0;
    end;
    MUIM_HANDLEEVENT: begin
      MUIB := TMUIObject(INST_DATA(cl, Pointer(obj))^);
      if Assigned(MUIB) then
      begin
        HEMsg := Pointer(Msg);
        iMsg := HeMsg^.imsg;
        ri := MUIRenderInfo(Obj);
        if Assigned(ri) then
        begin
          Win := ri^.mri_Window;
        end;
        if Assigned(Win) then
        begin
          // why this is needed?
          //IMsg^.MouseX := IMsg^.MouseX - 1;
          //IMsg^.MouseY := IMsg^.MouseY - 6;
        end;
        if OBJ_IsInObject(Imsg^.MouseX, Imsg^.MouseY, obj) then
        begin
          RelX := Imsg^.MouseX - obj_Left(obj);
          RelY := Imsg^.MouseY - obj_Top(obj);
          case IMsg^.IClass of
            IDCMP_MOUSEMOVE: begin
              LCLSendMouseMoveMsg(MUIB.PasObject, RelX, RelY, []);
            end;
            IDCMP_MOUSEBUTTONS: begin
              case iMsg^.Code of
                SELECTDOWN: LCLSendMouseDownMsg(MUIB.PasObject, RelX, RelY, mbLeft, []);
                SELECTUP: LCLSendMouseUpMsg(MUIB.PasObject, RelX, RelY, mbLeft, []);
                MIDDLEDOWN: LCLSendMouseDownMsg(MUIB.PasObject, RelX, RelY, mbMiddle, []);
                MIDDLEUP: LCLSendMouseUpMsg(MUIB.PasObject, RelX, RelY, mbMiddle, []);
                MENUDOWN: LCLSendMouseDownMsg(MUIB.PasObject, RelX, RelY, mbRight, []);
                MENUUP: LCLSendMouseUpMsg(MUIB.PasObject, RelX, RelY, mbRight, []);
              end;
            end;
            IDCMP_RAWKEY: begin
              if (iMsg^.Code = $7A) or (iMsg^.Code = $7B) then
              begin
                if iMsg^.Code = $7A then
                  LCLSendMouseWheelMsg(MUIB.PasObject, RelX, RelY, -1, [])
                else
                  LCLSendMouseWheelMsg(MUIB.PasObject, RelX, RelY, +1, [])
              end else
              begin
                KeyUp := (IMsg^.Code and IECODE_UP_PREFIX) <> 0;
                IMsg^.Code := IMsg^.Code and not IECODE_UP_PREFIX;
                ie.ie_Class := IECLASS_RAWKEY;
                ie.ie_SubClass := 0;
                ie.ie_Code := IMsg^.Code;
                ie.ie_Qualifier := IMsg^.Qualifier;
                ie.ie_NextEvent := nil;
                Buff[0] := #0;
                Ret := MapRawKey(@ie, @Buff[0], 1, nil);
                if Ret = 0 then
                begin
                  KeyData := Ord(CharCode);
                  LCLSendKeyDownEvent(MUIB.PasObject, KeyData, KeyData, False, False);
                end else
                begin
                  KeyData := 0;
                  LCLSendKeyDownEvent(MUIB.PasObject, KeyData, IMsg^.Code, False, True);
                end;
              end;
            end;
            else
            begin
              writeln('IDCMP: ', HexStr(Pointer(IMsg^.IClass)));
            end;
          end;
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

