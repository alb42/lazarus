unit MUIBaseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Contnrs, Exec, AmigaDos, Intuition, Utility,
  Mui, Forms, LCLMessageGlue;

type
  TEventFunc = procedure (Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;
  { TMUIObject }

  TMUIObject = class(TObject)
  private
    //EventHooks
    ButtonUp: THook;
    ButtonDown: THook;
    //Position
    FLeft, FTop, FWidth, FHeight: LongInt;
    //Parent
    FParent: TMUIObject;
    // AWinControl lcl-Object
    FPasObject: TControl;
    procedure SetParent(const AValue: TMUIObject);

  protected
    LayoutHook: THook;
    FObjects: TObjectList;
    FGrpObj: pObject_;
    procedure SetAttribute(const Tags : Array Of Const);
    function GetAttribute(tag: LongWord): LongWord;
    procedure SetAttObj(obje: pObject_; const Tags : Array Of Const);
    function GetAttObj(obje: pObject_; tag: LongWord): LongWord;
      // DoMethod(Params = [MethodID, Parameter for Method ...])
    function DoMethodObj(Obje: pObject_; const Params : Array Of Const): LongInt;
    function DoMethod(const Params : Array Of IPTR): LongInt;
      //
    procedure AddChild(Child: TMUIObject); virtual;
    procedure RemoveChild(Child: TMUIObject); virtual;
    procedure SetVisible(const AValue: Boolean); virtual;
    function GetVisible: Boolean; virtual;
  public
    FObject: pObject_;
    constructor Create(ObjType : LongInt; const Params : Array Of Const); overload; reintroduce; virtual;
    constructor Create(AClassName : PChar; Tags: PTagItem); overload; reintroduce; virtual;
    destructor Destroy; override;
    //
    procedure SetOwnSize; virtual;
    procedure SetPos(ALeft, ATop: LongInt); virtual;
    procedure SetSize(AWidth, AHeight: LongInt); virtual;
    //
    property Parent: TMUIObject read FParent write SetParent;
    property Left: LongInt read FLeft write FLeft;
    property Top: LongInt read FTop write FTop;
    property Width: LongInt read FWidth write FWidth;
    property Height: LongInt read FHeight write FHeight;
    property Obj: pObject_ read FObject write FObject;
    property PasObject:TControl read FPasObject write FPasObject;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  { TMuiArea }

  TMuiArea = class(TMUIObject)
  private
    function GetCaption: string; virtual;
    function GetDragable: Boolean;
    function GetDropable: Boolean;
    function GetEnabled: Boolean;
    function GetHint: string;
    function GetSelected: Boolean;
    procedure SetCaption(const AValue: string); virtual;
    procedure SetDragable(const AValue: Boolean);
    procedure SetDropable(const AValue: Boolean);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetHint(const AValue: string);
    procedure SetSelected(const AValue: Boolean);
  protected
  public
    property Caption: string read GetCaption write SetCaption;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Dragable: Boolean read GetDragable write SetDragable;
    property Dropable: Boolean read GetDropable write SetDropable;
    property Selected: Boolean read GetSelected write SetSelected;
    property Hint: string read GetHint write SetHint;
  end;

  { TMuiApplication }

  TMuiApplication = class(TMUIObject)
  private
    FTerminated: Boolean;
    FSignals: LongWord;
    FMainWin: pObject_;
    function GetIconified: Boolean;
    procedure SetIconified(const AValue: Boolean);
  protected
    procedure AddChild(Child: TMUIObject); override;
    procedure RemoveChild(Child: TMUIObject); override;
  public
    constructor Create(Tags: PTagItem); overload; reintroduce; virtual;
    function NewInput(Signals: PLongword):LongWord;
    procedure ProcessMessages;
    procedure WaitMessages;
    property Terminated: Boolean read FTerminated write FTerminated;
    property Iconified: Boolean read GetIconified write SetIconified;
  end;

procedure BtnDownFunc(Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;
procedure BtnUpFunc(Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;

var
  MUIApp: TMuiApplication;

implementation

uses
  tagsarray,longarray;


procedure BtnDownFunc(Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;
var
  MuiObject: TMuiObject;
begin
  //writeln('-->btndown');
  if TObject(Hook^.h_Data) is TMuiObject then
  begin
    MuiObject := TMuiObject(Hook^.h_Data);
    LCLSendMouseDownMsg(TControl(MuiObject.PasObject), 0,0, mbLeft, []);
  end;
  //writeln('<--btndown');
end;

procedure BtnUpFunc(Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;
var
  MuiObject: TMuiObject;
begin
  //writeln('-->btnup');
  if TObject(Hook^.h_Data) is TMuiObject then
  begin
    MuiObject := TMuiObject(Hook^.h_Data);
    LCLSendMouseUpMsg(TControl(MuiObject.PasObject), 0,0, mbLeft, []);
    LCLSendClickedMsg(TControl(MuiObject.PasObject));
  end;
  //writeln('<--btnup');
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

function TMUIObject.GetVisible: Boolean;
begin
  //writeln('getvis');
  Result := Boolean(GetAttribute(MUIA_ShowMe));
end;

procedure TMUIObject.SetVisible(const AValue: Boolean);
begin
  //writeln('setVis');
  SetAttribute([LongInt(MUIA_ShowMe), LongInt(AValue), TAG_END]);
end;

procedure TMUIObject.SetPos(ALeft, ATop: LongInt);
begin
  //writeln('setpos');
  FLeft := ALeft;
  FTop :=  ATop;
end;

procedure TMUIObject.SetSize(AWidth, AHeight: LongInt);
begin
  //writeln('setsize');
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TMUIObject.SetAttObj(obje: pObject_; const Tags : Array Of Const);
var
  TagList: TTagsList;
begin
  AddTags(TagList, Tags);
  SetAttrsA(obje, GetTagPtr(TagList));
end;

function TMUIObject.GetAttObj(obje: pObject_; tag: LongWord): LongWord;
var
  Res: LongWord;
begin
  GetAttr(tag, obje, @Res);
  Result := Res;
end;

function TMUIObject.DoMethodObj(Obje: pObject_;
  const Params: array of const): LongInt;
var
  Tags: TTagsList;  
begin
  AddTags(Tags, Params);
  Result := CallHookPkt(PHook(OCLASS(Obje)), Obje, GetTagPtr(Tags));
end;

procedure TMUIObject.SetAttribute(const Tags: array of const);
var
  TagList: TTagsList; 
begin
  AddTags(TagList, Tags);
  SetAttrsA(FObject, GetTagPtr(TagList));
end;

function TMUIObject.GetAttribute(tag: LongWord): LongWord;
var
  Res: LongWord;
begin
  GetAttr(tag, FObject, @Res);
  Result := Res;
end;

function TMUIObject.DoMethod(const Params : Array Of IPTR): LongInt;
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

constructor TMUIObject.Create(ObjType: LongInt; const Params: array of const);
begin
  inherited Create;
  FObjects := TObjectList.create(False);
  FParent := NIL;
  FObject := MUI_MakeObject(ObjType, Params);
  //
  ButtonUp.h_Entry := IPTR(@BtnUpFunc);
  ButtonUp.h_SubEntry := 0;//IPTR(@BtnUpFunc);
  ButtonUp.h_Data := Self;
  ButtonDown.h_Entry := IPTR(@BtnDownFunc);
  ButtonDown.h_SubEntry := 0;//IPTR(@BtnDownFunc);
  ButtonDown.h_Data := Self;
  //
  
  DoMethod([LongInt(MUIM_Notify),
    LongInt(MUIA_Pressed), LongInt(True),
    LongInt(MUIV_Notify_Self),
    2,
    LongInt(MUIM_CallHook), IPTR(@ButtonDown)
    ]);
  DoMethod([LongInt(MUIM_Notify),
    LongInt(MUIA_Pressed), LongInt(False),
    LongInt(MUIV_Notify_Self),
    2,
    LongInt(MUIM_CallHook), IPTR(@ButtonUp)
    ]);
   //writeln('create obj: ',self.classname,' addr:', inttoHex(Cardinal(FObject),8));  
end;

constructor TMUIObject.Create(AClassName: PChar; Tags: PTagItem);
begin
  inherited Create;
  FObjects := TObjectList.create(False);
  FParent := NIL;
  FObject := MUI_NewObjectA(AClassName, Tags);
  //writeln('create obj: ',self.classname,' addr:', inttoHex(Cardinal(FObject),8));
end;

destructor TMUIObject.Destroy;
begin
  //writeln(self.classname, '--> muiobject destroy');
  SetParent(nil);
  MUI_DisposeObject(FObject);
  FObjects.Free;
  inherited Destroy;
  //writeln(self.classname, '<-- muiobject destroy');
end;

procedure TMUIObject.SetOwnSize;
var
  i: LongInt;
begin
  if not Assigned(FObject) then
    Exit;
  //writeln(self.classname,' setsize ', FLeft, ', ', FTop, ' - ', FWidth, ', ', FHeight);  
  MUI_LayOut(FObject, FLeft, FTop, FWidth, FHeight, 0);
  for i := 0 to FObjects.Count - 1 do
  begin
    TMuiObject(FObjects.Items[i]).SetOwnSize;
  end;
end;

{ TMuiApplication }

function TMuiApplication.GetIconified: Boolean;
begin
  Result := Boolean(GetAttribute(MUIA_Application_Iconified));
end;

procedure TMuiApplication.SetIconified(const AValue: Boolean);
begin
  SetAttribute([MUIA_Application_Iconified, AValue, TAG_END]);
end;

procedure TMuiApplication.AddChild(Child: TMUIObject);
begin
  inherited AddChild(Child);
  if FMainWin = nil then
  begin
    FMainWin := Child.obj;
    SetAttribute([LongInt(MUIA_Application_Window), child.obj, TAG_END]);
    CallHook(PHook(OCLASS(FMainWin)), FMainWin,
      [LongInt(MUIM_Notify), LongInt(MUIA_Window_CloseRequest), True,
      LongWord(FObject), 2,
      LongInt(MUIM_Application_ReturnID), LongInt(MUIV_Application_ReturnID_Quit)
      ]);
  end;
end;

procedure TMuiApplication.RemoveChild(Child: TMUIObject);
begin
  inherited RemoveChild(Child);
  if Child.obj = FMainWin then
  Begin
    FMainWin := nil;
    SetAttribute([LongInt(MUIA_Application_Window), NIL, TAG_END]);
  end;
end;

constructor TMuiApplication.Create(Tags: PTagItem);
begin
  inherited Create(MUIC_Application, Tags);
  FSignals := 0;
end;

function TMuiApplication.NewInput(Signals: PLongword): LongWord;
begin
  Result := DoMethod([IPTR(Signals)]);
end;

procedure TMuiApplication.ProcessMessages;
begin
  if Integer(DoMethod([LongInt(MUIM_Application_NewInput), IPTR(@FSignals)])) = MUIV_Application_ReturnID_Quit then
  begin
    Application.Terminate;
    Exit;
  end;
end;

procedure TMuiApplication.WaitMessages;
begin
  if DoMethod([LongInt(MUIM_Application_NewInput), IPTR(@FSignals)]) = MUIV_Application_ReturnID_Quit then
  begin
    Application.Terminate;
    Exit;
  end;
  if (FSignals <> 0) then
  begin
    FSignals := Wait(FSignals or SIGBREAKF_CTRL_C);
    if FTerminated or ((FSignals and SIGBREAKF_CTRL_C) <> 0) then
    begin
      Application.Terminate;
      Exit;
    end;
  end;
end;

{ TMuiArea }

function TMuiArea.GetCaption: string;
var
  Pc: PChar;
begin
  Pc := PChar(GetAttribute(MUIA_Text_Contents));
  Result := string(Pc);
end;

function TMuiArea.GetDragable: Boolean;
begin
  Result := Boolean(GetAttribute(MUIA_Draggable));
end;

function TMuiArea.GetDropable: Boolean;
begin
  Result := Boolean(GetAttribute(MUIA_Dropable));
end;

function TMuiArea.GetEnabled: Boolean;
begin
  Result := not Boolean(GetAttribute(MUIA_Disabled));
end;

function TMuiArea.GetHint: string;
begin
  Result := string(PChar(GetAttribute(MUIA_ShortHelp)));
end;

function TMuiArea.GetSelected: Boolean;
begin
  Result := not Boolean(GetAttribute(MUIA_Selected));
end;

procedure TMuiArea.SetCaption(const AValue: string);
begin
  SetAttribute([LongInt(MUIA_Text_Contents), PChar(AValue), TAG_END]);
end;

procedure TMuiArea.SetDragable(const AValue: Boolean);
begin
  SetAttribute([LongInt(MUIA_Draggable), AValue, TAG_END]);
end;

procedure TMuiArea.SetDropable(const AValue: Boolean);
begin
  SetAttribute([LongInt(MUIA_Dropable), AValue, TAG_END]);
end;

procedure TMuiArea.SetEnabled(const AValue: Boolean);
var
  NValue: LongBool;
begin
  NValue := not AValue;
  SetAttribute([LongInt(MUIA_Disabled), LongInt(NValue), TAG_END]);
end;

procedure TMuiArea.SetHint(const AValue: string);
begin
  SetAttribute([LongInt(MUIA_ShortHelp), PChar(AValue), TAG_END])
end;

procedure TMuiArea.SetSelected(const AValue: Boolean);
begin
  SetAttribute([LongInt(MUIA_Selected), AValue, TAG_END]);
end;

initialization

finalization
  MUIApp.Free;
end.

