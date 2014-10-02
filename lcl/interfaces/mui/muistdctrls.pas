unit MuiStdCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Contnrs, Exec, AmigaDos, Intuition, Utility, Mui, Forms,
  tagsarray,
  MuiBaseUnit, StdCtrls, muistringsunit, LCLMessageGlue, LMessages;

  { TMuiButton }
type

  TMuiButton = class(TMuiArea)
  public
    constructor Create(const Params : Array Of Const); overload; reintroduce; virtual;
  end;

  { TMuiText }

  TMuiText = class(TMuiArea)
  public
    constructor Create(var Tags: TTagsList); overload; reintroduce; virtual;
  end;

  { TMuiCheckMark }

  TMuiCheckMark = class(TMuiArea)
  private
    CheckHook: THook;
    FullWidth: Integer;
    CheckLabel: TMuiText;
    function GetChecked: LongBool;
    procedure SetChecked(const AValue: LongBool);
  protected
    procedure SetParent(const AValue: TMUIObject); override;
    function GetCaption: string; override;
    procedure SetCaption(const AValue: string); override;  
    procedure SetLeft(ALeft: Integer); override;
    procedure SetTop(ATop: Integer); override;
    procedure SetWidth(AWidth: Integer); override;
    procedure SetHeight(AHeight: Integer); override;
    function GetWidth(): Integer; override;
    procedure SetVisible(const AValue: Boolean); override;
  public
    constructor Create(IsRadio: Boolean; const Params : Array Of Const); overload; reintroduce; virtual;
    destructor Destroy; override;
    property Checked: LongBool read GetChecked write SetChecked;
  end;

  { TMuiStringEdit }

  TMuiStringEdit = class(TMuiArea)
  private
    TextChanged: THook;
    function GetText: string;
    procedure SetText(const AValue: string);
  public
    constructor Create(const Params : Array Of Const); overload; reintroduce; virtual;
    property Text:string read GetText write SetText;
  end;

  { TMuiCycle }

  TMuiCycle = class(TMuiArea)
  private
    ActiveItemChanged: THook;
    FStrings: TMuiStrings;
    function GetActive: LongInt;
    procedure SetActive(const AValue: LongInt);
    procedure ChangedItems(Sender: TObject);
  public
    constructor Create(ACaption: PChar; AStrings: TStrings); overload; reintroduce; virtual;
    Destructor Destroy; override;
    //
    property Strings: TMuiStrings read FStrings write FStrings;
    property Active: LongInt read GetActive write SetActive;
  end;

  { TMuiTextEdit }

  TMuiTextEdit = class;

  { TFlowString }

  TFlowString = class(TStrings)
  private
    SL: TStringList;
  public
    FMuiObject: TMuiTextEdit;
    constructor Create;
    destructor Destroy; override;
    function GetCount: Integer; override;
    function Add(const S: String): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1: Integer; Index2: Integer); override;
    function Get(Index: Integer): string; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure Insert(Index: Integer; const S: String); override;
    procedure LoadFromFile(const FileName: String); override;
  end;

  TMuiTextEdit = class(TMuiObject)
  private
    FText: PChar;
    FStrings: TFlowString;
    FTextObj: pObject_;
  public
    constructor Create(AStrings: TStrings; var Tags: TTagsList); overload; reintroduce; virtual;
    Destructor Destroy; override;
    property Strings: TFlowString read FStrings write FStrings;
    property TextObj: pObject_ read FTextObj write FTextObj;
  end;

  { TFloatText }

  TFloatText = class(TMuiArea)
  public
    constructor Create(var Tags: TTagsList); overload; reintroduce; virtual;
  end;

  { TMuiListView }

  TMuiListView = class(TMuiArea)
  private
    ListChangeHook: THook;
    DoubleClickHook: THook;
    FFloatText: TFloatText;
    FStrings: TStringList;
    function GetActive: LongInt;
    procedure SetActive(const AValue: LongInt);
    procedure TextChanged(Sender: TObject);
  public
    constructor Create(AStrings:TStrings; var Tags: TTagsList); overload; reintroduce; virtual;
    destructor Destroy; override;
    property Strings: TStringList read FStrings;
    property Active: LongInt read GetActive write SetActive;
    property FloatText: TFloatText read FFloatText;
  end;

implementation

{ TFloatText }

constructor TFloatText.Create(var Tags: TTagsList);
begin
  inherited Create(MUIC_FloatText, GetTagPtr(Tags));
end;


procedure ListChangeFunc(Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;
var
  MuiObject: TMuiListView;
  Idx: Integer;
begin
  if TObject(Hook^.h_Data) is TMuiListView then
  begin
    MuiObject := TMuiListView(Hook^.h_Data);
    Idx := MuiObject.Active;
    // LCLSendMouseDownMsg(TControl(MuiObject.PasObject), 0,0, mbLeft, []);
    if Idx > 0 then
      LCLSendChangedMsg(TControl(MuiObject.PasObject), Idx);    
    //LCLSendMouseUpMsg(TControl(MuiObject.PasObject), 0,0, mbLeft, []);
    LCLSendClickedMsg(TControl(MuiObject.PasObject));
  end;
end;

procedure DoubleClickFunc(Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;
var
  MuiObject: TMuiListView;
  Idx: Integer;
begin
  if TObject(Hook^.h_Data) is TMuiListView then
  begin
    MuiObject := TMuiListView(Hook^.h_Data);
    Idx := MuiObject.Active;
    //LCLSendMouseDownMsg(TControl(MuiObject.PasObject), 0,0, mbLeft, []);
    //if Idx > 0 then
    //  LCLSendChangedMsg(TControl(MuiObject.PasObject), Idx);
    LCLSendMouseMultiClickMsg(TControl(MuiObject.PasObject), 0, 0, mbLeft, 2);
    //LCLSendMouseUpMsg(TControl(MuiObject.PasObject), 0,0, mbLeft, []);
    //LCLSendClickedMsg(TControl(MuiObject.PasObject));
  end;
end;


{ TMuiListView }

constructor TMuiListView.Create(AStrings:TStrings; var Tags: TTagsList);
var
  FText: PChar;
  MenuTags: TTagsList;
begin
  FStrings := TStringList.create;
  FStrings.Assign(AStrings);
  FText := FStrings.GetText;
  FFloatText := TFloatText.create(MenuTags);
  AddTags(Tags, [MUIA_Listview_List, FloatText.Obj ,TAG_DONE]);
  inherited Create(MUIC_ListView, GetTagPtr(Tags));
  FStrings.OnChange := @TextChanged;
  
  ListChangeHook.h_Entry := IPTR(@ListChangeFunc);
  ListChangeHook.h_SubEntry := 0;
  ListChangeHook.h_Data := Self;
  //
  DoubleClickHook.h_Entry := IPTR(@DoubleClickFunc);
  DoubleClickHook.h_SubEntry := 0;
  DoubleClickHook.h_Data := Self;
  
  DoMethod([LongInt(MUIM_Notify),
    IPTR(MUIA_List_Active), IPTR(MUIV_EveryTime),
    LongInt(MUIV_Notify_Self),
    2,
    LongInt(MUIM_CallHook), IPTR(@ListChangeHook)
    ]);
    
  DoMethod([LongInt(MUIM_Notify),
    IPTR(MUIA_ListView_DoubleClick), IPTR(True),
    LongInt(MUIV_Notify_Self),
    2,
    LongInt(MUIM_CallHook), IPTR(@DoubleClickHook)
    ]);  
end;

destructor TMuiListView.Destroy;
begin
  inherited Destroy;
  // Object ist automatically destroyed by Listview
  FFloatText.Obj := nil;
  FFloatText.Free;
end;

procedure TMuiListView.TextChanged(Sender: TObject);
var
  FText: PChar;
  TagList: TTagsList;
begin
  if Assigned(FStrings) then
  begin
    FText := FStrings.GetText;
    AddTags(TagList, [LongInt(MUIA_FloatText_Text), FText, TAG_END]);
    SetAttrsA(FloatText.Obj, GetTagPtr(TagList));
  end;  
end;

function TMuiListView.GetActive: LongInt;
var
  Res: LongInt;
begin
  Result := 0;
  GetAttr(LongInt(MUIA_List_Active), FloatText.Obj, @Res);
  if Res = MUIV_List_Active_Off then
    Result := 0
  else
  begin
    if (Res >= 0) and (Res < Strings.Count) then
      Result := Res
    else
    begin
      if (Res < 0) then
        Result := 0
      else
        Result := Strings.Count - 1;  
      SetActive(Result);  
    end;  
  end  
end;

procedure TMuiListView.SetActive(const AValue: LongInt);
var
  Res: LongInt;
  TagList: TTagsList;
begin
  if AValue = -1 then
    Res := MUIV_List_Active_Off
  else
    Res := AValue;
  AddTags(TagList, [LongInt(MUIA_List_Active), Res, TAG_END]);  
  SetAttrsA(FloatText.Obj, GetTagPtr(TagList));
end;

{ TMuiButton }

constructor TMuiButton.Create(const Params: array of const);
begin
  inherited Create(MUIO_Button, Params);
end;

{ TMuiText }

constructor TMuiText.Create(var Tags: TTagsList);
begin
  inherited Create(MUIC_Text, GetTagPtr(Tags));
end;

{ TMuiCheckMark }

procedure CheckFunc(Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;
var
  MuiObject: TMuiObject;
begin
  if TObject(Hook^.h_Data) is TMuiObject then
  begin
    MuiObject := TMuiObject(Hook^.h_Data);
    LCLSendChangedMsg(TControl(MuiObject.PasObject), 0);
  end;
end;

function TMuiCheckMark.GetChecked: LongBool;
begin
  Result := LongBool(GetAttribute(MUIA_Selected));
end;

procedure TMuiCheckMark.SetChecked(const AValue: LongBool);
begin
  SetAttribute([LongInt(MUIA_Selected), LongInt(AValue), TAG_END]);
end;

constructor TMuiCheckMark.Create(IsRadio: Boolean; const Params: array of const);
var
  TagList: TTagsList;
begin
  if isRadio then
    inherited Create(MUIO_Radio, Params)
  else
    inherited Create(MUIO_Checkmark, Params);
  CheckLabel := TMuiText.Create(TagList);
  
  CheckHook.h_Entry := IPTR(@CheckFunc);
  CheckHook.h_SubEntry := 0;
  CheckHook.h_Data := Self;
  
  DoMethod([LongInt(MUIM_Notify),
    LongInt(MUIA_Selected), IPTR(MUIV_EveryTime),
    LongInt(MUIV_Notify_Self),
    2,
    LongInt(MUIM_CallHook), IPTR(@CheckHook)
    ]);
end;

destructor TMuiCheckMark.Destroy;
begin
  CheckLabel.Free;
  CheckLabel := Nil;
  inherited;  
end;

function TMuiCheckMark.GetCaption: string;
begin
  Result := '';
  if Assigned(CheckLabel) then
    Result := CheckLabel.Caption;
end;

procedure TMuiCheckMark.SetCaption(const AValue: string);
begin
  if Assigned(CheckLabel) then
    CheckLabel.Caption := AValue;
end;

procedure TMuiCheckMark.SetParent(const AValue: TMUIObject);
begin
  inherited;
  if Assigned(CheckLabel) then
    CheckLabel.Parent := AValue;
end;

procedure TMuiCheckMark.SetLeft(ALeft: Integer);
begin
  inherited;
  if Assigned(CheckLabel) then
  begin
    CheckLabel.Left := Left + Height + 2;
  end;
end;

procedure TMuiCheckMark.SetTop(ATop: Integer);
begin
  inherited;
  if Assigned(CheckLabel) then
  begin
    CheckLabel.Top := Top;
  end;
end;

procedure TMuiCheckMark.SetWidth(AWidth: Integer);
begin
  FullWidth := AWidth;
  inherited SetWidth(Height);
  if Assigned(CheckLabel) then
  begin
    CheckLabel.Left := Left + Height + 2;
    CheckLabel.Width := FullWidth - (Height + 2);
  end;
end;

procedure TMuiCheckMark.SetHeight(AHeight: Integer);
begin
  inherited SetHeight(AHeight);
  SetWidth(FullWidth);
  if Assigned(CheckLabel) then
    CheckLabel.Height := Height;
end;

function TMuiCheckMark.GetWidth(): Integer;
begin
  Result := FullWidth;
end;

procedure TMuiCheckMark.SetVisible(const AValue: Boolean);
begin
  inherited;
  if Assigned(CheckLabel) then
    CheckLabel.Visible := AValue;
end;

{ TMuiStringEdit }


procedure TextChangedFunc(Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;
var
  MuiObject: TMuiObject;
begin
  //writeln('text changed');
  if TObject(Hook^.h_Data) is TMuiObject then
  begin
    MuiObject := TMuiObject(Hook^.h_Data);
    SendSimpleMessage(MuiObject.PasObject, CM_TEXTCHANGED);
  end;
end;

function TMuiStringEdit.GetText: string;
var
  Pc: PChar;
begin
  Result := '';
  if Obj = nil then
    Exit;   
  Pc := PChar(GetAttribute(MUIA_String_Contents));     
  if Assigned(PC) then
    Result := string(Pc);
end;

procedure TMuiStringEdit.SetText(const AValue: string);
begin
  SetAttribute([MUIA_String_Contents, LongInt(PChar(AValue)), TAG_END]);
end;

constructor TMuiStringEdit.Create(const Params: array of const);
begin
  inherited Create(MUIO_String, Params);
  // Set Event for Changed Text
  TextChanged.h_Entry := IPTR(@TextChangedFunc);
  TextChanged.h_SubEntry := IPTR(@TextChangedFunc);
  TextChanged.h_Data := Self;
  CallHook(PHook(OCLASS(FObject)), FObject,
      [LongInt(MUIM_Notify), LongInt(MUIA_String_Acknowledge), LongInt(MUIV_EveryTime),
      LongInt(MUIV_Notify_Self),
      2,
      LongInt(MUIM_CallHook), @TextChanged
      ]);
end;

{ TMuiCycle }

procedure ActiveItemChangedFunc(Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;
var
  MuiObject: TMuiCycle;
begin
  if TObject(Hook^.h_Data) is TMuiCycle then
  begin
    MuiObject := TMuiCycle(Hook^.h_Data);
    LCLSendChangedMsg(MuiObject.PasObject, MuiObject.Active);
  end;
end;

function TMuiCycle.GetActive: LongInt;
begin
  Result := LongInt(GetAttribute(MUIA_Cycle_Active));
end;

procedure TMuiCycle.SetActive(const AValue: LongInt);
begin
  SetAttribute([LongInt(MUIA_Cycle_Active), AValue, TAG_END]);
end;

procedure TMuiCycle.ChangedItems(Sender: TObject);
begin
  SetAttribute([LongInt(MUIA_Cycle_Entries), @(FStrings.StringPtrs[0]), TAG_END]);
end;

constructor TMuiCycle.Create(ACaption: PChar; AStrings: TStrings);
var
  i: LongInt;
begin
  FStrings := TMuiStrings.create;
  for i:= 0 to AStrings.Count - 1 do
  begin
    FStrings.Add(AStrings.strings[i]);
  end;
  inherited Create(MUIO_Cycle, [ACaption, @(FStrings.StringPtrs[0])]);
  FStrings.OnChanged := @ChangedItems;
  // event for item changed
  ActiveItemChanged.h_Entry := IPTR(@ActiveItemChangedFunc);
  ActiveItemChanged.h_SubEntry := IPTR(@ActiveItemChangedFunc);
  ActiveItemChanged.h_Data := Self;
  CallHook(PHook(OCLASS(FObject)), FObject,
      [LongInt(MUIM_Notify), LongInt(MUIA_Cycle_Active), LongInt(MUIV_EveryTime),
      LongInt(MUIV_Notify_Self),
      2,
      LongInt(MUIM_CallHook), @ActiveItemChanged
      ]);

end;

Destructor TMuiCycle.Destroy;
begin  
  inherited;  
  FStrings.Free;
end;

{ TMuiTextEdit }

constructor TMuiTextEdit.Create(AStrings: TStrings; var Tags: TTagsList);
var
  i: Integer;
  scroll: pObject_;
  CreateTags: TTagsList;
begin
  FStrings := TFlowString.create;
  FStrings.FMuiObject := self;
  FTextObj := MUI_NewObjectA(PChar('TextEditor.mcc'), GetTagPtr(Tags));
  scroll := MUI_NewObjectA(MUIC_ScrollBar, NIL);
  AddTags(CreateTags, [MUIA_Group_Horiz, True, MUIA_Group_Child, FTextObj, MUIA_Group_Child, scroll, TAG_END]);
  inherited Create(MUIC_Group, GetTagPtr(CreateTags));
  SetAttObj(FTextObj, [$ad00001a, scroll, TAG_END]);
  //Create(PChar('TextEditor.mcc'), Tags);
  FText := AStrings.GetText;
  SetAttribute([LongInt($ad000002), FText, TAG_END]);
end;

Destructor TMuiTextEdit.Destroy;
begin;
  inherited;
  FStrings.Free;
end;

{ TFlowString }

constructor TFlowString.Create;
begin
  inherited;
  SL := TStringList.Create;
end;

destructor TFlowString.Destroy;
begin
  SL.Free;
  inherited;
end;

function TFlowString.GetCount: Integer;
var
  PC: Pchar;
begin
  if Assigned(FMuiObject) then
  begin
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [LongInt($ad000025)]));
    SL.SetText(PC);
    Result := SL.Count;
  end;
end;

function TFlowString.Add(const S: String): Integer;
var
  PC: Pchar;
begin
  if Assigned(FMuiObject) then
  begin
    //writeln('add: ', s);
    //PC := PChar(#10 + S);
    //CallHookPkt(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [LongInt($ad000026), PC, 2]);
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [LongInt($ad000025)]));
    SL.SetText(PC);
    Result := SL.Add(S);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[LongInt($ad000002), PC, TAG_END]);;
  end;
end;

procedure TFlowString.Clear;
begin
  if Assigned(FMuiObject) then
  begin
    SL.Clear;
    CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [LongInt($ad000024)]);
  end;
end;

procedure TFlowString.Delete(Index: Integer);
var
  PC: PChar;
begin
  if Assigned(FMuiObject) then
  begin;
    SL.Clear;
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [LongInt($ad000025)]));
    SL.SetText(PC);
    SL.Delete(Index);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[LongInt($ad000002), PC, TAG_END]);
  end;
end;

procedure TFlowString.Exchange(Index1: Integer; Index2: Integer);
var
  PC: PChar;
begin
  if Assigned(FMuiObject) then
  begin
    SL.Clear;
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [LongInt($ad000025)]));
    SL.SetText(PC);
    SL.Exchange(Index1, Index2);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[LongInt($ad000002), PC, TAG_END]);
  end;
end;

function TFlowString.Get(Index: Integer): string;
var
  PC: PChar;
begin
  if Assigned(FMuiObject) then
  begin
    SL.Clear;
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [LongInt($ad000025)]));
    SL.SetText(PC);
    Result := SL.strings[Index];
  end;
end;

procedure TFlowString.Put(Index: Integer; const S: string);
var
  PC: PChar;
begin
  inherited Put(Index, S);
  if Assigned(FMuiObject) then
  begin
    SL.Clear;
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [LongInt($ad000025)]));
    SL.SetText(PC);
    SL.strings[Index] := S;
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[LongInt($ad000002), PC, TAG_END]);
  end;
end;

procedure TFlowString.Insert(Index: Integer; const S: String);
var
  PC: PChar;
begin
  if Assigned(FMuiObject) then
  begin
    SL.Clear;
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [LongInt($ad000025)]));
    SL.SetText(PC);
    SL.Insert(Index, S);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[LongInt($ad000002), PC, TAG_END]);
  end;
end;

procedure TFlowString.LoadFromFile(const FileName: String);
var
  PC: PChar;
begin
  if Assigned(FMuiObject) then
  begin
    SL.Clear;
    SL.LoadFromFile(FileName);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[LongInt($ad000002), PC, TAG_END]);
  end;
end;

end.

