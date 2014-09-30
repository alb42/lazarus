unit MuiStdCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Contnrs, Exec, AmigaDos, Intuition, Utility, Mui, Forms,
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
    constructor Create(const Tags : Array Of Const); overload; reintroduce; virtual;
  end;

  { TMuiCheckMark }

  TMuiCheckMark = class(TMuiArea)
  private
    function GetChecked: LongBool;
    procedure SetChecked(const AValue: LongBool);
  public
    constructor Create(const Params : Array Of Const); overload; reintroduce; virtual;
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
    constructor Create(AStrings: TStrings; const Tags : Array Of Const); overload; reintroduce; virtual;
    Destructor Destroy; override;
    property Strings: TFlowString read FStrings write FStrings;
    property TextObj: pObject_ read FTextObj write FTextObj;
  end;

  { TFloatText }

  TFloatText = class(TMuiArea)
  public
    constructor Create(const Tags: array of Const); overload; reintroduce; virtual;
  end;

  { TMuiListView }

  TMuiListView = class(TMuiArea)
  private
    FFloatText: TFloatText;
    FStrings: TStringList;
    function GetActive: LongInt;
    procedure SetActive(const AValue: LongInt);
    procedure TextChanged(Sender: TObject);
  public
    constructor Create(AStrings:TStrings; const Tags: array of Const); overload; reintroduce; virtual;
    destructor Destroy; override;
    property Strings: TStringList read FStrings;
    property Active: LongInt read GetActive write SetActive;
    property FloatText: TFloatText read FFloatText;
  end;

implementation

uses
  tagsarray;

{ TFloatText }

constructor TFloatText.Create(const Tags: array of const);
begin
  inherited Create(MUIC_FloatText, Tags);
end;

{ TMuiListView }

constructor TMuiListView.Create(AStrings:TStrings; const Tags: array of const);
var
  FText: PChar;
begin
  FStrings := TStringList.create;
  FStrings.Assign(AStrings);
  FText := FStrings.GetText;
  FFloatText := TFloatText.create([TAG_END]);

  inherited Create(MUIC_ListView, [MUIA_Listview_List, FloatText.Obj ,TAG_DONE]);
  FStrings.OnChange := @TextChanged;
end;

destructor TMuiListView.Destroy;
begin
  FFloatText.Free;
  inherited Destroy;
end;

procedure TMuiListView.TextChanged(Sender: TObject);
var
  FText: PChar;
begin
  FText := FStrings.GetText;
  SetAttrsA(FloatText.Obj, ReadInTags([LongInt(MUIA_FloatText_Text), FText, TAG_END]));
end;

function TMuiListView.GetActive: LongInt;
var
  Res: LongInt;
begin
  GetAttr(LongInt(MUIA_List_Active), FloatText.Obj, @Res);
  if Res = MUIV_List_Active_Off then
    Result := -1
  else
    Result := Res;
end;

procedure TMuiListView.SetActive(const AValue: LongInt);
var
  Res: LongInt;
begin
  if AValue = -1 then
    Res := MUIV_List_Active_Off
  else
    Res := AValue;
  SetAttrsA(FloatText.Obj, ReadInTags([LongInt(MUIA_List_Active), Res, TAG_END]));
end;

{ TMuiButton }

constructor TMuiButton.Create(const Params: array of const);
begin
  inherited Create(MUIO_Button, Params);
end;

{ TMuiText }

constructor TMuiText.Create(const Tags: array of const);
begin
  inherited Create(MUIC_Text, Tags);
end;

{ TMuiCheckMark }


function TMuiCheckMark.GetChecked: LongBool;
begin
  Result := LongBool(GetAttribute(MUIA_Selected));
end;

procedure TMuiCheckMark.SetChecked(const AValue: LongBool);
begin
  SetAttribute([LongInt(MUIA_Selected), LongInt(AValue), TAG_END]);
end;

constructor TMuiCheckMark.Create(const Params: array of const);
begin
  inherited Create(MUIO_Checkmark, Params);
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
  Pc := PChar(GetAttribute(MUIA_String_Contents));
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

constructor TMuiTextEdit.Create(AStrings: TStrings; const Tags: array of const);
var
  i: Integer;
  scroll: pObject_;
begin
  FStrings := TFlowString.create;
  FStrings.FMuiObject := self;
  FTextObj := MUI_NewObject(PChar('TextEditor.mcc'), Tags);
  scroll := MUI_NewObjectA(MUIC_ScrollBar, NIL);
  inherited Create(MUIC_Group, [MUIA_Group_Horiz, True, MUIA_Group_Child, FTextObj, MUIA_Group_Child, scroll, TAG_END]);
  SetAttObj(FTextObj, [$ad00001a, scroll, TAG_END]);
  //Create(PChar('TextEditor.mcc'), Tags);
  FText := AStrings.GetText;
  SetAttribute([LongInt($ad000002), FText, TAG_END]);
end;

Destructor TMuiTextEdit.Destroy;
begin;
  FStrings.Free;
  inherited;
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

