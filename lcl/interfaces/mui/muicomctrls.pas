unit muicomctrls;
{$mode objfpc}{$H+}
interface

uses
  controls, muibaseunit, mui, exec, utility, sysutils, strings, tagsarray, Intuition, Types,
  ComCtrls, LCLMessageGlue, LMessages, LCLType, graphics;

type

  { TMUIGauge }

  TMUIGauge = class(TMUIArea)
  private
    FMinPos: Integer;
    FMaxPos: Integer;
    FShowText: Boolean;
    Text: string;
    function GetMaxPos: Integer;
    function GetMinPos: Integer;
    function GetPosition: Integer;
    function GetShowText: boolean;
    procedure SetMaxPos(AValue: Integer);
    procedure SetMinPos(AValue: Integer);
    procedure SetPosition(AValue: Integer);
    procedure SetShowText(AValue: boolean);
    procedure UpdateText;
  public
    Horiz: Boolean;
    constructor Create(AClassName: PChar; Tags: PTagItem); override;
    property Position: Integer read GetPosition write SetPosition;
    property MinPos: Integer read GetMinPos write SetMinPos;
    property MaxPos: Integer read GetMaxPos write SetMaxPos;
    property ShowText: boolean read GetShowText write SetShowText;
  end;
  
  TMUIGroup = class(TMUIArea)
  protected
    procedure BasicInitOnCreate(); override;  
    procedure InstallHooks; override;
    function GetActivePage: Integer; virtual;
    procedure SetActivePage(AValue: Integer); virtual;
  public
    property ActivePage: Integer read GetActivePage write SetActivePage;    
  end;
  
  TMUIRegister = class(TMUIGroup)
  private
    FActivePage: Integer;
    FTexts: TMUIGroup;
    FRegisterHeight: Integer;
    TabHook: THook;
  protected
    procedure BasicInitOnCreate(); override;
    procedure SetParent(const AValue: TMUIObject); override;
    procedure InstallHooks; override;
    procedure SetLeft(ALeft: Integer); override;
    procedure SetTop(ATop: Integer); override;
    procedure SetWidth(AWidth: Integer); override;
    procedure SetHeight(AHeight: Integer); override;
    function GetHeight: Integer; override;
    function GetActivePage: Integer; override;
    procedure SetActivePage(AValue: Integer); override;
    procedure SetVisible(const AValue: boolean); override;
    procedure SetEnabled(const AValue: boolean); override;
    procedure SetColor(const AValue: TColor); override;
    function RegisterHeight: Integer;
  public
    ShowTabs: Boolean;
    FNames: array[0..100] of PChar;
    constructor Create(AClassName: PChar; var TagList: TTagsList); overload; reintroduce; virtual;
    destructor Destroy; override;
    procedure AddChild(APage: TMUIObject); override;
  end;  


implementation

{ TMUIGauge }

constructor TMUIGauge.Create(AClassName: PChar; Tags: PTagItem);
begin
  inherited;
  FMinPos := 0;
  FMaxPos := 100;
end;

function TMUIGauge.GetMaxPos: Integer;
begin
  Result := GetAttribute(IPTR(MUIA_Gauge_Max)) + FMinPos;
end;

function TMUIGauge.GetMinPos: Integer;
begin
  Result := FMinPos;
end;

function TMUIGauge.GetPosition: Integer;
begin
  Result := GetAttribute(IPTR(MUIA_Gauge_Current)) + FMinPos;
end;

function TMUIGauge.GetShowText: boolean;
begin
  Result := FShowText;
end;

procedure TMUIGauge.SetMaxPos(AValue: Integer);
begin
  FMaxPos := AValue;
  if FMaxPos - FMinPos > 0 then
  begin
    SetAttribute([LongInt(MUIA_Gauge_Max), FMaxPos - FMinPos, TAG_END]);
    if FShowText then
      UpdateText;
  end;
end;

procedure TMUIGauge.SetMinPos(AValue: Integer);
begin
  FMinPos := AValue;
  SetMaxPos(FMaxPos);
end;

procedure TMUIGauge.SetPosition(AValue: Integer);
begin
  SetAttribute([LongInt(MUIA_Gauge_Current), AValue - FMinPos, TAG_END]);
  if FShowText then
    UpdateText;
end;

procedure TMUIGauge.SetShowText(AValue: boolean);
begin
  FShowText := AValue;
  UpdateText;
end;

procedure TMUIGauge.UpdateText;
var
  Pos: Integer;
begin
  Text := '';
  if FShowText and ((FMaxPos - FMinPos) > 0) then
  begin
    Pos := Position;
    if not Horiz then
      Text := IntToStr(Pos)
    else
      Text := IntToStr(Pos) + ' from ['+IntToStr(FMinPos)+'-'+IntToStr(FMaxPos)+']('+IntToStr(Round(100*(Pos/(FMaxPos-FMinPos))))+'%%)'
  end;
  SetAttribute([LongInt(MUIA_Gauge_InfoText), PChar(Text)]);
end;

{ TMUIGroup }

function TMUIGroup.GetActivePage: Integer;
begin
  Result := GetAttribute(MUIA_Group_ActivePage);
end;

procedure TMUIGroup.SetActivePage(AValue: Integer);
var
  i: Integer;
begin
  for i := 0 to FChilds.Count - 1 do
  begin
    TMUIObject(FChilds[i]).Visible := i = AValue;
  end;
  SetAttribute([MUIA_Group_ActivePage, AValue]);
end;

procedure TMUIGroup.BasicInitOnCreate();
begin
  inherited;
end;

procedure TMUIGroup.InstallHooks;
begin
end;

{ TMUIRegister }

constructor TMUIRegister.Create(AClassName: PChar; var TagList: TTagsList);
var
  tg: TTagsList;
begin
  FColor := clNone;
  FActivePage := -1;
  FRegisterHeight := 30;
  //writeln('I got it');
  FNames[0] := GetMem(100);
  StrCopy(FNames[0], ' ');
  FNames[1] := nil;
  AddTags(tg, [MUIA_Register_Titles, IPTR(@FNames[0])]);
  FTexts := TMUIGroup.create(MUIC_Register, GetTagPtr(tg));
  //
  AddTags(TagList, [
    MUIA_InnerTop, 0, MUIA_InnerLeft, 0,
    MUIA_InnerBottom, 4, MUIA_InnerRight, 4,
    MUIA_Frame, MUIV_Frame_Group]);
  inherited Create(AClassName, GetTagPtr(TagList));
end;

procedure TMUIRegister.BasicInitOnCreate();
begin
  inherited;
end;

destructor TMUIRegister.Destroy;
begin
  inherited;
  FTexts.Free;
end;

procedure TMUIRegister.InstallHooks;
begin
end;

procedure TMUIRegister.SetParent(const AValue: TMUIObject);
begin
  inherited SetParent(AValue);
  if ShowTabs and Assigned(FTexts) then
    FTexts.Parent := AValue;
end;

procedure TMUIRegister.SetLeft(ALeft: Integer);
begin
  inherited SetLeft(ALeft);
  if ShowTabs then
    FTexts.Left := ALeft;
end;

procedure TMUIRegister.SetTop(ATop: Integer);
begin
  if ShowTabs then
  begin
    inherited SetTop(ATop + RegisterHeight);
    FTexts.Top := ATop;
  end else
  begin
    inherited SetTop(ATop);
  end;  
end;

procedure TMUIRegister.SetWidth(AWidth: Integer);
begin
  inherited SetWidth(AWidth);
  if ShowTabs then
    FTexts.Width := AWidth;  
end;

procedure TMUIRegister.SetHeight(AHeight: Integer);
begin
  if ShowTabs then
  begin
    inherited SetHeight(AHeight - RegisterHeight);
    FTexts.Height := RegisterHeight;
  end else
  begin
    inherited SetHeight(AHeight);
  end;
end;

function TMUIRegister.GetHeight: Integer;
begin
  if ShowTabs then  
  begin
    Result := FHeight + RegisterHeight;
  end else
  begin
    Result := FHeight;
  end;  
end;

function TMUIRegister.GetActivePage: Integer;
begin
  Result := GetAttribute(MUIA_Group_ActivePage);
end;

procedure TMUIRegister.SetActivePage(AValue: Integer);
var
  PGIdx: Integer;
begin
  if AValue < 0 then
    AValue := 0;
  FActivePage := AValue;
  TCustomTabControl(PasObject).PageIndex := AValue;  
  inherited SetActivePage(AValue);
  if ShowTabs and Assigned(FTexts.Obj) then
  begin
    PGIdx := GetAttObj(FTexts.Obj, MUIA_Group_ActivePage);
    if PGIdx <> FActivePage then
    begin
      SetAttObj(FTexts.Obj, [MUIA_Group_ActivePage, AValue]);
    end;  
    PasObject.Invalidate;
  end;  
end;

procedure TMUIRegister.SetVisible(const AValue: boolean);
begin
  inherited SetVisible(AValue);
  if Assigned(FTexts) then
    FTexts.Visible := AValue;
end;

procedure TMUIRegister.SetEnabled(const AValue: boolean);
begin
  inherited SetEnabled(AValue);
  if Assigned(FTexts) then
    FTexts.Enabled := AValue;
end;

function TMUIRegister.RegisterHeight: Integer;
begin
  Result := 0;
  if Assigned(FTexts) then
    Result := GetAttObj(FTexts.Obj, MUIA_InnerTop); 
  if Result = 0 then
    Result := FRegisterHeight;
end;

procedure TMUIRegister.SetColor(const AValue: TColor);
begin
  FColor := AValue;
  if Assigned(FTexts) then
  begin    
    FTexts.Color := FColor;
  end;  
end;

procedure TabIdxFunc(Hook: PHook; Obj: PObject_; Msg: Pointer); cdecl;
var
  MUIRegister: TMUIRegister;
  PGIdx: Integer;
  Mess: TLMNotify;
  NMHdr: tagNMHDR;
begin
  if TObject(Hook^.h_Data) is TMUIRegister then
  begin
    MUIRegister := TMUIRegister(Hook^.h_Data);
    PGIdx := MUIRegister.GetAttObj(MUIRegister.FTexts.Obj, MUIA_Group_ActivePage);
    //
    FillChar(Mess, SizeOf(Mess), 0);
    Mess.Msg := LM_Notify;
    FillChar(NMHdr, SizeOf(NMHdr), 0);
    NMHdr.Code := TCN_SELCHANGING;
    NMHdr.hwndFrom := PtrUInt(MUIRegister);
    NMHdr.idFrom := PGIdx;
    Mess.NMHdr := @NMHdr;
    DeliverMessage(MUIRegister.PasObject, Mess);
    // forbidden to change
    if Mess.Result <> 0 then
    begin
      PGIdx := MUIRegister.FActivePage;
      MUIRegister.SetAttObj(MUIRegister.FTexts.Obj, [MUIA_Group_ActivePage, MUIRegister.FActivePage]);  
      Exit;
    end;    
    MUIRegister.ActivePage := PGIdx;
    TCustomTabControl(MUIRegister.Pasobject).PageIndex := PGIdx;
    FillChar(Mess, SizeOf(Mess), 0);
    Mess.Msg := LM_Notify;
    FillChar(NMHdr, SizeOf(NMHdr), 0);
    NMHdr.Code := TCN_SELCHANGE;
    NMHdr.hwndFrom := PtrUInt(MUIRegister);
    NMHdr.idFrom := PGIdx;
    Mess.NMHdr := @NMHdr;
    DeliverMessage(MUIRegister.PasObject, Mess);
    //LCLSendChangedMsg(MUIRegister.PasObject, PGIdx);
  end;
end;


procedure TMUIRegister.AddChild(APage: TMUIObject);
var
  i: Integer;
  MyStr: string;
  l,t,w,h: Integer;
  tg: TTagsList;
  Tab: TCustomTabControl;
begin
  inherited;
  
  if not ShowTabs and Assigned(FTexts) then
  begin
    FTexts.Free;
    FTexts := nil;
  end;  
  
  if ShowTabs and Assigned(PasObject) and (PasObject is TCustomTabControl) then
  begin
    Tab := TCustomTabControl(PasObject);
  
    //
    l := FTexts.Left;
    t := FTexts.Top;
    w := FTexts.Width;
    h := FTexts.Height;
    
    FTexts.Free; 
    for i := 0 to FChilds.Count - 1 do
    begin
      if Assigned(FNames[i]) then
        FreeMem(FNames[i]);
      MyStr := Tab.Pages[i];
      FNames[i] := GetMem(Length(FNames) + 1);
      strings.StrCopy(FNames[i], PChar(MyStr));
    end;
    FNames[FChilds.Count + 1] := nil;
    AddTags(tg, [
      MUIA_Register_Titles, IPTR(@FNames[0]),
      MUIA_Frame, MUIV_Frame_None,
      MUIA_Register_Frame , False
      ]);
    FTexts := TMUIGroup.create(MUIC_Register, GetTagPtr(tg));
    FTexts.Top := t;
    FTexts.Left := l;
    FTexts.Width := w;
    FTexts.Height := h;
    FTexts.Parent := Parent;
    FTexts.Color := FColor;
    
    TabHook.h_Entry := IPTR(@TabIdxFunc);
    TabHook.h_SubEntry := 0;
    TabHook.h_Data := Self;
    
    DoMethodObj(FTexts.Obj, [IPTR(MUIM_Notify), IPTR(MUIA_Group_ActivePage), IPTR(MUIV_EveryTime),
      IPTR(MUIV_Notify_Self), 2, IPTR(MUIM_CallHook), IPTR(@TabHook)]); 
  end;
end;

end.
