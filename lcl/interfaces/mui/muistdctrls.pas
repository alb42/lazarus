unit MuiStdCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Contnrs, Exec, AmigaDos, Intuition, Utility, Mui, Forms,
  tagsarray, muidrawing, buttons, Math, Graphics,
  {$ifdef HASAMIGA}
  cybergraphics, agraphics,
  {$endif}
  MuiBaseUnit, StdCtrls, muistringsunit, LCLMessageGlue, LMessages;

  { TMuiButton }
type

  TMuiButton = class(TMuiArea)
  public
    constructor Create(const Params : Array Of Const); overload; reintroduce; virtual;
  end;

  TMuiBitBtn = class(TMuiArea)
  private
    FCaption: string;
  protected
    procedure SetCaption(const AValue: string); override;
    function GetCaption: string; override;
    procedure DoReDraw(); override;
  public
    FLayout: TButtonLayout;
    FMargin: Integer;
    FSpacing: Integer;
    FBitmap: TMUIBitmap;
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
  protected
    FullWidth: Integer;
    CheckLabel: TMuiText;
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
    constructor Create(ObjType : LongInt; const Params : Array Of Const); override;
    destructor Destroy; override;
  end;

  { TMuiRadioButton }

  TMuiRadioButton = class(TMuiCheckMark)
  protected
    procedure SetChecked(const AValue: LongBool); override;
  public
    procedure MakeOneChecked;
    procedure RemoveCheck;
  end;


  { TMuiToggleButton }

  TMuiToggleButton = class(TMuiArea)
  private
    CheckHook: THook;
  public
    constructor Create(ObjType : LongInt; const Params : Array Of Const); override;
  end;

  { TMuiStringEdit }

  TMuiStringEdit = class(TMuiArea)
  private
    TextChanged: THook;
    TextDone: THook;
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
    StringPtrs: TStringPtrs;
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
    procedure SetReadOnly(AReadOnly: Boolean);
    function GetReadOnly: Boolean;
  public
    constructor Create(AStrings: TStrings; var Tags: TTagsList); overload; reintroduce; virtual;
    Destructor Destroy; override;
    property Strings: TFlowString read FStrings write FStrings;
    property TextObj: pObject_ read FTextObj write FTextObj;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
  end;

  { TFloatText }

  TFloatText = class(TMuiArea)
  public
    constructor Create(var Tags: TTagsList); overload; reintroduce; virtual;
  end;

  { TMuiListView }

  TMuiListView = class(TMuiArea)
  private
    FText: PChar;
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

  { TMUIScrollbar }

  TMUIScrollBar = class(TMuiGroup)
  private
    FMinValue: Integer;
    FMaxValue: Integer;
    FPageSize: Integer;
    ChangeHook: THook;
    BlockScrollEvent: Boolean;
    function GetHoriz: Boolean;
    function GetMaxValue: Integer;
    function GetMinValue: Integer;
    function GetPageSize: Integer;
    function GetPosition: Integer;
    procedure SetHoriz(AValue: Boolean);
    procedure SetMaxValue(AValue: Integer);
    procedure SetMinValue(AValue: Integer);
    procedure SetPageSize(AValue: Integer);
    procedure SetPosition(AValue: Integer);
  protected
    procedure InstallHooks; override;
  public
    constructor Create(var Tags: TTagsList); overload; reintroduce; virtual;

    property Horizontal: Boolean read GetHoriz write SetHoriz;
    property MinValue: Integer read GetMinValue write SetMinValue;
    property MaxValue: Integer read GetMaxValue write SetMaxValue;
    property Position: Integer read GetPosition write SetPosition;
    property PageSize: Integer read GetPageSize write SetPageSize;
  end;
  
  {TMUIGroupBox}
  
  TMUIGroupBox = class(TMUIGroup)
  protected
    function GetCaption: string; override;
    procedure SetCaption(const AValue: string); override;
  public
    constructor Create(var Tags: TTagsList); overload; reintroduce; virtual;
  end;

implementation

uses
  LCLType;

{ TMUIScrollBar }

function TMUIScrollBar.GetHoriz: Boolean;
begin
  Result := Boolean(GetAttribute(MUIA_Group_Horiz));
end;

function TMUIScrollBar.GetMaxValue: Integer;
begin
  Result := FMaxValue;
end;

function TMUIScrollBar.GetMinValue: Integer;
begin
  Result := FMinValue;
end;

function TMUIScrollBar.GetPageSize: Integer;
begin
  Result := GetAttribute(MUIA_Prop_Visible);
end;

function TMUIScrollBar.GetPosition: Integer;
begin
  Result := GetAttribute(MUIA_Prop_First) + FMinValue;
end;

procedure TMUIScrollBar.SetHoriz(AValue: Boolean);
begin
  SetAttribute([IPTR(MUIA_Group_Horiz), IPTR(AValue)]);
end;

procedure TMUIScrollBar.SetMaxValue(AValue: Integer);
begin
  FMaxValue := AValue;
  SetAttribute([IPTR(MUIA_Prop_Entries), (AValue - FMinValue) + FPageSize]);
end;

procedure TMUIScrollBar.SetMinValue(AValue: Integer);
begin
  FMinValue := AValue;
  SetAttribute([IPTR(MUIA_Prop_Entries), (FMaxValue - AValue) + FPageSize]);
end;

procedure TMUIScrollBar.SetPageSize(AValue: Integer);
begin
  FPageSize := AValue;
  SetAttribute([IPTR(MUIA_Prop_Entries), (FMaxValue - FMinValue) + AValue]);  
  SetAttribute([IPTR(MUIA_Prop_Visible), AValue]);  
end;

procedure TMUIScrollBar.SetPosition(AValue: Integer);
begin
  if AValue <> Position then
  begin
    SetAttribute([IPTR(MUIA_Prop_First), AValue - FMinValue]);
  end;  
end;

procedure ChangeScroll(Hook: PHook; Obj: PObject_; Msg: Pointer); cdecl;
var
  Parent, MuiObject: TMuiObject;
  ScrollMsg: TLMVScroll;
  SendMsg: Boolean;
begin
  //writeln('--> Scroll hook');
  //Exit;
  if TObject(Hook^.h_Data) is TMuiObject then
  begin
    MuiObject := TMuiObject(Hook^.h_Data);
    if TMUIScrollbar(MUIObject).Horizontal then
      ScrollMsg.Msg := LM_HSCROLL
    else
      ScrollMsg.Msg := LM_VScroll;  
    ScrollMsg.Pos := TMUIScrollBar(MUIObject).Position;
    ScrollMsg.ScrollBar := PtrUInt(MuiObject);
    ScrollMsg.ScrollCode := SB_THUMBPOSITION;//SB_ENDSCROLL;   
    if MuiObject.PasObject is TScrollbar then
    begin
      if TScrollbar(MuiObject.PasObject).Position <> ScrollMsg.Pos then
      begin
        TScrollbar(MuiObject.PasObject).Position := ScrollMsg.Pos;
        DeliverMessage(TControl(MuiObject.PasObject), ScrollMsg);
      end;
    end else
    begin
      Parent := MUIObject.Parent;
      SendMsg := False;
      ScrollMsg.Pos := TMUIScrollBar(MUIObject).Position;
      if (Parent.VScroll = MUIObject) then
      begin
        if ScrollMsg.Pos <> Parent.VScrollPos then
        begin
          Parent.VSCrollPos := Scrollmsg.Pos;
          SendMsg := True;
        end;
      end;
      if (Parent.HScroll = MUIObject) then
      begin
        if ScrollMsg.Pos <> Parent.HScrollPos then
        begin
          Parent.HSCrollPos := Scrollmsg.Pos;
          SendMsg := True;
        end;
      end;
      if SendMsg then
        DeliverMessage(TControl(MuiObject.PasObject), ScrollMsg);
    end;
  end;
  //writeln('<-- Scroll hook');
end;


procedure TMUIScrollBar.InstallHooks;
begin
  inherited InstallHooks;
  ChangeHook.h_Entry := IPTR(@ChangeScroll);
  ChangeHook.h_SubEntry := 0;
  ChangeHook.h_Data := Self;

  DoMethod([IPTR(MUIM_Notify), IPTR(MUIA_Prop_First), IPTR(MUIV_EveryTime),
    IPTR(MUIV_Notify_Self), 2, IPTR(MUIM_CallHook), IPTR(@ChangeHook)]);
end;

constructor TMUIScrollBar.Create(var Tags: TTagsList);
begin
  FMinValue := 0;
  FMaxValue := 100;
  FPageSize := 10;
  BlockScrollEvent := False;
  inherited Create(MUIC_Scrollbar, GetTagPtr(Tags));
end;


{ TMuiRadioButton }

procedure TMuiRadioButton.SetChecked(const AValue: LongBool);
var
  i: Integer;
  RB: TMUIObject;
begin
  if AValue and Assigned(Parent) then
  begin
    for i := 0 to Parent.FChilds.Count - 1 do
    begin
      RB := TMUIObject(Parent.FChilds.Items[i]);
      if (RB is TMuiRadioButton) and (RB.obj <> Self.obj) then
      begin
        if TMuiRadioButton(RB).checked then
          TMuiRadioButton(RB).RemoveCheck;
      end;
    end;
  end;
  inherited SetChecked(AValue);
end;

procedure TMuiRadioButton.MakeOneChecked;
var
  i: Integer;
  RB: TObject;
begin
  if Assigned(Parent) then
  begin
    for i := 0 to Parent.FChilds.Count - 1 do
    begin
      RB := TMUIObject(Parent.FChilds.Items[i]);
      if (RB is TMuiRadioButton) then
      begin
        if TMuiRadioButton(RB).checked then
        begin
          Exit;
        end;
      end;
    end;
    Self.Checked := True;
  end;
end;

procedure TMuiRadioButton.RemoveCheck;
begin
  inherited SetChecked(False);
end;

procedure TMuiBitBtn.DoReDraw();
var
  GlyphPos: TPoint;
  TextPos: TPoint;
  Len, Hi: Integer;
  TextLength: Integer;
  GLeft, GWidth:Integer;
  GTop, GHeight: Integer;
  DHeight, DWidth: Integer;
  Ma, Spa: Integer;

  procedure GlyphLeft;
  var
    GlyTextWi: Integer;
  begin
    GWidth := Min(FBitmap.FWidth, DWidth - ((Ma * 2) + Len + Spa));
    GHeight := Min(FBitmap.FHeight, DHeight - ((Ma * 2) + Spa));
    GLeft := Max(0,(FBitmap.FWidth - GWidth) div 2);
    GTop := Max(0, (FBitmap.FHeight - GHeight) div 2);

    if FMargin < 0 then
    begin
      GlyphPos.Y := DHeight div 2 - (GHeight div 2);
      TextPos.Y := ((DHeight - Hi) div 2) + 2;
      GlyTextWi := GWidth + FSpacing + Len; // Width of Glyph + Text + spacing
      GlyphPos.X := (DWidth - GlyTextWi) div 2;
      TextPos.X := GlyphPos.X + GWidth + FSpacing;
    end else
    begin
      GlyphPos.Y := DHeight div 2 - (GHeight div 2);
      GlyphPos.X := FMargin;
      TextPos.Y := DHeight div 2 - ((Hi div 2) - 2);
      if FSpacing < 0 then
      begin
        TextPos.X := (DWidth - Len) div 2
      end else
        TextPos.X := GlyphPos.X + GWidth + FSpacing;
    end;
  end;

  procedure GlyphRight;
  var
    GlyTextWi: Integer;
  begin
    GWidth := Min(FBitmap.FWidth, DWidth - ((Ma * 2) + Len + Spa));
    GHeight := Min(FBitmap.FHeight, DHeight - ((Ma * 2) + Spa));
    GLeft := Max(0,(FBitmap.FWidth - GWidth) div 2);
    GTop := Max(0, (FBitmap.FHeight - GHeight) div 2);

    if FMargin < 0 then
    begin
      GlyphPos.Y := DHeight div 2 - (GHeight div 2);
      TextPos.Y := ((DHeight - Hi) div 2) + 2;
      GlyTextWi := GWidth + FSpacing + Len; // Width of Glyph + Text + spacing
      TextPos.X := (DWidth - GlyTextWi) div 2;
      GlyphPos.X := TextPos.X + FSpacing + Len;
    end else
    begin
      GlyphPos.Y := DHeight div 2 - (GHeight div 2);
      TextPos.X := FMargin;
      TextPos.Y := DHeight div 2 - ((Hi div 2) - 2);
      if FSpacing < 0 then
      begin
        GlyphPos.X := (DWidth - GWidth) div 2
      end else
        GlyphPos.X := TextPos.X + Len + FSpacing;
    end;
  end;

  procedure GlyphTop;
  var
    GlyTextHi: Integer;
  begin
    GWidth := Min(FBitmap.FWidth, DWidth - ((Ma * 2) + Spa));
    GHeight := Min(FBitmap.FHeight, DHeight - ((Ma * 2) + Hi + Spa));
    GLeft := Max(0,(FBitmap.FWidth - GWidth) div 2);
    GTop := Max(0, (FBitmap.FHeight - GHeight) div 2);
    if FMargin < 0 then
    begin
      GlyphPos.X := DWidth div 2 - (GWidth div 2);
      TextPos.X := ((DWidth - Len) div 2) + 2;
      GlyTextHi := GHeight + FSpacing + Hi; // Width of Glyph + Text + spacing
      GlyphPos.Y := (DHeight - GlyTextHi) div 2;
      TextPos.Y := GlyphPos.Y + GHeight + FSpacing + 2;
    end else
    begin
      GlyphPos.X := DWidth div 2 - (GWidth div 2);
      GlyphPos.Y := FMargin;
      TextPos.X := DWidth div 2 - (Len div 2);
      if FSpacing < 0 then
      begin
        TextPos.Y := ((DHeight - Hi) div 2) + 2
      end else
        TextPos.Y := GlyphPos.Y + GHeight + FSpacing + 2;
    end;
  end;

  procedure GlyphBottom;
  var
    GlyTextHi: Integer;
  begin
    GWidth := Min(FBitmap.FWidth, DWidth - ((Ma * 2) + Spa));
    GHeight := Min(FBitmap.FHeight, DHeight - ((Ma * 2) + Hi + Spa));
    GLeft := Max(0,(FBitmap.FWidth - GWidth) div 2);
    GTop := Max(0, (FBitmap.FHeight - GHeight) div 2);
    if FMargin < 0 then
    begin
      GlyphPos.X := DWidth div 2 - (GWidth div 2);
      TextPos.X := ((DWidth - Len) div 2);
      GlyTextHi := GHeight + FSpacing + Hi; // Width of Glyph + Text + spacing
      TextPos.Y := ((DHeight - GlyTextHi) div 2) + (Hi div 2) + 2;
      GlyphPos.Y := TextPos.Y + FSpacing + Hi div 2;
    end else
    begin
      GlyphPos.X := DWidth div 2 - (GWidth div 2);
      TextPos.Y := FMargin + 2;
      TextPos.X := DWidth div 2 - (Len div 2);
      if FSpacing < 0 then
      begin
        GlyphPos.Y := (DHeight - GHeight) div 2
      end else
        GlyphPos.Y := TextPos.Y + Hi + FSpacing;
    end;
  end;

begin
  inherited;
  if Assigned(MUICanvas) then
  begin
    Ma := Max(4, FMargin);
    Spa:= Max(4, FSpacing);
    DHeight := MUICanvas.DrawRect.Bottom - MUICanvas.DrawRect.Top;
    DWidth := MUICanvas.DrawRect.Right - MUICanvas.DrawRect.Left;
    SetDrMd(MUICanvas.RastPort, JAM1);
    TextLength := Length(FCaption);
    Hi := MUICanvas.TextHeight(PChar(FCaption), TextLength);
    Len := MUICanvas.TextWidth(PChar(FCaption), TextLength);
    TextPos.Y := (MUICanvas.DrawRect.Bottom - MUICanvas.DrawRect.Top) div 2 - ((Hi div 2) - 2);
    TextPos.X := ((MUICanvas.DrawRect.Right - MUICanvas.DrawRect.Left) - Len) div 2;
    if Assigned(FBitmap) then
    begin
      case FLayout of
        blGlyphLeft: GlyphLeft;
        blGlyphRight: GlyphRight;
        blGlyphTop: GlyphTop;
        blGlyphBottom: GlyphBottom;
      end;
      WritePixelArrayAlpha(FBitmap.FImage, GLeft, GTop, FBitmap.FWidth * SizeOf(LongWord), MUICanvas.RastPort, MUICanvas.GetOffset.X + GlyphPos.X, MUICanvas.GetOffset.Y + GlyphPos.Y, GWidth, GHeight, 255)
    end;
    MUICanvas.MoveTo(TextPos.X, TextPos.Y);
    MUICanvas.WriteText(PChar(FCaption), TextLength);
  end;
end;

procedure TMuiBitBtn.SetCaption(const AValue: string);
begin
  FCaption := AValue;
end;

function TMuiBitBtn.GetCaption: string;
begin
  Result := FCaption;
end;

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
begin
  if TObject(Hook^.h_Data) is TMuiListView then
  begin
    MuiObject := TMuiListView(Hook^.h_Data);
    LCLSendMouseMultiClickMsg(TControl(MuiObject.PasObject), 0, 0, mbLeft, 2);
  end;
end;


{ TMuiListView }

constructor TMuiListView.Create(AStrings:TStrings; var Tags: TTagsList);
var
  MenuTags: TTagsList;
begin
  FText := nil;
  FStrings := TStringList.create;
  FStrings.Assign(AStrings);
  FFloatText := TFloatText.create(MenuTags);
  AddTags(Tags, [IPTR(MUIA_Listview_List), FloatText.Obj ,TAG_DONE]);
  inherited Create(MUIC_ListView, GetTagPtr(Tags));
  FStrings.OnChange := @TextChanged;
  
  ListChangeHook.h_Entry := IPTR(@ListChangeFunc);
  ListChangeHook.h_SubEntry := 0;
  ListChangeHook.h_Data := Self;
  //
  DoubleClickHook.h_Entry := IPTR(@DoubleClickFunc);
  DoubleClickHook.h_SubEntry := 0;
  DoubleClickHook.h_Data := Self;
  
  DoMethod([IPTR(MUIM_Notify),
    IPTR(MUIA_List_Active), IPTR(MUIV_EveryTime),
    IPTR(MUIV_Notify_Self),
    2,
    IPTR(MUIM_CallHook), IPTR(@ListChangeHook)
    ]);
    
  DoMethod([IPTR(MUIM_Notify),
    IPTR(MUIA_ListView_DoubleClick), IPTR(True),
    IPTR(MUIV_Notify_Self),
    2,
    IPTR(MUIM_CallHook), IPTR(@DoubleClickHook)
    ]);  
end;

destructor TMuiListView.Destroy;
begin
  inherited Destroy;
  // Object ist automatically destroyed by Listview
  FFloatText.Obj := nil;
  FFloatText.Free;
  FreeMem(FText);
  FStrings.Free;
end;

procedure TMuiListView.TextChanged(Sender: TObject);
var
  str: String;
  TagList: TTagsList;
begin
  if Assigned(FStrings) then
  begin
    Str := FStrings.GetText + #0;
    if Assigned(FText) then
      FreeMem(FText);
    FText := system.AllocMem(Length(str));
    Move(str[1], FText^, Length(str));
    AddTags(TagList, [IPTR(MUIA_FloatText_Text), FText, TAG_END]);
    SetAttrsA(FloatText.Obj, GetTagPtr(TagList));
  end;  
end;

function TMuiListView.GetActive: LongInt;
var
  Res: LongInt;
begin
  Result := 0;
  GetAttr(IPTR(MUIA_List_Active), FloatText.Obj, @Res);
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
  AddTags(TagList, [IPTR(MUIA_List_Active), Res, TAG_END]);  
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
  //AddTags(Tags, [LongInt(MUIA_BACKGROUND), MUII_BACKGROUND]);
  //AddTags(Tags, [LongInt(MUIA_BACKGROUND), MUII_FILLSHINE]);
  inherited Create(MUIC_Text, GetTagPtr(Tags));
end;

{ TMuiCheckMark }

procedure CheckFunc(Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;
var
  MuiObject: TMuiObject;
  SendMessages: Boolean;
begin
  SendMessages := True;
  if TObject(Hook^.h_Data) is TMuiObject then
  begin
    MuiObject := TMuiObject(Hook^.h_Data);
    if MUIObject is TMUIArea then
    begin
      if TMUIArea(MUIObject).FBlockChecked then
        SendMessages := False;  
    end;
    if MuiObject is TMUIRadioButton then
    begin
      if TMUIRadioButton(MUIObject).Checked then
      begin   
        TMUIRadioButton(MUIObject).Checked := True;
      end else
      begin  
        TMUIRadioButton(MUIObject).MakeOneChecked;
      end;
    end;
    if SendMessages then   
      LCLSendChangedMsg(TControl(MuiObject.PasObject), 0);    
  end;
end;

constructor TMuiCheckMark.Create(ObjType : LongInt; const Params: array of const);
var
  TagList: TTagsList;
  Taglist2: TTagsList;
begin
  if ObjType = MUIO_Radio then
  begin
    AddTags(TagList2, [
      IPTR(MUIA_InputMode), IPTR(MUIV_InputMode_Immediate),
      IPTR(MUIA_ShowSelState), IPTR(False),
      IPTR(MUIA_Image_Spec), IPTR(MUII_RadioButton)]);
    inherited Create(MUIC_Image, GetTagPtr(TagList2));
  end else
    inherited Create(ObjType, Params);
  CheckLabel := TMuiText.Create(TagList);
  CheckHook.h_Entry := IPTR(@CheckFunc);
  CheckHook.h_SubEntry := 0;
  CheckHook.h_Data := Self;
  
  DoMethod([IPTR(MUIM_Notify),
    IPTR(MUIA_Selected), IPTR(MUIV_EveryTime),
    IPTR(MUIV_Notify_Self),
    2,
    IPTR(MUIM_CallHook), IPTR(@CheckHook)
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
  begin
    CheckLabel.Caption := AValue;
    CheckLabel.Visible := AValue <> '';
  end;  
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
  if Self is TMUIRadioButton then
  begin
    inherited SetWidth(16);
  end else
  begin
    inherited SetWidth(20);
  end;
  if Assigned(CheckLabel) and (CheckLabel.Visible) then
  begin
    CheckLabel.Left := Left + Height + 2;
    CheckLabel.Width := FullWidth - (Height + 2);
  end;
end;

procedure TMuiCheckMark.SetHeight(AHeight: Integer);
begin
  if Self is TMUIRadioButton then
  begin
    inherited SetHeight(16);
  end else
  begin
    inherited SetHeight(20);
  end;
  SetWidth(FullWidth);
  if Assigned(CheckLabel) and (CheckLabel.Visible) then
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
    CheckLabel.Visible := AValue and (Caption <> '');
end;

{ TMuiToggleButton }

constructor TMuiToggleButton.Create(ObjType: LongInt;
  const Params: array of const);
begin
  inherited Create(MUIO_Button, Params);
  SetAttribute([IPTR(MUIA_InputMode), IPTR(MUIV_InputMode_Toggle)]);
  CheckHook.h_Entry := IPTR(@CheckFunc);
  CheckHook.h_SubEntry := 0;
  CheckHook.h_Data := Self;

  DoMethod([IPTR(MUIM_Notify),
    IPTR(MUIA_Selected), IPTR(MUIV_EveryTime),
    IPTR(MUIV_Notify_Self),
    2,
    IPTR(MUIM_CallHook), IPTR(@CheckHook)
    ]);
end;

{ TMuiStringEdit }

procedure TextDoneFunc(Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;
var
  MuiObject: TMuiObject;
begin
  //writeln('editing done');
  if TObject(Hook^.h_Data) is TMuiObject then
  begin
    MuiObject := TMuiObject(Hook^.h_Data);
    MuiObject.PasObject.EditingDone;
  end;
end;


procedure TextChangedFunc(Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;
var
  MuiObject: TMuiObject;
begin
  //writeln('edit text changed');
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
  if AValue <> GetText then
    SetAttribute([IPTR(MUIA_String_Contents), PChar(AValue), TAG_END]);
end;

constructor TMuiStringEdit.Create(const Params: array of const);
begin
  inherited Create(MUIO_String, Params);
  // Set Event for Changed Text
  TextChanged.h_Entry := IPTR(@TextChangedFunc);
  TextChanged.h_SubEntry := 0;
  TextChanged.h_Data := Self;
  CallHook(PHook(OCLASS(FObject)), FObject,
      [IPTR(MUIM_Notify), IPTR(MUIA_String_Contents), IPTR(MUIV_EveryTime),
      IPTR(MUIV_Notify_Self),
      2,
      IPTR(MUIM_CallHook), @TextChanged
      ]);
  TextDone.h_Entry := IPTR(@TextDoneFunc);
  TextDone.h_SubEntry := 0;
  TextDone.h_Data := Self;
  CallHook(PHook(OCLASS(FObject)), FObject,
      [IPTR(MUIM_Notify), IPTR(MUIA_String_Acknowledge), IPTR(MUIV_EveryTime),
      IPTR(MUIV_Notify_Self),
      2,
      IPTR(MUIM_CallHook), @TextDone
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
  SetAttribute([IPTR(MUIA_Cycle_Active), AValue, TAG_END]);
end;

procedure TMuiCycle.ChangedItems(Sender: TObject);
begin
  // on change -> recreate the combobox (items only set on initialization in MUI)
  RecreateWnd(TWinControl(PasObject));
end;

constructor TMuiCycle.Create(ACaption: PChar; AStrings: TStrings);
var
  str: string;
  Len: Integer;
  i: LongInt;
begin
  FStrings := TMuiStrings.create;
  SetLength(StringPtrs, AStrings.Count + 1);
  for i:= 0 to AStrings.Count - 1 do
  begin
    str := AStrings.strings[i] + #0;
    FStrings.Add(str);
    Len := Length(Str);
    StringPtrs[i] := System.AllocMem(Len + 1);
    Move(Str[1], StringPtrs[i]^, Len);
  end;
  StringPtrs[High(StringPtrs)] := nil;
  inherited Create(MUIO_Cycle, [ACaption, @(StringPtrs[0])]);
  FStrings.OnChange := @ChangedItems;
  // event for item changed
  ActiveItemChanged.h_Entry := IPTR(@ActiveItemChangedFunc);
  ActiveItemChanged.h_SubEntry := 0;
  ActiveItemChanged.h_Data := Self;
  CallHook(PHook(OCLASS(FObject)), FObject,
      [IPTR(MUIM_Notify), IPTR(MUIA_Cycle_Active), IPTR(MUIV_EveryTime),
      IPTR(MUIV_Notify_Self),
      2,
      IPTR(MUIM_CallHook), @ActiveItemChanged
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
  scroll: pObject_;
  CreateTags: TTagsList;
begin
  FStrings := TFlowString.create;
  FStrings.FMuiObject := self;
  FTextObj := MUI_NewObjectA(PChar('TextEditor.mcc'), GetTagPtr(Tags));
  scroll := MUI_NewObjectA(MUIC_ScrollBar, NIL);
  AddTags(CreateTags, [IPTR(MUIA_Group_Horiz), True, IPTR(MUIA_Group_Child), FTextObj, IPTR(MUIA_Group_Child), scroll, TAG_END]);
  inherited Create(MUIC_Group, GetTagPtr(CreateTags));
  SetAttObj(FTextObj, [IPTR($ad00001a), scroll, TAG_END]);
  //Create(PChar('TextEditor.mcc'), Tags);
  FText := AStrings.GetText;
  SetAttribute([IPTR($ad000002), FText, TAG_END]);
end;

Destructor TMuiTextEdit.Destroy;
begin;
  inherited;
  FStrings.Free;
end;

const
  TextEditor_Dummy = $ad000000;
  MUIA_TextEditor_ReadOnly = TextEditor_Dummy + $19;

procedure TMuiTextEdit.SetReadOnly(AReadOnly: Boolean);
begin
  SetAttribute([IPTR(MUIA_TextEditor_ReadOnly), AReadOnly]);
end;

function TMuiTextEdit.GetReadOnly: Boolean;
begin
  Result := Boolean(GetAttribute(IPTR(MUIA_TextEditor_ReadOnly)));
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
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [IPTR($ad000025)]));
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
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [IPTR($ad000025)]));
    SL.SetText(PC);
    Result := SL.Add(S);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[IPTR($ad000002), PC, TAG_END]);;
  end;
end;

procedure TFlowString.Clear;
begin
  if Assigned(FMuiObject) then
  begin
    SL.Clear;
    CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [IPTR($ad000024)]);
  end;
end;

procedure TFlowString.Delete(Index: Integer);
var
  PC: PChar;
begin
  if Assigned(FMuiObject) then
  begin;
    SL.Clear;
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [IPTR($ad000025)]));
    SL.SetText(PC);
    SL.Delete(Index);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[IPTR($ad000002), PC, TAG_END]);
  end;
end;

procedure TFlowString.Exchange(Index1: Integer; Index2: Integer);
var
  PC: PChar;
begin
  if Assigned(FMuiObject) then
  begin
    SL.Clear;
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [IPTR($ad000025)]));
    SL.SetText(PC);
    SL.Exchange(Index1, Index2);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[IPTR($ad000002), PC, TAG_END]);
  end;
end;

function TFlowString.Get(Index: Integer): string;
var
  PC: PChar;
begin
  if Assigned(FMuiObject) then
  begin
    SL.Clear;
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [IPTR($ad000025)]));
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
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [IPTR($ad000025)]));
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
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [IPTR($ad000025)]));
    SL.SetText(PC);
    SL.Insert(Index, S);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[IPTR($ad000002), PC, TAG_END]);
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
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[IPTR($ad000002), PC, TAG_END]);
  end;
end;

{TMUIGroupBox}

constructor TMUIGroupBox.Create(var Tags: TTagsList);
begin
  inherited Create(LCLGroupClass, GetTagPtr(Tags));
  MUIDrawing := True;
end;

function TMUIGroupBox.GetCaption: string;
begin
  Result := PChar(GetAttribute(MUIA_FrameTitle));
end;

procedure TMUIGroupBox.SetCaption(const AValue: string);
begin
  SetAttribute([MUIA_FrameTitle, AValue]);
end;



end.

