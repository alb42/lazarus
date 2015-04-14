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
    EditHook: THook;
    TextChanged: THook;
    TextDone: THook;
    FNumbersOnly: Boolean;
    FText: PChar;
    function GetText: string;
    procedure SetText(const AValue: string);
    function GetNumbersOnly: Boolean;
    procedure SetNumbersOnly(const AValue: Boolean);
  public
    constructor Create(var Tags: TTagsList); overload; reintroduce; virtual;
    destructor Destroy; override;
    property Text:string read GetText write SetText;
    property NumbersOnly: Boolean read GetNumbersOnly write SetNumbersOnly;
  end;
  
  { TMuiSpinEdit }

  TMuiSpinEdit = class(TMuiArea)
  private
    //EventHooks
    ButtonUpClick: THook;
    ButtonDownClick: THook;
    TextChanged: THook;
    TextDone: THook;
    //
    FMinValue: Double;
    FMaxValue: Double;
    FDecimals: Integer;
    FIncrement: Double;
    //
    FText: PChar;
    BtnUp: PObject_;
    BtnDown: PObject_;
    UpDownPanel: PObject_;
    Edit: PObject_;
  protected  
    function GetNumValue: Double;
    procedure SetNumValue(const AValue: Double);
    //
    procedure SetMinValue(const AValue: Double);
    procedure SetMaxValue(const AValue: Double);
    procedure SetIncrement(const AValue: Double);
    procedure SetDecimals(const AValue: Integer);
    function GetTabStop: boolean; override;
    procedure SetTabStop(const AValue: boolean); override;
    function GetFocusObject: PObject_; override;
  public
    constructor Create(var Tags: TTagsList); overload; reintroduce; virtual;
    destructor Destroy; override;
    property CurValue: Double read GetNumValue write SetNumValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property Increment: Double read FIncrement write SetIncrement;
    property Decimals: Integer read FDecimals write SetDecimals;
  end;
  

  { TMuiCycle }

  TMuiCycle = class(TMuiArea)
  private
    FEditable: Boolean;
    StrObj: PObject_;
    BtnObj: PObject_;
    ActiveItemChanged: THook;
    TextEntered: THook;    
    FStrings: TMuiStrings;
    StringPtrs: TStringPtrs;
    function GetActive: LongInt;
    procedure SetActive(const AValue: LongInt);
    function GetText: string;
    procedure SetText(const AText: string);    
    procedure ChangedItems(Sender: TObject);
  public
    constructor Create(ACaption: PChar; AStrings: TStrings; AEditable: Boolean); overload; reintroduce; virtual;
    Destructor Destroy; override;
    //
    property Strings: TMuiStrings read FStrings write FStrings;
    property Active: LongInt read GetActive write SetActive;
    property Editable: Boolean read FEditable;
    property Text: string read GetText write SetText;
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

  { TMuiListView }

  TMuiListView = class(TMuiArea)
  private
    ListChangeHook: THook;
    DoubleClickHook: THook;
    StrObj: PObject_;
    Texts: array of PChar;
    FStrings: TStringList;
    function GetActive: LongInt;
    procedure SetActive(const AValue: LongInt);
    procedure TextChanged(Sender: TObject);
  public
    constructor Create(AStrings:TStrings; var Tags: TTagsList); overload; reintroduce; virtual;
    destructor Destroy; override;
    procedure SetOwnSize; override;
    property Strings: TStringList read FStrings;
    property Active: LongInt read GetActive write SetActive;
  end;

  { TMUIScrollbar }

  TMUIScrollBar = class(TMuiGroup)
  private
    FMinValue: Integer;
    FMaxValue: Integer;
    FPageSize: Integer;
    FPosition: Integer;
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
    FText: PChar;
    function GetClientRect: TRect; override;
    constructor Create(var Tags: TTagsList); overload; reintroduce; virtual;
    destructor Destroy; override;
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
  //DebugLn('LCL: GetPosition: ' + IntToStr(Result) + ' MinValue: ' + IntToStr(FMinValue));
end;

procedure TMUIScrollBar.SetHoriz(AValue: Boolean);
begin
  SetAttribute([IPTR(MUIA_Group_Horiz), IPTR(AValue)]);
end;

procedure TMUIScrollBar.SetMaxValue(AValue: Integer);
var
  Pos: Integer;
begin
  //debugln('set MaxValue ' + IntToStr(AValue));
  if (AValue = FMaxValue) or (AValue <= FMinValue) then
    Exit;
  Pos := Position;  
  FMaxValue := AValue;
  SetAttribute([IPTR(MUIA_Prop_Entries), (AValue - FMinValue) + FPageSize]);  
  Position := Pos;
end;

procedure TMUIScrollBar.SetMinValue(AValue: Integer);
var
  Pos: Integer;
begin
  if AValue = FMinValue then
    Exit;
  //debugln('set MinValue ' + IntToStr(AValue));
  Pos := Position;
  FMinValue := AValue;
  SetAttribute([IPTR(MUIA_Prop_Entries), (FMaxValue - AValue) + FPageSize]);
  Position := Pos;
end;

procedure TMUIScrollBar.SetPageSize(AValue: Integer);
var
  Pos: Integer;
begin
  if PageSize = FPageSize then
    Exit;
  //debugln('set page size ' + IntToStr(AValue));
  Pos := Position;
  FPageSize := AValue;
  SetAttribute([IPTR(MUIA_Prop_Entries), (FMaxValue - FMinValue) + AValue]);  
  SetAttribute([IPTR(MUIA_Prop_Visible), AValue]);  
  Position := Pos;
end;

procedure TMUIScrollBar.SetPosition(AValue: Integer);
begin
  //DebugLn('LCL: set to '+ IntToStr(AValue) + ' Position is ' + IntToStr(Position) + ' MinValue: ' + IntToStr(FMinValue));
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
  //debugln('--> Scroll hook');
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
  //debugln('<-- Scroll hook');
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
  StrTags: TTagsList;
begin
  FStrings := TStringList.create;
  FStrings.Assign(AStrings);
  StrObj := MUI_NewObjectA(MUIC_List, GetTagPtr(StrTags)); 
  AddTags(Tags, [IPTR(MUIA_Listview_List), StrObj]);
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
var
  i: Integer;
begin
  FStrings.Free;
  for i := 0 to High(Texts) do
    System.FreeMem(Texts[i]);
  inherited Destroy;  
end;

procedure TMuiListView.TextChanged(Sender: TObject);
var
  str: String;
  TagList: TTagsList;
  i: Integer;
begin
  if Assigned(FStrings) then
  begin
    DoMethodObj(StrObj, [MUIM_List_Clear]);
    for i := 0 to High(Texts) do
      System.FreeMem(Texts[i]);
    SetLength(Texts, FStrings.Count + 1);
    for i := 0 to FStrings.Count - 1 do
    begin
      str := FStrings[i] + #0;      
      Texts[i] := System.AllocMem(Length(str));
      Move(str[1], Texts[i]^, Length(str));
    end;
    Texts[FStrings.Count] := nil;
    DoMethodObj(StrObj, [MUIM_List_Insert, IPTR(@(Texts[0])), FStrings.Count, MUIV_List_Insert_Bottom]);
    DoMethodObj(StrObj, [MUIM_List_Redraw, MUIV_List_Redraw_All]);
  end;  
end;

function TMuiListView.GetActive: LongInt;
var
  Res: LongInt;
begin
  Result := 0;
  GetAttr(IPTR(MUIA_List_Active), StrObj, @Res);
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
  SetAttrsA(StrObj, GetTagPtr(TagList));
end;

procedure TMuiListView.SetOwnSize;
begin
  //writeln('Listview set own size: ', FWidth);
  inherited;
  //MUI_Layout(FObject, FLeft, FTop, FHeight, FHeight, 0);  
  //MUI_Layout(StrObj, FLeft, FTop, FWidth, FHeight, 0);
  
  //MUI_Layout(StrObj, FLeft, FTop, FWidth, FHeight, 0);  
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
    if MUIObject is TMUISpinEdit then
    begin
      TMUISpinEdit(MUIObject).CurValue := TMUISpinEdit(MUIObject).CurValue;
    end;    
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

procedure TextEditFunc(Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;
var
  MuiObject: TMuiObject;
begin
  writeln('edit text Event');
  if TObject(Hook^.h_Data) is TMuiObject then
  begin
    //MuiObject := TMuiObject(Hook^.h_Data);
    //SendSimpleMessage(MuiObject.PasObject, CM_TEXTCHANGED);
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
  begin
    FreeMem(FText);
    FText := System.AllocMem(Length(AValue) + 1);
    Move(AValue[1], FText^, Length(AValue));
    SetAttribute([IPTR(MUIA_String_Contents), FText, TAG_END]);
  end;  
end;

constructor TMuiStringEdit.Create(var Tags: TTagsList);
var
  p: Pointer;
begin
  EditHook.h_Entry := IPTR(@TextEditFunc);
  EditHook.h_SubEntry := 0;
  EditHook.h_Data := Self;
  P := @EditHook;
  // Edithook does not work currently
  AddTags(Tags, [
    MUIA_Background, MUII_TextBack,
    MUIA_Frame, MUIV_Frame_String
    //,
    //MUIA_String_EditHook, P,
    //MUIA_String_LonelyEditHook, 1
  ]);
  inherited Create(MUIC_String, GetTagPtr(Tags));
  //
  FNumbersOnly := False;
  FText := System.AllocMem(2048);
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

destructor TMuiStringEdit.Destroy;
begin
  FreeMem(FText);
  inherited;
end;

function TMuiStringEdit.GetNumbersOnly: Boolean;
begin
  Result := FNumbersOnly;
end;

var
  IntegerChars: string = '0123456789-';
  FloatChars: string = '0123456789-.,';  

procedure TMuiStringEdit.SetNumbersOnly(const AValue: Boolean);
var
  StrTxt: String;
begin
  if FNumbersOnly = AValue then
    Exit;
  FNumbersOnly := AValue;
  if FNumbersOnly then
  begin
    StrTxt := GetText;
    SetAttribute([MUIA_String_Integer, StrToIntDef(StrTxt, 0)]);
    SetAttribute([MUIA_String_Accept, PChar(IntegerChars)]);
  end else
  begin
    SetAttribute([MUIA_String_Accept, '']);
  end;  
end;


{ TMuiSpinEdit }

procedure BtnDownClickFunc(Hook: PHook; Obj: PObject_; Msg: Pointer); cdecl;
var
  MuiSpin: TMuiSpinEdit;
begin
  if TObject(Hook^.h_Data) is TMuiSpinEdit then
  begin
    MuiSpin := TMuiSpinEdit(Hook^.h_Data);
    MuiSpin.CurValue := MuiSpin.CurValue - MuiSpin.Increment;
  end;
end;

procedure BtnUpClickFunc(Hook: PHook; Obj: PObject_; Msg: Pointer); cdecl;
var
  MuiSpin: TMuiSpinEdit;
begin
  if TObject(Hook^.h_Data) is TMuiSpinEdit then
  begin
    MuiSpin := TMuiSpinEdit(Hook^.h_Data);
    MuiSpin.CurValue := MuiSpin.CurValue + MuiSpin.Increment;
  end;
end;

function TMuiSpinEdit.GetNumValue: Double;
var
  PC: PChar;
  strValue: string;
begin
  Result := 0;
  if Assigned(Edit) then
  begin
    PC := PChar(GetAttObj(Edit, MUIA_String_Contents));
    if Assigned(PC) then
    begin
      // we accept , and . :-P but set it to the system set DECIMALSEPARATOR
      strValue := StringReplace(string(PC), ',', DECIMALSEPARATOR, [rfReplaceAll]);
      strValue := StringReplace(strValue, '.', DECIMALSEPARATOR, [rfReplaceAll]);
      Result := StrToFloatDef(string(PC), 0);
      Result := Min(FMaxValue, Max(FMinValue, Result));
    end;  
  end;    
end;

procedure TMuiSpinEdit.SetNumValue(const AValue: Double);
var
  StrValue: String;
  Val: Double;
begin
  Val := Min(FMaxValue, Max(FMinValue, AValue));
  StrValue := FloatToStrF(Val, ffFixed, 8, FDecimals);
  FillChar(FText^, Length(StrValue) + 2, 0);
  Move(StrValue[1], FText^, Length(strValue));
  SetAttObj(Edit, [MUIA_String_Contents, PChar(FText)]);
  SetAttObj(Edit, [MUIA_String_BufferPos, Length(FText)]);
end;

constructor TMuiSpinEdit.Create(var Tags: TTagsList);
var
  GrpTags: TTagsList;
  BtnUpTags: TTagsList;
  BtnDownTags: TTagsList; 
  BtnGroupTags: TTagsList;
  EditTags: TTagsList;
begin
  FIncrement := 1;
  FMinValue := -1e308;
  FMaxValue := 1e308;
  FDecimals := 2;
  // BUTTON DOWN  ##################################
  FText := System.AllocMem(100);
  AddTags(BtnUpTags, [
    IPTR(MUIA_InputMode), IPTR(MUIV_InputMode_RelVerify),
    IPTR(MUIA_ShowSelState), IPTR(True),
    //MUIA_Frame, MUIV_Frame_ImageButton,
    MUIA_InnerLeft , 0, MUIA_InnerRight , 0,
    MUIA_InnerTop , 0, MUIA_InnerBottom , 0,    
    IPTR(MUIA_Image_Spec), IPTR(MUII_ArrowUp)
    ]);
  btnUp := MUI_NewObjectA(MUIC_Image, GetTagPtr(BtnUpTags));
  
  ButtonUpClick.h_Entry := IPTR(@BtnUpClickFunc);
  ButtonUpClick.h_SubEntry := 0;
  ButtonUpClick.h_Data := Self;
  
  DoMethodObj(btnUp, [IPTR(MUIM_Notify), IPTR(MUIA_Timer), IPTR(MUIV_EveryTime),
    IPTR(MUIV_Notify_Self), 2, IPTR(MUIM_CallHook), IPTR(@ButtonUpClick)]);  
  
  // BUTTON UP #######################################
  AddTags(BtnDownTags, [
    IPTR(MUIA_InputMode), IPTR(MUIV_InputMode_RelVerify),
    IPTR(MUIA_ShowSelState), IPTR(True),
    //MUIA_Frame, MUIV_Frame_ImageButton,
    MUIA_InnerLeft , 0, MUIA_InnerRight , 0,
    MUIA_InnerTop , 0, MUIA_InnerBottom , 0,
    IPTR(MUIA_Image_Spec), IPTR(MUII_ArrowDown)
    ]);
  btndown := MUI_NewObjectA(MUIC_Image, GetTagPtr(BtnDownTags));
  
  ButtonDownClick.h_Entry := IPTR(@BtnDownClickFunc);
  ButtonDownClick.h_SubEntry := 0;
  ButtonDownClick.h_Data := Self;
  
  DoMethodObj(btnDown, [IPTR(MUIM_Notify), IPTR(MUIA_Timer), IPTR(MUIV_EveryTime),
    IPTR(MUIV_Notify_Self), 2, IPTR(MUIM_CallHook), IPTR(@ButtonDownClick)]);  
  
  // BUTTON GROUP ####################################
  AddTags(BtnGroupTags, [
    MUIA_Background, MUII_TextBack,
    MUIA_Group_Child, BtnUp,
    MUIA_Group_Child, BtnDown,
    MUIA_InnerLeft , 0, MUIA_InnerRight , 0,
    MUIA_InnerTop , 0, MUIA_InnerBottom , 0,
    MUIA_Group_Spacing, 0,
    MUIA_Group_Horiz, False
    ]);
  
  UpDownPanel := MUI_NewObjectA(MUIC_Group, GetTagPtr(BtnGroupTags));
  //
  // Editor ###########################################
  AddTags(EditTags, [
    MUIA_String_Format, MUIV_String_Format_Right, 
    MUIA_Background, MUII_TextBack,
    MUIA_Frame, MUIV_Frame_String,
    MUIA_Group_Spacing, 0,
    MUIA_String_MaxLen, 100,
    MUIA_String_Accept, PChar(FloatChars)
  ]);
  Edit := MUI_NewObjectA(MUIC_String, GetTagPtr(EditTags));
  //
  // Group ############################################# 
  AddTags(GrpTags, [
    MUIA_InnerLeft , 0, MUIA_InnerRight , 0,
    MUIA_InnerTop , 0, MUIA_InnerBottom , 0,
    MUIA_Group_Spacing, 0,
    MUIA_Group_Child, Edit,
    MUIA_Group_Child, UpDownPanel,
    MUIA_Frame, MUIV_Frame_string,
    MUIA_Group_Horiz, True
    ]); 
  inherited Create(MUIC_Group, GetTagPtr(GrpTags));
  //
  // connect some events
  TextChanged.h_Entry := IPTR(@TextChangedFunc);
  TextChanged.h_SubEntry := 0;
  TextChanged.h_Data := Self;
  DoMethodObj(Edit, [IPTR(MUIM_Notify), IPTR(MUIA_String_Contents), IPTR(MUIV_EveryTime),
      IPTR(MUIV_Notify_Self), 2,
      IPTR(MUIM_CallHook), IPTR(@TextChanged)
      ]);
  TextDone.h_Entry := IPTR(@TextDoneFunc);
  TextDone.h_SubEntry := 0;
  TextDone.h_Data := Self;
  DoMethodObj(Edit, [IPTR(MUIM_Notify), IPTR(MUIA_String_Acknowledge), IPTR(MUIV_EveryTime),
      IPTR(MUIV_Notify_Self), 2,
      IPTR(MUIM_CallHook), IPTR(@TextDone)
      ]);
end;

destructor TMuiSpinEdit.Destroy;
begin
  System.FreeMem(FText);
  inherited;
end;

procedure TMuiSpinEdit.SetMinValue(const AValue: Double);
begin
  if FMinValue = AValue then
    Exit;
  FMinValue := AValue;
  if CurValue < FMinValue then
    CurValue := FMinValue;  
end;

procedure TMuiSpinEdit.SetMaxValue(const AValue: Double);
begin
  if FMaxValue = AValue then
    Exit;
  FMaxValue := AValue;
  if CurValue > FMaxValue then
    CurValue := FMaxValue;
end;

procedure TMuiSpinEdit.SetIncrement(const AValue: Double);
begin
  FIncrement := AValue;
end;

procedure TMuiSpinEdit.SetDecimals(const AValue: Integer);
begin
  if FDecimals = AValue then
    Exit;
  if FDecimals = 0 then
  begin
    if AValue <> 0 then
      SetAttObj(Edit, [MUIA_String_Accept, PChar(FloatChars)]);
  end else
  begin
    if AValue = 0 then
      SetAttObj(Edit, [MUIA_String_Accept, PChar(IntegerChars)]);
  end;
  FDecimals := AValue;
  CurValue := CurValue;  
end;

function TMuiSpinEdit.GetTabStop: boolean;
begin
  Result := GetAttObj(Edit, IPTR(MUIA_CycleChain)) <> 0;
end;

procedure TMuiSpinEdit.SetTabStop(const AValue: boolean);
var
  Val: Integer;
begin
  if AValue then
    Val := 1
  else
    Val := 0;  
  SetAttObj(Edit, [IPTR(MUIA_CycleChain), IPTR(Val)]);
  SetAttribute([IPTR(MUIA_CycleChain), 0]);
end;

function TMuiSpinEdit.GetFocusObject: PObject_;
begin
  Result := Edit;
end;


{ TMuiCycle }

procedure ActiveItemChangedFunc(Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;
var
  MuiObject: TMuiCycle;
  ItemIndex: Integer;
begin
  if TObject(Hook^.h_Data) is TMuiCycle then
  begin
    MuiObject := TMuiCycle(Hook^.h_Data);
    ItemIndex := MuiObject.Active;
    LCLSendChangedMsg(MuiObject.PasObject, ItemIndex);
  end;
end;

procedure TextEnteredFunc(Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;
var
  MuiObject: TMuiCycle;
  ItemIndex: Integer;
begin
  if TObject(Hook^.h_Data) is TMuiCycle then
  begin
    MuiObject := TMuiCycle(Hook^.h_Data);
    ItemIndex := MuiObject.Active;
    if MuiObject.Editable then
    begin
      if ItemIndex < 0 then
        Exit;      
    end;
    LCLSendChangedMsg(MuiObject.PasObject, ItemIndex);
  end;
end;

function TMuiCycle.GetActive: LongInt;
var
  str: string;
begin
  if FEditable then
  begin
    str := PChar(GetAttObj(StrObj, MUIA_String_Contents));    
    Result := FStrings.IndexOf(str);
  end else
  begin
    Result := LongInt(GetAttribute(MUIA_Cycle_Active));
  end;  
end;

procedure TMuiCycle.SetActive(const AValue: LongInt);
begin
  if FEditable then
  begin
    SetAttObj(StrObj, [MUIA_String_Contents, PChar(FStrings[AValue])]);
  end else
  begin
    SetAttribute([IPTR(MUIA_Cycle_Active), AValue, TAG_END]);
  end;
end;


function TMuiCycle.GetText: string;
var
  Idx: Integer;
begin
  Result := '';
  if FEditable then
  begin
    Result := PChar(GetAttObj(StrObj, MUIA_String_Contents));  
  end else
  begin
    Idx := GetActive;
    if (Idx >= 0) and (Idx < FStrings.Count) then
    begin
      Result := FStrings[Idx];
    end;
  end;
end;

procedure TMuiCycle.SetText(const AText: string);
var
  Idx: Integer;
begin
  if FEditable then
  begin
    SetAttObj(StrObj, [MUIA_String_Contents, PChar(AText)]);  
  end else
  begin
    Idx := FStrings.IndexOf(AText);
    if (Idx >= 0) and (Idx < FStrings.Count) then
      SetActive(Idx);
  end;
end;


procedure TMuiCycle.ChangedItems(Sender: TObject);
begin
  // on change -> recreate the combobox (items only set on initialization in MUI)
  RecreateWnd(TWinControl(PasObject));
end;

constructor TMuiCycle.Create(ACaption: PChar; AStrings: TStrings; AEditable: Boolean);
var
  str: string;
  Len: Integer;
  i: LongInt;
  ListTags: TTagsList;
  BtnTags: TTagsList;
  StrTags: TTagsList;
begin
  FEditable := AEditable;
  //
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
  if FEditable then
  begin
    AddTags(BtnTags, [
      IPTR(MUIA_InputMode), IPTR(MUIV_InputMode_RelVerify),
      IPTR(MUIA_ShowSelState), IPTR(True),
      IPTR(MUIA_Frame), IPTR(MUIV_Frame_Button),
      IPTR(MUIA_Image_Spec), IPTR(MUII_PopUp)    
    ]);
    BtnObj := MUI_NewObjectA(MUIC_Image, GetTagPtr(BtnTags));
    
    AddTags(StrTags, [
      IPTR(MUIA_Frame), IPTR(MUIV_Frame_String)
    ]);
    StrObj := MUI_NewObjectA(MUIC_String, GetTagPtr(StrTags));
    
    AddTags(ListTags, [
      MUIA_Popstring_String, StrObj,
      MUIA_Popstring_Button, BtnObj,
      MUIA_PopList_Array, IPTR(@(StringPtrs[0]))
      ]);
    
    inherited Create(MUIC_PopList, GetTagPtr(ListTags));    
    
    
    TextEntered.h_Entry := IPTR(@TextEnteredFunc);
    TextEntered.h_SubEntry := 0;
    TextEntered.h_Data := Self;
    DoMethodObj(StrObj, [IPTR(MUIM_Notify), IPTR(MUIA_String_Contents), IPTR(MUIV_EveryTime),
        IPTR(MUIV_Notify_Self), 2,
        IPTR(MUIM_CallHook), IPTR(@TextEntered)
        ]);

    ActiveItemChanged.h_Entry := IPTR(@ActiveItemChangedFunc);
    ActiveItemChanged.h_SubEntry := 0;
    ActiveItemChanged.h_Data := Self;    
    DoMethodObj(StrObj, [IPTR(MUIM_Notify), IPTR(MUIA_String_Acknowledge), IPTR(MUIV_EveryTime),
        IPTR(MUIV_Notify_Self), 2,
        IPTR(MUIM_CallHook), IPTR(@ActiveItemChanged)
        ]);
  end else
  begin
    inherited Create(MUIO_Cycle, [ACaption, @(StringPtrs[0])]);
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
  FStrings.OnChange := @ChangedItems;   
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
    SL.BeginUpdate;
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [IPTR($ad000025)]));
    SL.SetText(PC);
    Result := SL.Count;
    SL.EndUpdate;
  end;
end;

function TFlowString.Add(const S: String): Integer;
var
  PC: Pchar;
begin
  if Assigned(FMuiObject) then
  begin
    SL.BeginUpdate;
    //writeln('add: ', s);
    //PC := PChar(#10 + S);
    //CallHookPkt(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [LongInt($ad000026), PC, 2]);
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [IPTR($ad000025)]));
    SL.SetText(PC);
    Result := SL.Add(S);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[IPTR($ad000002), PC, TAG_END]);
    SL.EndUpdate;
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
    SL.BeginUpdate;
    SL.Clear;
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [IPTR($ad000025)]));
    SL.SetText(PC);
    SL.Delete(Index);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[IPTR($ad000002), PC, TAG_END]);
    SL.EndUpdate;
  end;
end;

procedure TFlowString.Exchange(Index1: Integer; Index2: Integer);
var
  PC: PChar;
begin
  if Assigned(FMuiObject) then
  begin
    SL.BeginUpdate;
    SL.Clear;
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [IPTR($ad000025)]));
    SL.SetText(PC);
    SL.Exchange(Index1, Index2);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[IPTR($ad000002), PC, TAG_END]);
    SL.EndUpdate;
  end;
end;

function TFlowString.Get(Index: Integer): string;
var
  PC: PChar;
begin
  if Assigned(FMuiObject) then
  begin
    SL.BeginUpdate;
    SL.Clear;
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [IPTR($ad000025)]));
    SL.SetText(PC);
    Result := SL.strings[Index];
    SL.EndUpdate;
  end;
end;

procedure TFlowString.Put(Index: Integer; const S: string);
var
  PC: PChar;
begin
  inherited Put(Index, S);
  if Assigned(FMuiObject) then
  begin
    SL.BeginUpdate;
    SL.Clear;
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [IPTR($ad000025)]));
    SL.SetText(PC);
    SL.strings[Index] := S;
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[LongInt($ad000002), PC, TAG_END]);
    SL.EndUpdate;
  end;
end;

procedure TFlowString.Insert(Index: Integer; const S: String);
var
  PC: PChar;
begin
  if Assigned(FMuiObject) then
  begin
    SL.BeginUpdate;
    SL.Clear;
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [IPTR($ad000025)]));
    SL.SetText(PC);
    SL.Insert(Index, S);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[IPTR($ad000002), PC, TAG_END]);
    SL.EndUpdate;
  end;
end;

procedure TFlowString.LoadFromFile(const FileName: String);
var
  PC: PChar;
begin
  if Assigned(FMuiObject) then
  begin
    SL.BeginUpdate;
    SL.Clear;
    SL.LoadFromFile(FileName);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[IPTR($ad000002), PC, TAG_END]);
    SL.EndUpdate;
  end;
end;

{TMUIGroupBox}

constructor TMUIGroupBox.Create(var Tags: TTagsList);
begin
  inherited Create(LCLGroupClass, GetTagPtr(Tags));
  MUIDrawing := True;
  FText := nil;
end;

destructor TMUIGroupBox.Destroy;
begin
  FreeMem(FText);
  inherited;
end;

function TMUIGroupBox.GetCaption: string;
begin
  Result := FText;//PChar(GetAttribute(MUIA_FrameTitle));
end;

procedure TMUIGroupBox.SetCaption(const AValue: string);
begin
  //Set is not supported
  //SetAttribute([MUIA_FrameTitle, AValue]);
end;

function TMUIGroupBox.GetClientRect: TRect;
begin
  //writeln(TGroupBox(pasobject).caption);
  Result.Left := GetAttribute(MUIA_InnerLeft);
  Result.Top := GetAttribute(MUIA_InnerTop);
  Result.Right:= FWidth - (GetAttribute(MUIA_InnerRight) + Result.Left + 1);
  Result.Bottom := FHeight - (GetAttribute(MUIA_InnerBottom) + Result.Top + 1);
  //writeln('get clientrect ', Result.Top, ' ', Result.Bottom);
  //writeln('               ', Result.Left, ' ', Result.Right);
end;



end.

