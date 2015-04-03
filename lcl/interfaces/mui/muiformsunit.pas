unit MuiFormsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, controls, Contnrs, Exec, AmigaDos, Intuition, Utility, Mui,
  Forms, MuiBaseUnit, lclmessageglue, menus, Tagsarray, Math, types, lclType,
  gadtools, strutils;

type

  { TMuiFamily }

  TMuiFamily = class(TMuiObject)
  private
    ChildList: TObjectList;
  protected
    procedure InstallHooks; override;
  public
    Par: TMUIFamily;
    procedure AddHead(AChild: TMuiFamily);
    procedure AddTail(AChild: TMuiFamily);
    procedure Remove(AChild: TMuiFamily);
    function GetList: PMinList;
    destructor Destroy; override;
  end;

  { TMuiMenuItem }

  TMuiMenuItem = class(TMuiFamily)
  private
    MenuChoosed: THook;
    FTitle: string;
    FShortCut: string;
    function GetChecked: Boolean;
    function GetCheckIt: Boolean;
    function GetEnabled: Boolean;
    function GetMenuItem: TMenuItem;
    function GetShortCut: string;
    function GetTitle: string;
    procedure SetChecked(const AValue: Boolean);
    procedure SetCheckIt(const AValue: Boolean);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetShortCut(const AValue: string);
    procedure SetTitle(const AValue: string);
  public
    constructor Create(var tags: TTagsList);
    property Checked: Boolean read GetChecked write SetChecked;
    property CheckIt: Boolean read GetCheckIt write SetCheckIt;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property ShortCut: string read GetShortCut write SetShortCut;
    property Title: string read GetTitle write SetTitle;
    property MenuItem: TMenuItem read GetMenuItem;
  end;

  { TMuiMenu }

  TMuiMenu = class(TMuiFamily)
  private
    FTitle: string;
    function GetEnabled: Boolean;
    function GetTitle: string;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetTitle(const AValue: string);
  public
    constructor Create(var tags: TTagsList);
    destructor Destroy; override;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Title: string read GetTitle write SetTitle;
  end;

  { TMuiMenuStrip }

  TMuiMenuStrip = class(TMuiFamily)
  private
    function GetEnabled: Boolean;
    procedure SetEnabled(const AValue: Boolean);
  public
    constructor Create(var tags: TTagsList);
    Destructor Destroy; override;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  { TMuiWindow }

  TMuiWindow = class(TMUIObject)
  private
    CloseWinHook: THook;
    MoveHook: THook;
    SizeHook: THook;
    FMainMenu: TMuiMenuStrip;
    FSizeable: Boolean;
    FHasMenu: Boolean;
    FInMoveEvent: Boolean;
    FFocusedControl: TMuiObject;
    FBlocksize, FBlockMove: Boolean;
    function GetCaption: string;
    procedure SetCaption(const AValue: string);
  protected
    function GetVisible: Boolean; override;
    procedure SetVisible(const AValue: Boolean); override;
    procedure AddChild(Child: TMUIObject); override;
    procedure RemoveChild(Child: TMUIObject); override;
    procedure SetLeft(ALeft: LongInt); override;
    procedure SetTop(ATop: LongInt); override;
    function GetTop(): Integer; override;
    function GetLeft(): Integer; override;
    procedure SetFocusedControl(AControl: TMUIObject); virtual;
  public    
    constructor Create(var TagList: TTagsList); overload; reintroduce; virtual;
    destructor Destroy; override;
    procedure GetSizes;
    procedure DoMUIDraw(); override;
    function GetClientRect: TRect; override;
    function GetWindowOffset: Types.TPoint; override;
    procedure GetBoundsFromMUI;
    procedure Redraw; override;
    property Caption: string read GetCaption write SetCaption;
    property MainMenu: TMuiMenuStrip read FMainMenu;
    property HasMenu: Boolean read FHasMenu write FHasMenu;
    property Sizeable: Boolean read FSizeable write FSizeable;    
    property FocusedControl: TMUIObject read FFocusedControl write SetFocusedControl;
  end;

  { TMuiGroup }

  TMuiGroup = class(TMuiObject)
  private

  public
    constructor Create(var tags: TTagsList); overload; reintroduce; virtual;
  end;

implementation

{ TMuiWindow }

function LayoutFunc(Hook: PHook; Obj: PObject_; Msg:Pointer): Longint; cdecl;
var
  LMsg: pMUI_LayoutMsg;
  i: LongInt;
  Win: TMuiWindow;
  PasWin: TWinControl;
  Miw, Mih, Maw, Mah: Integer;
  AWin: PWindow;
begin
  LMsg := Msg;
  Result := LongInt(True);
  Win := TMuiWindow(Hook^.h_Data);
  case LMsg^.lm_type of
    MUILM_MINMAX: begin
      if Win.Sizeable then
      begin
        Miw := 100;
        Mih := 100;
        Maw := 10000;
        Mah := 10000;        
        if Assigned(Win.PasObject) then
        begin
          PasWin := TWinControl(Win.PasObject);
          MiW := Max(PasWin.Constraints.MinWidth, 100);
          MiH := Max(PasWin.Constraints.MinHeight, 100);
          if PasWin.Constraints.MaxWidth > 0 then
            MaW := Min(PasWin.Constraints.MaxWidth, 10000);
          if PasWin.Constraints.MaxHeight > 0 then
            MaH := Min(PasWin.Constraints.MaxHeight, 10000);
          LMsg^.lm_MinMax.MinWidth := MiW;
          LMsg^.lm_MinMax.MinHeight := MiH;
          LMsg^.lm_MinMax.MaxWidth :=  MaW;
          LMsg^.lm_MinMax.MaxHeight := MaH;
        end;  
        LMsg^.lm_MinMax.DefWidth := Win.Width;
        LMsg^.lm_MinMax.DefHeight := Win.Height;
      end else
      begin
        AWin := Obj_Window(Win.Obj);
        LMsg^.lm_MinMax.MinWidth := Win.Width - AWin^.BorderRight;
        LMsg^.lm_MinMax.MinHeight := Win.Height - AWin^.BorderBottom;
        LMsg^.lm_MinMax.MaxWidth := Win.Width - AWin^.BorderRight;
        LMsg^.lm_MinMax.MaxHeight := Win.Height - AWin^.BorderBottom;
        LMsg^.lm_MinMax.DefWidth := Win.Width - AWin^.BorderRight;
        LMsg^.lm_MinMax.DefHeight := Win.Height - AWin^.BorderBottom;
      end;    
      TWinControl(Win.PasObject).Realign;
    end;
    MUILM_LAYOUT:
    begin
      Win.GetSizes;
      for i:= 0 to Win.FChilds.Count - 1 do
      begin
        if Win.FChilds.Items[i] is TMUIObject then 
          TMuiObject(Win.FChilds.Items[i]).SetOwnSize;
      end;
    end;
  end;
end;

{ TMuiMenuStrip }

function TMuiMenuStrip.GetEnabled: Boolean;
begin
  Result := LongBool(GetAttribute(MUIA_Menu_Enabled));
end;

procedure TMuiMenuStrip.SetEnabled(const AValue: Boolean);
begin
  SetAttribute([LongInt(MUIA_Menuitem_Enabled), AValue, TAG_END]);
end;

constructor TMuiMenuStrip.Create(var tags: TTagsList);
begin
  inherited Create(MUIC_MenuStrip, GetTagPtr(tags));
  Par := nil;
end;

destructor TMuiMenuStrip.Destroy;
var
  i: Integer;
begin
  FObject := nil;
  inherited;
end;

{ TMuiMenu }

function TMuiMenu.GetEnabled: Boolean;
begin
  Result := LongBool(GetAttribute(MUIA_Menu_Enabled));
end;

function TMuiMenu.GetTitle: string;
begin
  Result := string(PChar(GetAttribute(MUIA_Menuitem_Title)));
end;

procedure TMuiMenu.SetEnabled(const AValue: Boolean);
begin
  SetAttribute([LongInt(MUIA_Menuitem_Enabled), AValue, TAG_END]);
end;

procedure TMuiMenu.SetTitle(const AValue: string);
begin
  FTitle := AValue;
  SetAttribute([LongInt(MUIA_Menuitem_Title), PChar(FTitle), TAG_END]);
end;

constructor TMuiMenu.Create(var tags: TTagsList);
begin
  inherited Create(MUIC_Menu, GetTagPtr(tags));
  Par := nil;
end;

destructor TMuiMenu.Destroy;
begin
  inherited;
end;

{ TMuiMenuItem }

function TMuiMenuItem.GetChecked: Boolean;
begin
  Result := LongBool(GetAttribute(MUIA_Menuitem_Checked));
end;

function TMuiMenuItem.GetCheckIt: Boolean;
begin
   Result := LongBool(GetAttribute(MUIA_Menuitem_CheckIt));
end;

function TMuiMenuItem.GetEnabled: Boolean;
begin
   Result := LongBool(GetAttribute(MUIA_Menuitem_Enabled));
end;

function TMuiMenuItem.GetMenuItem: TMenuItem;
begin
  Result := TMenuItem(TObject(PasObject));
end;

function TMuiMenuItem.GetShortCut: string;
begin
  Result := string(PChar(GetAttribute(MUIA_Menuitem_ShortCut)));
end;

function TMuiMenuItem.GetTitle: string;
begin
  Result := string(PChar(GetAttribute(MUIA_Menuitem_Title)));
end;

procedure TMuiMenuItem.SetChecked(const AValue: Boolean);
var
  I: Integer;
  MI: TMuiMenuItem;
begin
  if AValue and Assigned(MenuItem) and (MenuItem.GroupIndex > 0) then
  begin
    if Assigned(Par) then
    begin
      for i := 0 to Par.ChildList.Count - 1 do
      begin
        if Par.ChildList.Items[i] is TMuiMenuItem then
        begin
          MI := TMuiMenuItem(Par.ChildList.Items[i]);
          if MI.MenuItem.GroupIndex = MenuItem.GroupIndex then
            MI.Checked := False;
        end;
      end;
    end;
  end;
  SetAttribute([LongInt(MUIA_Menuitem_Checked), ifthen(AValue,1,0), TAG_END]);
end;

procedure TMuiMenuItem.SetCheckIt(const AValue: Boolean);
begin
  SetAttribute([LongInt(MUIA_Menuitem_CheckIt), ifthen(AValue,1,0), TAG_END]);
end;

procedure TMuiMenuItem.SetEnabled(const AValue: Boolean);
begin
  //writeln('SetEnabled: ',AValue);
  SetAttribute([LongInt(MUIA_Menuitem_Enabled), ifthen(AValue,1,0), TAG_END]);
  //writeln('getEnabled: ', GetEnabled);
end;

procedure TMuiMenuItem.SetShortCut(const AValue: string);
begin
  FShortCut := AValue;
  //SetAttribute([MUIA_Menuitem_CommandString, 1, LongInt(MUIA_Menuitem_ShortCut), PChar(FShortCut), TAG_END]);
end;

procedure TMuiMenuItem.SetTitle(const AValue: string);
begin
  FTitle := AValue;
  if AValue = '-' then
    SetAttribute([LongInt(MUIA_Menuitem_Title), LongInt(NM_BARLABEL), TAG_END])
  else
    SetAttribute([LongInt(MUIA_Menuitem_Title), LongInt(PChar(FTitle)), TAG_END]);
end;

procedure MenuClickedFunc(Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;
var
  MuiObject: TMuiMenuItem;
  TargetObject: TObject;
  mi: PNewMenu;
begin
  if TObject(Hook^.h_Data) is TMuiMenuItem then
  begin
    MuiObject := TMuiMenuItem(Hook^.h_Data);
    TargetObject := MuiObject.pasobject;
    TMenuItem(TargetObject).Click;
  end;
end;

constructor TMuiMenuItem.Create(var tags: TTagsList);
begin
  inherited Create(MUIC_MenuItem, GetTagPtr(tags));
  Par := nil;
  MenuChoosed.h_Entry := IPTR(@MenuClickedFunc);
  MenuChoosed.h_SubEntry := IPTR(@MenuClickedFunc);
  MenuChoosed.h_Data := Self;
  DoMethod([IPTR(MUIM_Notify), IPTR(MUIA_Menuitem_Trigger), IPTR(MUIV_EveryTime),
      IPTR(MUIV_Notify_Self),
      2,
      IPTR(MUIM_CallHook), IPTR(@MenuChoosed)
      ]);      
end;

{ TMuiFamily }

procedure TMuiFamily.InstallHooks;
begin
  ChildList := TObjectList.Create;
  ChildList.OwnsObjects := False;
end;

destructor TMuiFamily.Destroy;
var
  i: Integer;
  MUIObject: TMuiObject;
begin
  for i := 0 to ChildList.Count - 1 do
  begin
    MUIObject := TMUIObject(ChildList[i]);
    MUIObject.FObject := nil;
    MUIObject.Free;
  end;
  ChildList.Free;
  inherited;
end;

procedure TMuiFamily.AddHead(AChild: TMuiFamily);
begin
  ChildList.Insert(0,AChild);
  DoMethod([IPTR(MUIM_Family_AddHead), IPTR(AChild.Obj)]);
end;

procedure TMuiFamily.AddTail(AChild: TMuiFamily);
begin
  ChildList.Add(AChild);
  DoMethod([IPTR(MUIM_Family_AddTail), IPTR(AChild.Obj)]);
end;

procedure TMuiFamily.Remove(AChild: TMuiFamily);
begin
  ChildList.Remove(AChild);
  DoMethod([IPTR(MUIM_Family_Remove), IPTR(AChild.Obj)]);
end;

function TMuiFamily.GetList: PMinList;
begin
  Result := PMinList(GetAttribute(MUIA_Family_List));
end;

function CloseWinFunc(Hook: PHook; Obj: PObject_; Msg:Pointer): Longint; cdecl;
var
  MuiObject: TMuiWindow;
begin
  if TObject(Hook^.h_Data) is TMuiWindow then
  begin
    MuiObject := TMuiWindow(Hook^.h_Data);
    Result := LCLSendCloseQueryMsg(MuiObject.pasobject);
  end;
end;

function MoveWinFunc(Hook: PHook; Obj: PObject_; Msg:Pointer): Longint; cdecl;
var
  MuiObject: TMuiWindow;
begin
  if TObject(Hook^.h_Data) is TMuiWindow then
  begin
    MuiObject := TMuiWindow(Hook^.h_Data);
    if Assigned(MuiObject.PasObject) and (not TMUIWindow(MUIObject).FBlockMove) then
    begin
      Result := LCLSendMoveMsg(MuiObject.pasobject, MuiObject.Left, MuiObject.Top, Move_Default, False);
    end;
  end;
end;

function SizeWinFunc(Hook: PHook; Obj: PObject_; Msg:Pointer): Longint; cdecl;
var
  MuiObject: TMuiWindow;
begin
  if TObject(Hook^.h_Data) is TMuiWindow then
  begin
    MuiObject := TMuiWindow(Hook^.h_Data);
    if Assigned(MuiObject.PasObject) and (not TMUIWindow(MUIObject).FBlockSize) then
    begin
      Result := LCLSendSizeMsg(MuiObject.pasobject, MuiObject.Width, MuiObject.Height, SIZENORMAL, False);  
    end;
  end;
end;

constructor TMuiWindow.Create(var TagList: TTagsList);
var
  LT: TTagsList;
  GrpTags: TTagsList;
  AltLeft, AltTop, AltHeight, AltWidth: Integer;
begin
  FBlockSize := False;
  FBlockMove := False;
  FFocusedControl := Self;
  FMainMenu := TMuiMenuStrip.Create(LT);
  HasMenu := False;
  FInMoveEvent := False;
  //FGrpObj := MUI_NewObject(MUIC_Group,[LongInt(MUIA_Group_LayoutHook), @LayoutHook, TAG_END]);
  AddTags(GrpTags, [LongInt(MUIA_Group_LayoutHook), @LayoutHook]);
  AddTags(GrpTags, [LongInt(MUIA_Frame), MUIV_Frame_None]);
  AddTags(GrpTags, [
    LongInt(MUIA_InnerLeft), 1,
    LongInt(MUIA_InnerTop), 1,
    LongInt(MUIA_InnerRight), 1,
    LongInt(MUIA_InnerBottom), 1
    ]);
  FGrpObj := NewObjectA(LCLGroupClass, nil, GetTagPtr(GrpTags));
  if Assigned(FGrpObj) then
    Pointer(INST_DATA(LCLGroupClass, Pointer(FGrpObj))^) := Self;
  //
  LayoutHook.h_Entry := IPTR(@LayoutFunc);
  LayoutHook.h_SubEntry := IPTR(@LayoutFunc);
  LayoutHook.h_Data := Self;
  //
  AltLeft := 0;
  AltTop := 0;
  AltWidth := IntuitionBase^.ActiveScreen^.Width;
  AltHeight := IntuitionBase^.ActiveScreen^.Height - IntuitionBase^.ActiveScreen^.BarHeight;
  AddTags(TagList, [MUIA_Window_AltLeftEdge , AltLeft]);
  AddTags(TagList, [MUIA_Window_AltTopEdge , AltTop]);
  AddTags(TagList, [MUIA_Window_AltWidth , AltWidth]);  
  AddTags(TagList, [MUIA_Window_AltHeight , AltHeight]);
  //
  AddTags(TagList, [LongInt(MUIA_Window_Menustrip), FMainMenu.Obj, LongInt(MUIA_Window_RootObject), FGrpObj]);
  inherited Create(MUIC_Window, GetTagPtr(TagList));
  //
  Self.Parent := MUIApp;
  CloseWinHook.h_Entry := IPTR(@CloseWinFunc);
  CloseWinHook.h_SubEntry := 0;
  CloseWinHook.h_Data := Self;
  DoMethod([IPTR(MUIM_Notify), IPTR(MUIA_Window_CloseRequest), LTrue,
      IPTR(FObject), 2,
      IPTR(MUIM_CallHook), IPTR(@CloseWinHook)
      ]);
  // Move Window      
  MoveHook.h_Entry := IPTR(@MoveWinFunc);
  MoveHook.h_SubEntry := 0;
  MoveHook.h_Data := Self;  
  DoMethod([IPTR(MUIM_Notify),
    IPTR(MUIA_Window_LeftEdge), IPTR(MUIV_EveryTime),
    IPTR(MUIV_Notify_Self),
    2,
    IPTR(MUIM_CallHook), IPTR(@MoveHook)
    ]); 
  DoMethod([IPTR(MUIM_Notify),
    IPTR(MUIA_Window_TopEdge), IPTR(MUIV_EveryTime),
    IPTR(MUIV_Notify_Self),
    2,
    IPTR(MUIM_CallHook), IPTR(@MoveHook)
    ]);
  // Resize Window  
  SizeHook.h_Entry := IPTR(@SizeWinFunc);
  SizeHook.h_SubEntry := 0;
  SizeHook.h_Data := Self;        
  DoMethod([IPTR(MUIM_Notify),
    IPTR(MUIA_Window_Width), IPTR(MUIV_EveryTime),
    IPTR(MUIV_Notify_Self),
    2,
    IPTR(MUIM_CallHook), IPTR(@SizeHook)
    ]);   
  DoMethod([IPTR(MUIM_Notify),
    IPTR(MUIA_Window_Height), IPTR(MUIV_EveryTime),
    IPTR(MUIV_Notify_Self),
    2,
    IPTR(MUIM_CallHook), IPTR(@SizeHook)
    ]);
  if MuiApp.MainWin = obj then
    SetAttribute([MUIA_Window_Activate, True]);  
end;

destructor TMuiWindow.Destroy;
begin
  FMainMenu.Free;
  inherited Destroy;
end;

procedure TMuiWindow.GetSizes;
var
  AWin: PWindow;
begin
  Left := GetAttribute(MUIA_Window_LeftEdge);
  Top := GetAttribute(MUIA_Window_TopEdge);
  //
  if not Sizeable then
  begin
    AWin := Obj_Window(Obj);
    Width := GetAttribute(MUIA_Window_Width) - AWin^.BorderRight - 2;
    Height := GetAttribute(MUIA_Window_Height) - AWin^.BorderBottom - 2;
  end else
  begin
    Width := GetAttribute(MUIA_Window_Width);
    Height := GetAttribute(MUIA_Window_Height);
  end;  
  TWinControl(PasObject).SetBounds(Left, Top, Width, Height);
end;

procedure TMuiWindow.Redraw;
begin
  if BlockRedraw then
    Exit;
  CallHook(PHook(OCLASS(FGrpObj)), FGrpObj, [LongInt(MUIM_Group_InitChange)]);
  CallHook(PHook(OCLASS(FGrpObj)), FGrpObj, [LongInt(MUIM_Group_ExitChange)]);
end;

procedure TMuiWindow.DoMUIDraw();
begin
  inherited;
  MUI_Redraw(FGrpobj, MADF_DRAWOBJECT)
end;

function TMuiWindow.GetClientRect: TRect;
begin
  Result := inherited;
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

function TMuiWindow.GetWindowOffset: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

procedure TMuiWindow.SetLeft(ALeft: LongInt);
begin
  FBlockMove := True;
  inherited;
  SetAttribute([LongInt(MUIA_Window_LeftEdge), ALeft]);
  FBlockMove := False;
end;

procedure TMuiWindow.SetTop(ATop: LongInt);
begin
  FBlockMove := True;
  inherited;
  SetAttribute([LongInt(MUIA_Window_TopEdge), ATop]);
  FBlockMove := False;
end;

function TMuiWindow.GetTop(): Integer;
begin
  Result := GetAttribute(MUIA_Window_TopEdge);
end;

function TMuiWindow.GetLeft(): Integer;
begin
  Result := GetAttribute(MUIA_Window_LeftEdge);
end;

function TMuiWindow.GetCaption: string;
begin
  Result := StrPas(PChar(GetAttribute(MUIA_Window_Title)));
end;

function TMuiWindow.GetVisible: Boolean;
begin
  Result := Boolean(GetAttribute(MUIA_Window_Open));
end;

procedure TMuiWindow.SetCaption(const AValue: string);
var
  PC: PChar;
begin
  PC := PChar(AValue);
  SetAttribute([LongInt(MUIA_Window_Title), PC, TAG_END]);
end;

procedure TMuiWindow.SetVisible(const AValue: Boolean);
begin
  SetAttribute([LongInt(MUIA_Window_Open), AValue, TAG_END]);
end;

procedure TMuiWindow.AddChild(Child: TMUIObject);
begin
  CallHook(PHook(OCLASS(FGrpObj)), FGrpObj, [LongInt(MUIM_Group_InitChange)]);
  CallHook(PHook(OCLASS(FGrpObj)), FGrpObj, [OM_ADDMEMBER, Child.obj]);
  CallHook(PHook(OCLASS(FGrpObj)), FGrpObj, [LongInt(MUIM_Group_ExitChange)]);
end;

procedure TMuiWindow.RemoveChild(Child: TMUIObject);
begin
  //writeln('-> window remove child ', HexStr(Child), ' ', HexStr(Child.obj), ' ', HexStr(Self), ' ', HexStr(FGrpObj));
  if Assigned(FGrpObj) and Assigned(Child) and Assigned(child.obj) then
  begin
    CallHook(PHook(OCLASS(FGrpObj)), FGrpObj, [OM_REMMEMBER, Child.obj]);
  end;  
  //writeln('<-window remove child');  
end;

procedure TMuiWindow.GetBoundsFromMUI;
begin
  FLeft := GetAttribute(MUIA_Window_LeftEdge);
  FTop := GetAttribute(MUIA_Window_TopEdge);
  FWidth := GetAttribute(MUIA_Window_Width);
  FHeight := GetAttribute(MUIA_Window_Height);
end;

procedure TMuiWindow.SetFocusedControl(AControl: TMUIObject);
begin
  FFocusedControl := AControl;
  if Assigned(AControl) then
  begin
    SetAttribute([MUIA_Window_Activate, True, MUIA_Window_ActiveObject, AControl.FocusObject]);
  end;  
end;

{ TMuiGroup }

constructor TMuiGroup.Create(var tags: TTagsList);
begin
  inherited Create(MUIC_Group, GetTagPtr(Tags));
end;

end.

