{
 *****************************************************************************
 *                             MuiFormsUnit.pas                              *
 *                              --------------                               *
 *                    MUI Windows and related things                         *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit MuiFormsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Contnrs, Exec, AmigaDos, Intuition, Utility, Mui,
  Forms, MuiBaseUnit, LCLMessageGlue, Menus, Math, Types, LCLType,
  StrUtils, tagsparamshelper, muiglobal;

const
  NM_Barlabel = -1;

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
    FTitle: string;
    FShortCut: string;
  protected
    function GetChecked: Boolean;
    function GetCheckIt: Boolean;
    function GetEnabled: Boolean; override;
    function GetMenuItem: TMenuItem;
    function GetShortCut: string;
    function GetTitle: string;
    procedure SetChecked(const AValue: Boolean);
    procedure SetCheckIt(const AValue: Boolean);
    procedure SetEnabled(const AValue: Boolean); override;
    procedure SetShortCut(const AValue: string);
    procedure SetTitle(const AValue: string);
  public
    constructor Create(const Tags: TATagList); reintroduce overload virtual;
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
  protected
    function GetEnabled: Boolean;  override;
    function GetTitle: string;
    procedure SetEnabled(const AValue: Boolean); override;
    procedure SetTitle(const AValue: string);
  public
    constructor Create(const Tags: TATagList); reintroduce overload virtual;
    destructor Destroy; override;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Title: string read GetTitle write SetTitle;
  end;

  { TMuiMenuStrip }

  TMuiMenuStrip = class(TMuiFamily)
  protected
    function GetEnabled: Boolean; override;
    procedure SetEnabled(const AValue: Boolean); override;
  public
    constructor Create(const Tags: TATagList);
    Destructor Destroy; override;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  { TMuiWindow }

  TMuiWindow = class(TMUIObject)
  private
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
    procedure AddChild(ChildObj: PObject_); override;
    procedure RemoveChild(ChildObj: PObject_); override;
    procedure SetLeft(ALeft: LongInt); override;
    procedure SetTop(ATop: LongInt); override;
    function GetTop(): Integer; override;
    function GetLeft(): Integer; override;
    procedure SetFocusedControl(AControl: TMUIObject); virtual;
    procedure InstallHooks; override;
  public
    constructor Create(const TagList: TATagList); overload; reintroduce; virtual;
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
    constructor Create(const TagList: TATagList); overload; reintroduce; virtual;
  end;

implementation

{ TMuiWindow }

function LayoutFunc(Hook: PHook; Obj: PObject_; Msg:Pointer): Longint;
var
  LMsg: pMUI_LayoutMsg;
  i: LongInt;
  Win: TMuiWindow;
  PasWin: TWinControl;
  Miw, Mih, Maw, Mah: Integer;
begin
  LMsg := Msg;
  Result := LongInt(True);
  Win := TMuiWindow(Hook^.h_Data);
  case LMsg^.lm_type of
    MUILM_MINMAX: begin
      if Win.Sizeable then
      begin
        Miw := 100;
        Mih := 20;
        Maw := 10000;
        Mah := 10000;
        if Assigned(Win.PasObject) then
        begin
          PasWin := TWinControl(Win.PasObject);
          MiW := Max(PasWin.Constraints.MinWidth, 100);
          MiH := Max(PasWin.Constraints.MinHeight, 20);
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
        LMsg^.lm_MinMax.MinWidth := Win.Width;
        LMsg^.lm_MinMax.MinHeight := Win.Height;
        LMsg^.lm_MinMax.MaxWidth := Win.Width;
        LMsg^.lm_MinMax.MaxHeight := Win.Height;
        LMsg^.lm_MinMax.DefWidth := Win.Width;
        LMsg^.lm_MinMax.DefHeight := Win.Height;
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
  SetAttribute(MUIA_Menuitem_Enabled, AValue);
end;

constructor TMuiMenuStrip.Create(const Tags: TATagList);
begin
  inherited Create(MUIC_MenuStrip, Tags);
  Par := nil;
end;

destructor TMuiMenuStrip.Destroy;
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
  SetAttribute(MUIA_Menuitem_Enabled, AValue);
end;

procedure TMuiMenu.SetTitle(const AValue: string);
begin
  FTitle := AValue;
  SetAttribute(MUIA_Menuitem_Title, PChar(FTitle));
end;

constructor TMuiMenu.Create(const Tags: TATagList);
begin
  inherited Create(MUIC_Menu, tags);
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
  SetAttribute(MUIA_Menuitem_Checked, AValue);
end;

procedure TMuiMenuItem.SetCheckIt(const AValue: Boolean);
begin
  SetAttribute(MUIA_Menuitem_CheckIt, AValue);
end;

procedure TMuiMenuItem.SetEnabled(const AValue: Boolean);
begin
  //writeln('SetEnabled: ',AValue);
  SetAttribute(MUIA_Menuitem_Enabled, AValue);
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
    SetAttribute(MUIA_Menuitem_Title, NativeUInt(NM_BARLABEL))
  else
    SetAttribute(MUIA_Menuitem_Title, PChar(FTitle));
end;

function MenuClickedFunc(Hook: PHook; Obj: PObject_; Msg:Pointer): LongInt;
var
  MuiObject: TMuiMenuItem;
  TargetObject: TObject;
begin
  Result := 0;
  if TObject(Hook^.h_Data) is TMuiMenuItem then
  begin
    MuiObject := TMuiMenuItem(Hook^.h_Data);
    TargetObject := MuiObject.pasobject;
    TMenuItem(TargetObject).Click;
  end;
end;

constructor TMuiMenuItem.Create(const Tags: TATagList);
begin
  inherited Create(MUIC_MenuItem, Tags);
  Par := nil;
  ConnectHook(MUIA_Menuitem_Trigger, LongWord(MUIV_EveryTime), @MenuClickedFunc);
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
  DoMethod([NativeUInt(MUIM_Family_AddHead), NativeUInt(AChild.Obj)]);
end;

procedure TMuiFamily.AddTail(AChild: TMuiFamily);
begin
  ChildList.Add(AChild);
  DoMethod([NativeUInt(MUIM_Family_AddTail), NativeUInt(AChild.Obj)]);
end;

procedure TMuiFamily.Remove(AChild: TMuiFamily);
begin
  ChildList.Remove(AChild);
  DoMethod([NativeUInt(MUIM_Family_Remove), NativeUInt(AChild.Obj)]);
end;

function TMuiFamily.GetList: PMinList;
begin
  Result := PMinList(GetAttribute(MUIA_Family_List));
end;

function CloseWinFunc(Hook: PHook; Obj: PObject_; Msg:Pointer): Longint;
var
  MuiObject: TMuiWindow;
begin
  if TObject(Hook^.h_Data) is TMuiWindow then
  begin
    MuiObject := TMuiWindow(Hook^.h_Data);
    Result := LCLSendCloseQueryMsg(MuiObject.pasobject);
  end;
end;

function MoveWinFunc(Hook: PHook; Obj: PObject_; Msg:Pointer): Longint;
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

function SizeWinFunc(Hook: PHook; Obj: PObject_; Msg:Pointer): Longint;
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

constructor TMuiWindow.Create(const TagList: TATagList);
var
  LT: TATagList;
  GrpTags: TATagList;
  AltLeft, AltTop, AltHeight, AltWidth: Integer;
begin
  FBlockSize := False;
  FBlockMove := False;
  FFocusedControl := Self;
  LT.Clear;
  FMainMenu := TMuiMenuStrip.Create(LT);
  HasMenu := False;
  FInMoveEvent := False;
  //FGrpObj := MUI_NewObject(MUIC_Group,[LongInt(MUIA_Group_LayoutHook), @LayoutHook, TAG_END]);
  //
  SetHook(LayoutHook, @LayoutFunc, self);
  //
  GrpTags.AddTags([
    MUIA_Group_LayoutHook, NativeUInt(@LayoutHook),
    MUIA_Frame, MUIV_Frame_None,
    MUIA_InnerLeft, 1,
    MUIA_InnerTop, 1,
    MUIA_InnerRight, 1,
    MUIA_InnerBottom, 1
    ]);
  {$ifdef MorphOS}
  FGrpObj := MUI_NewObjectA(MUIC_Group,GrpTags);
  {$else}
  FGrpObj := NewObjectA(LCLGroupClass, nil, GrpTags);
  if Assigned(FGrpObj) then
    Pointer(INST_DATA(LCLGroupClass, Pointer(FGrpObj))^) := Self;
  {$endif}
  //
  AltLeft := 0;
  AltTop := 0;
  AltWidth := IntuitionBase^.ActiveScreen^.Width;
  AltHeight := IntuitionBase^.ActiveScreen^.Height - IntuitionBase^.ActiveScreen^.BarHeight;
  TagList.AddTags([
    MUIA_Window_AltLeftEdge , AltLeft,
    MUIA_Window_AltTopEdge , AltTop,
    MUIA_Window_AltWidth , AltWidth,
    MUIA_Window_AltHeight , AltHeight,
    MUIA_Window_Menustrip, NativeUInt(FMainMenu.Obj),
    MUIA_Window_RootObject, NativeUInt(FGrpObj)
    ]);
  inherited Create(MUIC_Window, TagList);
  //
  Self.Parent := MUIApp;
  if MuiApp.MainWin = obj then
    SetAttribute(MUIA_Window_Activate, True);
end;

destructor TMuiWindow.Destroy;
begin
  FMainMenu.Free;
  inherited Destroy;
end;

procedure TMuiWindow.InstallHooks;
begin
  inherited;
  ConnectHook(MUIA_Window_CloseRequest, LongWord(LTrue), @CloseWinFunc);
  // Move Window
  ConnectHook(MUIA_Window_LeftEdge, LongWord(MUIV_EveryTime), @MoveWinFunc);
  ConnectHook(MUIA_Window_TopEdge, LongWord(MUIV_EveryTime), @MoveWinFunc);
  // Resize Window
  ConnectHook(MUIA_Window_Width, LongWord(MUIV_EveryTime), @SizeWinFunc);
  ConnectHook(MUIA_Window_Height, LongWord(MUIV_EveryTime), @SizeWinFunc);
end;

procedure TMuiWindow.GetSizes;
begin
  Left := GetAttribute(MUIA_Window_LeftEdge);
  Top := GetAttribute(MUIA_Window_TopEdge);
  //
  if not Sizeable then
  begin
    Width := GetAttribute(MUIA_Window_Width) - 2;
    Height := GetAttribute(MUIA_Window_Height) - 2;
  end else
  begin
    Width := GetAttribute(MUIA_Window_Width);
    Height := GetAttribute(MUIA_Window_Height);
  end;
  TWinControl(PasObject).SetBounds(Left, Top, Width, Height);
end;

procedure TMuiWindow.Redraw;
var
  Msg: LongWord;
begin
  if BlockRedraw then
    Exit;
  DoMethodObj(FGrpObj, [MUIM_Group_InitChange]);
  DoMethodObj(FGrpObj, [MUIM_Group_ExitChange]);
end;

procedure TMuiWindow.DoMUIDraw();
begin
  inherited;
  if Assigned(FGrpObj) then
    MUI_Redraw(FGrpobj, MADF_DRAWOBJECT);
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
  SetAttribute(MUIA_Window_LeftEdge, ALeft);
  FBlockMove := False;
end;

procedure TMuiWindow.SetTop(ATop: LongInt);
begin
  FBlockMove := True;
  inherited;
  SetAttribute(MUIA_Window_TopEdge, ATop);
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
  inherited;
  //Result := FCaption;
  //Result := StrPas(PChar(GetAttribute(MUIA_Window_Title)));
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
  SetAttribute(MUIA_Window_Title, PC);
end;

procedure TMuiWindow.SetVisible(const AValue: Boolean);
begin
  SetAttribute(MUIA_Window_Open, AValue);
end;

procedure TMuiWindow.AddChild(ChildObj: PObject_);
begin
  if Assigned(FGrpObj) and Assigned(ChildObj) then
  begin
    DoMethodObj(FGrpObj, [MUIM_Group_InitChange]);
    DoMethodObj(FGrpObj, [OM_ADDMEMBER, PtrUInt(ChildObj)]);
    DoMethodObj(FGrpObj, [MUIM_Group_ExitChange]);
  end;
end;

procedure TMuiWindow.RemoveChild(ChildObj: PObject_);
begin
  //writeln('-> window remove child ', HexStr(Child), ' ', HexStr(Child.obj), ' ', HexStr(Self), ' ', HexStr(FGrpObj));
  if Assigned(FGrpObj) and Assigned(ChildObj) then
  begin
    //DoMethodObj(FGrpObj, [MUIM_Group_InitChange]);
    DoMethodObj(FGrpObj, [OM_REMMEMBER, PtrUInt(ChildObj)]);
    //DoMethodObj(FGrpObj, [MUIM_Group_ExitChange]);
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
    SetAttribute([MUIA_Window_Activate, TagTrue, MUIA_Window_ActiveObject, NativeUInt(AControl.FocusObject)]);
  end;
end;

{ TMuiGroup }

constructor TMuiGroup.Create(const TagList: TATagList);
begin
  inherited Create(MUIC_Group, TagList);
end;

end.

