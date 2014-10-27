unit MuiFormsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, controls, Contnrs, Exec, AmigaDos, Intuition, Utility, Mui,
  Forms, MuiBaseUnit, lclmessageglue, menus, Tagsarray, Math;

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
    FMainMenu: TMuiMenuStrip;
    FSizeable: Boolean;
    function GetCaption: string;
    procedure SetCaption(const AValue: string);
  protected
    function GetVisible: Boolean; override;
    procedure SetVisible(const AValue: Boolean); override;
    procedure AddChild(Child: TMUIObject); override;
    procedure RemoveChild(Child: TMUIObject); override;
    procedure SetLeft(ALeft: LongInt); override;
    procedure SetTop(ATop: LongInt); override;
  public
    constructor Create(var TagList: TTagsList); overload; reintroduce; virtual;
    destructor Destroy; override;
    procedure GetSizes;
    procedure DoMUIDraw(); override;
    function GetClientRect: TRect; override;
    procedure Redraw; override;
    property Caption: string read GetCaption write SetCaption;
    property MainMenu: TMuiMenuStrip read FMainMenu;
    property Sizeable: Boolean read FSizeable write FSizeable;
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
begin
  LMsg := Msg;
  Result := LongInt(True);
  Win := TMuiWindow(Hook^.h_Data);
  case LMsg^.lm_type of
    MUILM_MINMAX: begin
      if Win.Sizeable then
      begin
        LMsg^.lm_MinMax.MinWidth := 100;
        LMsg^.lm_MinMax.MinHeight := 100;
        LMsg^.lm_MinMax.MaxWidth := 10000;
        LMsg^.lm_MinMax.MaxHeight := 10000;
      end else
      begin
        LMsg^.lm_MinMax.MinWidth := Win.Width;
        LMsg^.lm_MinMax.MinHeight := Win.Height;
        LMsg^.lm_MinMax.MaxWidth := Win.Width;
        LMsg^.lm_MinMax.MaxHeight := Win.Height;
      end;    
      LMsg^.lm_MinMax.DefWidth :=  Win.Width;
      LMsg^.lm_MinMax.DefHeight := Win.Height;
      TWinControl(Win.PasObject).Realign;
    end;
    MUILM_LAYOUT:
    begin
      Win.GetSizes;
      for i:= 0 to Win.FObjects.Count - 1 do
      begin
        TMuiObject(Win.FObjects.Items[i]).SetOwnSize;
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
begin
  //
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
  //SetAttribute([LongInt(MUIA_Menuitem_ShortCut), PChar(AValue), TAG_END]);
end;

procedure TMuiMenuItem.SetTitle(const AValue: string);
begin
  FTitle := AValue;
  SetAttribute([LongInt(MUIA_Menuitem_Title), LongInt(PChar(FTitle)), TAG_END]);
end;

procedure MenuClickedFunc(Hook: PHook; Obj: PObject_; Msg:Pointer); cdecl;
var
  MuiObject: TMuiMenuItem;
  TargetObject: TObject;
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
      ])
end;

{ TMuiFamily }

procedure TMuiFamily.InstallHooks;
begin
  ChildList := TObjectList.Create;
  ChildList.OwnsObjects := False;
end;

destructor TMuiFamily.Destroy;
begin
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

constructor TMuiWindow.Create(var TagList: TTagsList);
var
  LT: TTagsList;
  GrpTags: TTagsList;
begin
  FMainMenu := TMuiMenuStrip.Create(LT);
  //FGrpObj := MUI_NewObject(MUIC_Group,[LongInt(MUIA_Group_LayoutHook), @LayoutHook, TAG_END]);
  AddTags(GrpTags, [LongInt(MUIA_Group_LayoutHook), @LayoutHook]);
  FGrpObj := NewObjectA(LCLGroupClass, nil, GetTagPtr(GrpTags));
  if Assigned(FGrpObj) then
    Pointer(INST_DATA(LCLGroupClass, Pointer(FGrpObj))^) := Self;
  //
  LayoutHook.h_Entry := IPTR(@LayoutFunc);
  LayoutHook.h_SubEntry := IPTR(@LayoutFunc);
  LayoutHook.h_Data := Self;
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
end;

destructor TMuiWindow.Destroy;
begin
  FMainMenu.Free;
  inherited Destroy;
end;

procedure TMuiWindow.GetSizes;
begin
  Left := GetAttribute(MUIA_Window_LeftEdge);
  Top := GetAttribute(MUIA_Window_TopEdge);
  //
  Width := GetAttribute(MUIA_Window_Width);
  Height := GetAttribute(MUIA_Window_Height);
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
  Result.Right := Width - 5 ;
  Result.Bottom := Height - 15;
end;

procedure TMuiWindow.SetLeft(ALeft: LongInt);
begin
  inherited;
  SetAttribute([LongInt(MUIA_Window_LeftEdge), ALeft]);
end;

procedure TMuiWindow.SetTop(ATop: LongInt);
begin
  inherited;
  SetAttribute([LongInt(MUIA_Window_TopEdge), ATop]);
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

{ TMuiGroup }

constructor TMuiGroup.Create(var tags: TTagsList);
begin
  inherited Create(MUIC_Group, GetTagPtr(Tags));
end;

end.

