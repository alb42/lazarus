unit MuiFormsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, controls, Contnrs, Exec, AmigaDos, Intuition, Utility, Mui,
  Forms, MuiBaseUnit, lclmessageglue, menus, Tagsarray;

type

  { TMuiFamily }

  TMuiFamily = class(TMuiObject)
  public
    procedure AddHead(AChild: TMuiFamily);
    procedure AddTail(AChild: TMuiFamily);
    procedure Remove(AChild: TMuiFamily);
  end;

  { TMuiMenuItem }

  TMuiMenuItem = class(TMuiFamily)
  private
    MenuChoosed: THook;
    function GetChecked: Boolean;
    function GetCheckIt: Boolean;
    function GetEnabled: Boolean;
    function GetShortCut: string;
    function GetTitle: string;
    procedure SetChecked(const AValue: Boolean);
    procedure SetCheckIt(const AValue: Boolean);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetShortCut(const AValue: string);
    procedure SetTitle(const AValue: string);
  public
    constructor Create(tags: array of Const);
    property Checked: Boolean read GetChecked write SetChecked;
    property CheckIt: Boolean read GetCheckIt write SetCheckIt;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property ShortCut: string read GetShortCut write SetShortCut;
    property Title: string read GetTitle write SetTitle;
  end;

  { TMuiMenu }

  TMuiMenu = class(TMuiFamily)
  private
    function GetEnabled: Boolean;
    function GetTitle: string;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetTitle(const AValue: string);
  public
    constructor Create(tags: array of Const);
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Title: string read GetTitle write SetTitle;
  end;

  { TMuiMenuStrip }

  TMuiMenuStrip = class(TMuiFamily)
  private
    function GetEnabled: Boolean;
    procedure SetEnabled(const AValue: Boolean);
  public
    constructor Create(tags: array of Const);
    Destructor Destroy; override;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  { TMuiWindow }

  TMuiWindow = class(TMUIObject)
  private
    CloseWinHook: THook;
    FMainMenu: TMuiMenuStrip;
    function GetCaption: string;
    procedure SetCaption(const AValue: string);
  protected
    function GetVisible: Boolean; override;
    procedure SetVisible(const AValue: Boolean); override;
    procedure AddChild(Child: TMUIObject); override;
    procedure RemoveChild(Child: TMUIObject); override;
  public
    constructor Create; overload; reintroduce; virtual;
    destructor Destroy; override;
    procedure GetSizes;
    property Caption: string read GetCaption write SetCaption;
    property MainMenu: TMuiMenuStrip read FMainMenu;
  end;

  { TMuiGroup }

  TMuiGroup = class(TMuiObject)
  private

  public
    constructor Create(const Tags : Array Of Const); overload; reintroduce; virtual;
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
      LMsg^.lm_MinMax.MinWidth := 100;
      LMsg^.lm_MinMax.MinHeight := 100;
      LMsg^.lm_MinMax.DefWidth :=  Win.Width;
      LMsg^.lm_MinMax.DefHeight := Win.Height;
      LMsg^.lm_MinMax.MaxWidth := 10000;
      LMsg^.lm_MinMax.MaxHeight := 10000;
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

constructor TMuiMenuStrip.Create(tags: array of const);
begin
  inherited Create(MUIC_MenuStrip, tags);
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
  SetAttribute([LongInt(MUIA_Menuitem_Title), PChar(AValue), TAG_END]);
end;

constructor TMuiMenu.Create(tags: array of const);
begin
  inherited Create(MUIC_Menu, tags);
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

function TMuiMenuItem.GetShortCut: string;
begin
  Result := string(PChar(GetAttribute(MUIA_Menuitem_ShortCut)));
end;

function TMuiMenuItem.GetTitle: string;
begin
  Result := string(PChar(GetAttribute(MUIA_Menuitem_Title)));
end;

procedure TMuiMenuItem.SetChecked(const AValue: Boolean);
begin
  SetAttribute([LongInt(MUIA_Menuitem_Checked), AValue, TAG_END]);
end;

procedure TMuiMenuItem.SetCheckIt(const AValue: Boolean);
begin
  SetAttribute([LongInt(MUIA_Menuitem_CheckIt), AValue, TAG_END]);
end;

procedure TMuiMenuItem.SetEnabled(const AValue: Boolean);
begin
  //writeln('SetEnabled: ',AValue);
  SetAttribute([LongInt(MUIA_Menuitem_Enabled), AValue, TAG_END]);
  //writeln('getEnabled: ', GetEnabled);
end;

procedure TMuiMenuItem.SetShortCut(const AValue: string);
begin
  //SetAttribute([LongInt(MUIA_Menuitem_ShortCut), PChar(AValue), TAG_END]);
end;

procedure TMuiMenuItem.SetTitle(const AValue: string);
begin
  SetAttribute([LongInt(MUIA_Menuitem_Title), PChar(AValue), TAG_END]);
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

constructor TMuiMenuItem.Create(tags: array of const);
begin
  inherited Create(MUIC_MenuItem, tags);
  MenuChoosed.h_Entry := IPTR(@MenuClickedFunc);
  MenuChoosed.h_SubEntry := IPTR(@MenuClickedFunc);
  MenuChoosed.h_Data := Self;
  DoMethod([LongInt(MUIM_Notify), LongInt(MUIA_Menuitem_Trigger), LongInt(MUIV_EveryTime),
      LongInt(MUIV_Notify_Self),
      2,
      LongInt(MUIM_CallHook), IPTR(@MenuChoosed)
      ])
end;

{ TMuiFamily }

procedure TMuiFamily.AddHead(AChild: TMuiFamily);
begin
  DoMethod([LongInt(MUIM_Family_AddHead), IPTR(AChild.Obj)]);
end;

procedure TMuiFamily.AddTail(AChild: TMuiFamily);
begin
  DoMethod([LongInt(MUIM_Family_AddTail), IPTR(AChild.Obj)]);
end;

procedure TMuiFamily.Remove(AChild: TMuiFamily);
begin
  DoMethod([LongInt(MUIM_Family_Remove), IPTR(AChild.Obj)]);
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

constructor TMuiWindow.Create;
begin
  FMainMenu := TMuiMenuStrip.Create([TAG_END]);
  FGrpObj := MUI_NewObject(MUIC_Group,[LongInt(MUIA_Group_LayoutHook), @LayoutHook, TAG_END]);
  //
  LayoutHook.h_Entry := IPTR(@LayoutFunc);
  LayoutHook.h_SubEntry := IPTR(@LayoutFunc);
  LayoutHook.h_Data := Self;
  //
  inherited Create(MUIC_Window, [LongInt(MUIA_Window_Menustrip), FMainMenu.Obj, LongInt(MUIA_Window_RootObject), FGrpObj, TAG_END]);
  //
  Self.Parent := MUIApp;
  CloseWinHook.h_Entry := IPTR(@CloseWinFunc);
  CloseWinHook.h_SubEntry := 0;
  CloseWinHook.h_Data := Self;
  DoMethod([LongInt(MUIM_Notify), LongInt(MUIA_Window_CloseRequest), LTrue,
      LongWord(FObject), 2,
      LongInt(MUIM_CallHook), IPTR(@CloseWinHook)
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
  CallHook(PHook(OCLASS(FGrpObj)), FGrpObj, [OM_ADDMEMBER, Child.obj]);
end;

procedure TMuiWindow.RemoveChild(Child: TMUIObject);
begin
  CallHook(PHook(OCLASS(FGrpObj)), FGrpObj, [OM_REMMEMBER, Child.obj]);
end;

{ TMuiGroup }

constructor TMuiGroup.Create(const Tags: array of const);
begin
  inherited Create(MUIC_Group, Tags);

end;

end.

