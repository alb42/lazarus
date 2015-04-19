{
 *****************************************************************************
 *                              MuiWSComCtrls.pp                              *
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.LCL, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit MuiWSComCtrls;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  muicomctrls, muibaseunit, muistdctrls,
  dos, exec,
  // LCL
  Classes,
  ComCtrls, Controls, LCLType, mui,
  // Widgetset
  WSComCtrls, WSLCLClasses;

type
  
  
  { TGtk2WSCustomTabControl }

  TMUIWSCustomTabControl = class(TWSCustomTabControl)
  private
    //class function CreateTTabControlHandle(const AWinControl: TWinControl;
    //  const AParams: TCreateParams): HWND;
  protected
    //class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl;
                                const AParams: TCreateParams): HWND; override;
    (*)class function GetDefaultClientRect(const AWinControl: TWinControl;
             const {%H-}aLeft, {%H-}aTop, aWidth, aHeight: integer; var aClientRect: TRect
             ): boolean; override;*)
    class procedure AddPage(const ATabControl: TCustomTabControl;
      const AChild: TCustomPage; const AIndex: integer); override;
    
    class procedure SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer); override;

    class procedure ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean); override;
    (*class procedure MovePage(const ATabControl: TCustomTabControl;
      const AChild: TCustomPage; const NewIndex: integer); override;

    class function GetCapabilities: TCTabControlCapabilities; override;
    class function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer; override;
    class function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer; override;
    class function GetTabIndexAtPos(const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer; override;
    class function GetTabRect(const ATabControl: TCustomTabControl; const AIndex: Integer): TRect; override;
    class procedure SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition); override;

    class procedure UpdateProperties(const ATabControl: TCustomTabControl); override;*)
  end;
  
  
  
  { TmuiWSCustomPage }

  TmuiWSCustomPage = class(TWSCustomPage)
  private
  protected
  public
  end;

  { TmuiWSCustomNotebook }

  TmuiWSCustomNotebook = class(TWSCustomTabControl)
  private
  protected
  published
    //class function  CreateHandle(const AWinControl: TWinControl;
    //      const AParams: TCreateParams): HWND; override;
    //class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

  { TmuiWSStatusBar }

  TmuiWSStatusBar = class(TWSStatusBar)
  private
  protected
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure Update(const AStatusBar: TStatusBar); override;
    class procedure GetPreferredSize(const {%H-}AWinControl: TWinControl;
                        var {%H-}PreferredWidth, PreferredHeight: integer;
                        {%H-}WithThemeSpace: Boolean); override;

    class procedure SetSizeGrip(const AStatusBar: TStatusBar; {%H-}SizeGrip: Boolean); override;
  end;

  { TmuiWSTabSheet }

  TmuiWSTabSheet = class(TWSTabSheet)
  private
  protected
  public
  end;

  { TmuiWSPageControl }

  TmuiWSPageControl = class(TWSPageControl)
  private
  protected
  public
  end;

  { TmuiWSCustomListView }

  TmuiWSCustomListView = class(TWSCustomListView)
  private
  protected
  public
  end;

  { TmuiWSListView }

  TmuiWSListView = class(TWSListView)
  private
  protected
  public
  end;

  { TmuiWSProgressBar }

  TmuiWSProgressBar = class(TWSProgressBar)
  private
  protected
  public
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
//    class procedure SetStyle(const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle); override;
  end;

  { TmuiWSCustomUpDown }

  TmuiWSCustomUpDown = class(TWSCustomUpDown)
  private
  protected
  public
  end;

  { TmuiWSUpDown }

  TmuiWSUpDown = class(TWSUpDown)
  private
  protected
  public
  end;

  { TmuiWSToolButton }

  TmuiWSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TmuiWSToolBar }

  TmuiWSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TmuiWSTrackBar }

  TmuiWSTrackBar = class(TWSTrackBar)
  private
  protected
  public
  end;

  { TmuiWSCustomTreeView }

  TmuiWSCustomTreeView = class(TWSCustomTreeView)
  private
  protected
  public
  end;

  { TmuiWSTreeView }

  TmuiWSTreeView = class(TWSTreeView)
  private
  protected
  public
  end;


implementation

uses
  tagsarray;
  
  
class function TMUIWSCustomTabControl.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  MUIRegister: TMUIRegister;
  TagList: TTagsList;
begin
  //writeln('Create Tabcontrol');
  //AddTags(TagList, [MUIA_Group_PageMode, True]);
  MUIRegister := TMUIRegister.Create(MUIC_Group, TagList);
  With MUIRegister do
  begin
    ShowTabs := TCustomTabControl(AWinControl).ShowTabs;
    Left := AParams.X;
    Top := AParams.Y;
    Width := AParams.Width;
    Height := AParams.Height;
    PasObject := AWinControl;
    Caption := PChar(AParams.Caption);
  end;
  if AWinControl.Parent <> NIL then
  begin
    MUIRegister.Parent := TMuiObject(AWinControl.Parent.Handle);
  end;
  Result := HWND(MUIRegister);  
end;                                

class procedure TMUIWSCustomTabControl.AddPage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AIndex: integer);
var
  MUIRegister: TMUIRegister;
  MUIObj: TMUIObject;
begin
  //Writeln('Add now page ', AChild.Caption, ' ', HexStr(ATabControl));
  MUIRegister := TMUIRegister(ATabControl.Handle);   
  MUIObj := TMUIObject(AChild.Handle);
  MUIObj.Parent := MUIRegister;
  //AChild.Parent := ATabControl;
end;

class procedure TMUIWSCustomTabControl.SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer);
var
  MUIRegister: TMUIRegister;
begin
  //writeln('Set Pageidx to ', AIndex);
  MUIRegister := TMUIRegister(ATabControl.Handle);
  if Assigned(MUIRegister) then
  begin
    MuiRegister.ActivePage := AIndex;
  end;  
end;

class procedure TMUIWSCustomTabControl.ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean);
begin
  //RecreateWnd(ATabControl);
end;

{ TmuiWSStatusBar }

class function  TmuiWSStatusBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  MUIText: TMUIText;
  TagList: TTagsList;
begin
  AddTags(TagList, [MUIA_Frame, MUIV_Frame_Text]);
  MUIText := TMUIText.Create(TagList);
  with MUIText do
  begin
    Left := AParams.X;
    Top := AParams.Y;
    Width := AParams.Width;
    Height := AParams.Height;
    PasObject := AWinControl;
    Caption := TStatusBar(AWinControl).SimpleText;
  end;
  if AWinControl.Parent <> NIL then
  begin
    MUIText.Parent := TMuiObject(AWinControl.Parent.Handle);
  end;
  Result := TLCLIntfHandle(MUIText);
end;

class procedure TmuiWSStatusBar.PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer);
begin
  //
end;

class procedure TmuiWSStatusBar.SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer);
var
  Area: TMUIText;
begin
  Area := TMUIText(AStatusBar.Handle);
  if Assigned(Area) then
  begin
    Area.Caption := AStatusbar.SimpleText;
  end;
end;

class procedure TmuiWSStatusBar.Update(const AStatusBar: TStatusBar);
begin

end;

class procedure TmuiWSStatusBar.GetPreferredSize(const {%H-}AWinControl: TWinControl;
                    var {%H-}PreferredWidth, PreferredHeight: integer;
                    {%H-}WithThemeSpace: Boolean);
begin
  PreferredHeight := 22;
  PreferredWidth := 100;
end;

class procedure TmuiWSStatusBar.SetSizeGrip(const AStatusBar: TStatusBar; {%H-}SizeGrip: Boolean);
begin

end;


{ TmuiWSProgressBar }

class function TmuiWSProgressBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  TagList: TTagsList;
  Gauge: TMUIGauge;
  Horiz: Boolean;
begin
  if AWinControl is TProgressBar then
  begin
    Horiz := (TProgressBar(AWinControl).Orientation = pbHorizontal) or (TProgressBar(AWinControl).Orientation = pbRightToLeft);
    if Horiz then
      AddTags(TagList, [MUIA_Gauge_Horiz, LTrue])
    else
      AddTags(TagList, [MUIA_Gauge_Horiz, LFalse]);
  end;
  AddTags(TagList, [MUIA_Frame, MUIV_Frame_Text, MUIA_Gauge_InfoText, PChar('')]);
  Gauge := TMUIGauge.Create(MUIC_Gauge, GetTagPtr(TagList));
  Gauge.Horiz := Horiz;
  With Gauge do
  begin
    Left := AParams.X;
    Top := AParams.Y;
    Width := AParams.Width;
    Height := AParams.Height;
    PasObject := AWinControl;
    Caption := PChar(AParams.Caption);
  end;

  if AWinControl.Parent <> NIL then
  begin
    Gauge.Parent := TMuiObject(AWinControl.Parent.Handle);
  end;
  Result := TLCLIntfHandle(Gauge);
end;

class procedure TmuiWSProgressBar.DestroyHandle(const AWinControl: TWinControl);
begin
  TMUIGauge(AWinControl.Handle).Free;
  AWinControl.Handle := 0;
end;

class procedure TmuiWSProgressBar.ApplyChanges(
  const AProgressBar: TCustomProgressBar);
var
  Gauge: TMUIGauge;
begin
  Gauge := TMUIGauge(AProgressBar.Handle);
  Gauge.MinPos := AProgressBar.Min;
  Gauge.MaxPos := AProgressBar.Max;
  Gauge.ShowText := AProgressBar.BarShowText;
  SetPosition(AProgressBar, AProgressBar.Position);
end;

class procedure TmuiWSProgressBar.SetPosition(
  const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  TMUIGauge(AProgressBar.Handle).Position := NewPosition;
end;

{ TmuiWSCustomNotebook }
(*
class function TmuiWSCustomNotebook.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
begin
  Result := TLCLIntfHandle(TmuiPrivatePageControl.Create(AWinControl, AParams));
end;

class procedure TmuiWSCustomNotebook.DestroyHandle(
  const AWinControl: TWinControl);
begin
  TmuiPrivatePageControl(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;
*)

end.

