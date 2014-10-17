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
  muicomctrls, muibaseunit, dos, exec,
  // LCL
  Classes,
  ComCtrls, Controls, LCLType, mui,
  // Widgetset
  WSComCtrls, WSLCLClasses;

type
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
  public
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

