{ $Id: ArosGadwsforms.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                               MUIWSForms.pp                                *
 *                               ------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit MUIWSForms;

{$mode objfpc}{$H+}

interface

uses
  sysutils, Intuition, exec, utility, gadtools,
  // LCL
  MUIBaseUnit, MuiFormsUnit, Mui, Classes, Forms, LCLType, Controls, Graphics,
  tagsarray,
  // Widgetset
  WSForms, WSLCLClasses;

type

  { TMUIWSScrollingWinControl }

  TMUIWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
  end;

  { TMUIWSScrollBox }

  TMUIWSScrollBox = class(TWSScrollBox)
  private
  protected
  public
  end;

  { TMUIWSCustomFrame }

  TMUIWSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TMUIWSFrame }

  TMUIWSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TMUIWSCustomForm }

  TMUIWSCustomForm = class(TWSCustomForm)
  private
  protected
  published
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;

    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;

    class procedure SetFormBorderStyle(const AForm: Forms.TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetFont(const AWinControl: TWinControl;
                           const AFont: TFont); override;
    class procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); override;
    class procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); override;
    class procedure ShowHide(const AWinControl: TWincontrol); override;
  end;

  { TMUIWSForm }

  TMUIWSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TMUIWSHintWindow }

  TMUIWSHintWindow = class(TWSHintWindow)
  private
  protected
  public
  end;

  { TMUIWSScreen }

  TMUIWSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TMUIWSApplicationProperties }

  TMUIWSApplicationProperties = class(TWSApplicationProperties)
  private
  protected
  public
  end;

implementation



{ TMUIWSCustomForm }

{------------------------------------------------------------------------------
  Method: TMUIWSCustomForm.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TMUIWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  MUIForm: TMuiWindow;
  TagList: TTagsList;
  Sizeable: Boolean;
begin
  //writeln('--> Create Form');
  Sizeable := True;
  case TCustomForm(AWincontrol).BorderStyle of
    bsNone: begin
      Sizeable := False;
      AddTags(TagList, [MUIA_Window_Borderless, True]);
    end;
    bsDialog: begin
      AddTags(TagList, [MUIA_Window_CloseGadget, True, MUIA_Window_SizeRight, False, MUIA_Window_SizeGadget, False]);
      Sizeable := False; 
    end;
    bsSingle: begin
      AddTags(TagList, [MUIA_Window_SizeRight, False]);
    end;
    bsToolWindow: begin
      AddTags(TagList, [MUIA_Window_SizeRight, False]);
      Sizeable := False;
    end;
    bsSizeToolWin: begin
      AddTags(TagList, [MUIA_Window_SizeRight, False]);
    end;
  end;
  AddTags(TagList, [
    MUIA_Window_LeftEdge, AParams.X,
    MUIA_Window_TopEdge, AParams.Y,
    MUIA_Window_Width, AParams.Width, 
    MUIA_Window_Height, AParams.Height
    ]);
  MuiForm := TMuiWindow.create(TagList);
  MuiForm.Sizeable := Sizeable;
  With Muiform do
  begin
    Left := AParams.X;
    Top := AParams.Y;
    Width := AParams.Width;
    Height := AParams.Height;
    PasObject := AWinControl;
    Caption := TCustomForm(AWinControl).Caption;
  end;
  Result := TLCLIntfHandle(MuiForm);
end;

{------------------------------------------------------------------------------
  Method: TMUIWSCustomForm.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TMUIWSCustomForm.DestroyHandle(const AWinControl: TWinControl);
begin
  //Writeln('<-- Destroy Form');
  TMuiWindow(AWinControl.Handle).Free;
  AWinControl.Handle := 0;
end;

{------------------------------------------------------------------------------
  Method: TMUIWSCustomForm.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TMUIWSCustomForm.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  MUIForm: TMuiWindow;
begin
  Result := False;
  MUIForm := TMuiWindow(AWinControl.Handle);
  if Assigned(MUIForm) then
  begin
    AText := MUIForm.Caption;
    Result := True;
  end;
end;

{------------------------------------------------------------------------------
  Method: TMUIWSCustomForm.SetFormBorderStyle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TMUIWSCustomForm.SetFormBorderStyle(const AForm: Forms.TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
begin
  
end;

class procedure TMUIWSCustomForm.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
begin
end;

{------------------------------------------------------------------------------
  Method: TMUIWSCustomForm.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TMUIWSCustomForm.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  MUIForm: TMuiWindow;
begin
  MUIForm := TMuiWindow(AWinControl.Handle);
  if Assigned(MUIForm) then
  begin
    MUIForm.Caption := AText;
  end;
end;


class procedure TMUIWSCustomForm.SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer);
var
  fpForm: pWindow;
  dX: LongInt;
  dY: LongInt;
begin
  //writeln('setpos ', Aleft, ATop);
  fpForm := MUIRenderInfo(TMuiObject(AWinControl.Handle).obj)^.mri_Window;
  if Assigned(fpForm) then
  begin
    dX := fpForm^.LeftEdge - ALeft;
    dY := fpForm^.TopEdge - ATop;
    MoveWindow(fpForm, dX, dY);
  end;
end;

class procedure TMUIWSCustomForm.SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer);
var
  fpForm: pWindow;
  dX: LongInt;
  dY: LongInt;
begin
  //writeln('setsize ', AWidth, AHeight);
  fpForm := MUIRenderInfo(TMuiObject(AWinControl.Handle).obj)^.mri_Window;
  if Assigned(fpForm) then
  begin
    dX := fpForm^.Width - AWidth;
    dY := fpForm^.Height - AHeight;
    SizeWindow(fpForm, dX, dY);
  end;
end;

class procedure TMUIWSCustomForm.ShowHide(const AWinControl: TWincontrol);
begin
  inherited ShowHide(AWinControl);
  TMuiWindow(AWinControl.Handle).Visible := AWinControl.Visible;
end;


end.
