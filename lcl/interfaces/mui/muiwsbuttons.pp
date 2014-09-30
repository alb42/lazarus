{ $Id: MUIwsbuttons.pp 5682 2004-07-15 10:43:39Z mattias $}
{
 *****************************************************************************
 *                              MUIWSButtons.pp                          *
 *                              --------------                               * 
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
unit MUIWSButtons;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  // LCL
  Buttons, LCLType, Controls,
  // Widgetset
  WSButtons, WSLCLClasses;

type

  { TMUIWSBitBtn }

  TMUIWSBitBtn = class(TWSBitBtn)
  private
  protected
  public
  end;

  { TMUIWSSpeedButton }

  TMUIWSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  end;


implementation

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomBitBtn, TMUIWSBitBtn);
//  RegisterWSComponent(TCustomSpeedButton, TMUIWSSpeedButton);
////////////////////////////////////////////////////
end.
