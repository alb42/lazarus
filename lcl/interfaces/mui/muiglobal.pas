{
 *****************************************************************************
 *                             MUIglobal.pas                                 *
 *                              --------------                               *
 *     Global functions for easier implementation of different Systems       *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit MUIglobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, exec, intuition, agraphics, utility, mui, tagsparamshelper;

type
  THookFunc = function(Hook: PHook; Obj: PObject_; Msg: Pointer): LongInt; cdecl;

procedure ConnectHookFunction(MUIField: PtrUInt; TriggerValue: PtrUInt; Obj: PObject_; Data: TObject; Hook: PHook; HookFunc: THookFunc);

implementation

procedure ConnectHookFunction(MUIField: PtrUInt; TriggerValue: PtrUInt; Obj: PObject_; Data: TObject; Hook: PHook; HookFunc: THookFunc);
var
  Para: TAParamList;
begin
  {$ifdef AROS}
  Hook^.h_Entry := IPTR(HookFunc);
  Hook^.h_SubEntry := 0;
  Hook^.h_Data := Data;
  //
  Para.SetParams([
    MUIM_Notify, MUIField, TriggerValue, MUIV_Notify_Self,
    2,
    MUIM_CallHook, NativeUInt(Hook),
    0]);
  //
  CallHookPkt(PHook(OCLASS(Obj)), Obj, Para);
  {$endif}
end;

end.

