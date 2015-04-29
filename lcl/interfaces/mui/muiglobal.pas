unit MUIglobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, exec, intuition, agraphics, gadtools, utility, mui;

type
  THookFunc = function(Hook: PHook; Obj: PObject_; Msg: Pointer): LongInt; cdecl;

//var
//  GlobalVisInfo: Pointer;
//  GlobalScreen: pScreen;

procedure ConnectHookFunction(MUIField: PtrUInt; TriggerValue: PtrUInt; Obj: PObject_; Data: TObject; Hook: PHook; HookFunc: THookFunc);

implementation

procedure ConnectHookFunction(MUIField: PtrUInt; TriggerValue: PtrUInt; Obj: PObject_; Data: TObject; Hook: PHook; HookFunc: THookFunc);
var
  Params: array of PtrUInt;
begin
  {$ifdef AROS}
  Hook^.h_Entry := IPTR(HookFunc);
  Hook^.h_SubEntry := 0;
  Hook^.h_Data := Data;
  //
  SetLength(Params, 7);
  Params[0] := PtrUInt(MUIM_Notify);
  Params[1] := PtrUInt(MUIField);
  Params[2] := PtrUInt(TriggerValue);
  Params[3] := PtrUInt(MUIV_Notify_Self);
  Params[4] := 2;
  Params[5] := PtrUInt(MUIM_CallHook);
  Params[6] := PtrUInt(Hook);
  Params[7] := 0;
  //
  CallHookPkt(PHook(OCLASS(Obj)), Obj, @(Params[0]));
  {$endif}
end;


initialization
  {
  GlobalScreen := LockPubScreen(NIL);
  GlobalVisInfo := GetVisualInfoA(GlobalScreen, NIL);
  }
finalization
  //FreeVisualInfo(GlobalVisInfo);
  //UnlockPubScreen(NIL, GlobalScreen);
end.

