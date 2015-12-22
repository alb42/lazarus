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
  Classes, SysUtils, exec, intuition, agraphics,
{$if defined(CPU68) or defined(CPUPOWERPC)}
  amigalib,
{$endif}
  utility, mui, tagsparamshelper;

{$ifdef MorphOS}
const
  RPTAG_PenMode    = $80000080;
  RPTAG_FgColor    = $80000081;
  RPTAG_BgColor    = $80000082;
{$endif}
type
  THookFunc = function(Hook: PHook; Obj: PObject_; Msg: Pointer): LongInt;

procedure ConnectHookFunction(MUIField: PtrUInt; TriggerValue: PtrUInt; Obj: PObject_; Data: TObject; Hook: PHook; HookFunc: THookFunc);
procedure SetHook(var Hook: THook; Func: THookFunc; Data: Pointer);
{$ifndef AROS}
function CallHook(h: PHook; obj: APTR; params: array of NativeUInt): LongWord;
//function DoMethodA(obj : Pointer; msg1 : Pointer): longword;
function CreateRastPort: PRastPort;
function CloneRastPort(Rp: PRastPort): PRastPort;
procedure FreeRastPort(Rp: PRastPort);
{$endif}

{$ifdef Amiga}
var
  IntuitionBase: PIntuitionBase;
{$endif}

implementation

{$undef SetHook}

{$ifdef CPU68}
{$define SetHook}
procedure SetHook(var Hook: THook; Func: THookFunc; Data: Pointer);
begin
  Hook.h_Entry := @HookEntry; { is defined in AmigaLib unit now }
  Hook.h_SubEntry := Func;
  Hook.h_Data := Data;
end;
{$endif}

{$ifdef CPU86}
{$define SetHook}
procedure HookEntry(h: PHook; obj: PObject_; Msg: Pointer); cdecl;
var
  Proc: THookFunc;
begin
  Proc := THookFunc(h^.h_SubEntry);
  Proc(h, obj, msg);
end;

procedure SetHook(var Hook: THook; Func: THookFunc; Data: Pointer);
begin
  Hook.h_Entry := IPTR(@HookEntry);
  Hook.h_SubEntry := IPTR(Func);
  Hook.h_Data := Data;
end;
{$endif}

{$ifdef CPUPOWERPC}
{$define SetHook}
procedure SetHook(var Hook: THook; Func: THookFunc; Data: Pointer);
{ This is MorphOS magic. Basically, CallHookPkt is designed to enter 68k code
  (remember, MorphOS is 68k AmigaOS binary compatible!) so this TRAP just
  redirects that call back to native PPC code. HookEntry is defined in
  AmigaLib unit }
const
  HOOKENTRY_TRAP: TEmulLibEntry = ( Trap: TRAP_LIB; Extension: 0; Func: @HookEntry );
begin
  Hook.h_Entry := @HOOKENTRY_TRAP;
  Hook.h_SubEntry := Func;
  Hook.h_Data := Data;
end;
{$endif}

{$ifndef SetHook}
{$FATAL "SetHook not implemented for this platform"}
{$endif}

procedure ConnectHookFunction(MUIField: PtrUInt; TriggerValue: PtrUInt; Obj: PObject_; Data: TObject; Hook: PHook; HookFunc: THookFunc);
var
  Para: TAParamList;
begin
  SetHook(Hook^, HookFunc, Data);

  Para.SetParams([
    MUIM_Notify, MUIField, TriggerValue, MUIV_Notify_Self,
    2,
    MUIM_CallHook, NativeUInt(Hook),
    0]);
  //
  DoMethodA(Obj, Para);
end;

{$ifndef AROS}
function CallHook(h: PHook; obj: APTR; params: array of NativeUInt): LongWord;
begin
  Result := CallHookPkt(h, obj, @Params[0]);
end;

//function DoMethodA(obj : Pointer; msg1 : Pointer): longword;
//begin
//  DoMethodA := amigalib.DoMethodA(DWord(obj), msg1);
//end;

function CreateRastPort: PRastPort;
begin
  Result := AllocMem(SizeOf(TRastPort));
  InitRastPort(Result);
end;

function CloneRastPort(Rp: PRastPort): PRastPort;
begin
  Result := AllocMem(SizeOf(TRastPort));
  Move(Rp^, Result^, SizeOf(TRastPort));
end;

procedure FreeRastPort(Rp: PRastPort);
begin
  FreeMem(Rp);
end;
{$endif}


initialization
{$ifdef Amiga}
  IntuitionBase := _IntuitionBase;
{$endif}
end.

