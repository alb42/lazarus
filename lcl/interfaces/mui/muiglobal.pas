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
{$if defined(AROS) and defined(VER3_0)}
  {$define FPC4AROS_VER3_FIXES}
{$endif}
interface

uses
  Classes, SysUtils, exec, intuition, agraphics,
{$if defined(CPU68) or defined(CPUPOWERPC)}
  {$ifndef AMIGAOS4}
  amigalib,
  {$endif}
{$endif}
  utility, mui, tagsparamshelper;

{$ifdef MorphOS}
const
  RPTAG_PenMode    = $80000080;
  RPTAG_FgColor    = $80000081;
  RPTAG_BgColor    = $80000082;
{$endif}
{$ifdef AmigaOS4}
const
  RPTAG_FGCOLOR = RPTAG_APENCOLOR;
  RPTAG_BGCOLOR = RPTAG_BPENCOLOR;
  RPTAG_PENMODE = TAG_IGNORE;
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
{$ifdef FPC4AROS_VER3_FIXES}
function DoMethod(Obj: PObject_; const Args: array of PtrUInt): IPTR;
function GetAttr(AttrID: LongWord; Object_: PObject_; var Storage: IPTR): LongWord; overload syscall IntuitionBase 109;
{$endif}
{$ifdef MorphOS}
function DoMethodA(obj : pObject_; msg1 : Pointer): longword; overload;
{$endif}

{$ifdef Amiga68k}
var
  IntuitionBase: PIntuitionBase;
{$endif}
{$ifdef Amiga}
function DoMethod(obj: Pointer; params: array of DWord): LongWord; overload;
function DoMethod(obj: LongWord; params: array of DWord): LongWord; overload;
{$endif}

implementation

{$ifdef MorphOS}
function DoMethodA(obj : pObject_; msg1 : Pointer): longword;
begin
  Result := Amigalib.DoMethodA(LongWord(Obj), Msg1);
end;
{$endif}

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
{$ifdef MorphOS}
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
{$ifdef AMIGAOS4}
{$define SetHook}
procedure SetHook(var Hook: THook; Func: THookFunc; Data: Pointer);
begin
  Hook.h_Entry := Func;
  Hook.h_SubEntry := Func;
  Hook.h_Data := Data;
end;
{$endif}
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

{$ifdef FPC4AROS_VER3_FIXES}
function DoMethod(Obj: PObject_; const Args: array of PtrUInt): IPTR; inline;
begin
  DoMethod := 0;
  if obj = nil then
    Exit;
  DoMethod := CALLHOOKPKT_(PHook(OCLASS(Obj)), Obj, @Args);
end;
{$endif}

{$ifdef Amiga}
function DoMethod(obj: Pointer; params: array of DWord): LongWord;
begin
  Result := DoMethodA(obj, @params);
end;

function DoMethod(obj: LongWord; params: array of DWord): LongWord;
begin
  Result := DoMethodA(Pointer(obj), @params);
end;
{$endif}

initialization
{$ifdef Amiga68k}
  IntuitionBase := _IntuitionBase;
{$endif}
end.
