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
  Classes, SysUtils, exec, amigados, intuition, agraphics, timer,
{$if defined(CPU68) or defined(CPUPOWERPC)}
  {$if defined(AMIGA68k) or defined(MorphOS)}
  amigalib,
  {$endif}
{$endif}
  utility, mui, tagsparamshelper;

{$ifdef MorphOS}
// Missing in the fpc units
const
  RPTAG_PenMode    = $80000080;
  RPTAG_FgColor    = $80000081;
  RPTAG_BgColor    = $80000082;
{$endif}
{$ifdef AmigaOS4}
// Colorsetting tags are different to AROS/MorphOS
const
  RPTAG_FGCOLOR = RPTAG_APENCOLOR;
  RPTAG_BGCOLOR = RPTAG_BPENCOLOR;
  RPTAG_PENMODE = TAG_IGNORE;
{$endif}
type
  THookFunc = function(Hook: PHook; Obj: PObject_; Msg: Pointer): LongInt;

function GetLCLTime: Int64;

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

var
  Tr: PTimeRequest = nil;


procedure NewList (list: pList);
begin
  with list^ do begin
    lh_Head     := pNode(@lh_Tail);
    lh_Tail     := NIL;
    lh_TailPred := pNode(@lh_Head)
  end;
end;

function CreateExtIO (port: pMsgPort; size: Longint): pIORequest;
var
   IOReq: pIORequest;
begin
    IOReq := NIL;
    if port <> NIL then
    begin
        IOReq := execAllocMem(size, MEMF_CLEAR);
        if IOReq <> NIL then
        begin
            IOReq^.io_Message.mn_Node.ln_Type   := 7;
            IOReq^.io_Message.mn_Length    := size;
            IOReq^.io_Message.mn_ReplyPort := port;
        end;
    end;
    CreateExtIO := IOReq;
end;

procedure DeleteExtIO (ioReq: pIORequest);
begin
    if ioReq <> NIL then
    begin
        ioReq^.io_Message.mn_Node.ln_Type := $FF;
        ioReq^.io_Message.mn_ReplyPort    := pMsgPort(-1);
        ioReq^.io_Device                  := pDevice(-1);
        execFreeMem(ioReq, ioReq^.io_Message.mn_Length);
    end
end;

function Createport(name : PChar; pri : longint): pMsgPort;
var
   sigbit : ShortInt;
   port    : pMsgPort;
begin
   sigbit := AllocSignal(-1);
   if sigbit = -1 then CreatePort := nil;
   port := execAllocMem(sizeof(tMsgPort),MEMF_CLEAR);
   if port = nil then begin
      FreeSignal(sigbit);
      CreatePort := nil;
   end;
   with port^ do begin
       if assigned(name) then
       mp_Node.ln_Name := name
       else mp_Node.ln_Name := nil;
       mp_Node.ln_Pri := pri;
       mp_Node.ln_Type := 4;
       mp_Flags := 0;
       mp_SigBit := sigbit;
       mp_SigTask := FindTask(nil);
   end;
   if assigned(name) then AddPort(port)
   else NewList(addr(port^.mp_MsgList));
   CreatePort := port;
end;

procedure DeletePort (port: pMsgPort);
begin
    if port <> NIL then
    begin
        if port^.mp_Node.ln_Name <> NIL then
            RemPort(port);

        port^.mp_Node.ln_Type     := $FF;
        port^.mp_MsgList.lh_Head  := pNode(-1);
        FreeSignal(port^.mp_SigBit);
        execFreeMem(port, sizeof(tMsgPort));
    end;
end;

function Create_Timer(theUnit : longint) : pTimeRequest;
var
  Error : longint;
  TimerPort : pMsgPort;
  TimeReq : pTimeRequest;
begin
  TimerPort := CreatePort(Nil, 0);
  if TimerPort = Nil then
    Create_Timer := Nil;
  TimeReq := pTimeRequest(CreateExtIO(TimerPort,sizeof(tTimeRequest)));
  if TimeReq = Nil then begin
    DeletePort(TimerPort);
    Create_Timer := Nil;
  end;
  Error := OpenDevice(TIMERNAME, theUnit, pIORequest(TimeReq), 0);
  if Error <> 0 then begin
    DeleteExtIO(pIORequest(TimeReq));
    DeletePort(TimerPort);
    Create_Timer := Nil;
  end;
  //LCLTimerBase := pointer(TimeReq^.tr_Node.io_Device);
  Create_Timer := pTimeRequest(TimeReq);
end;

Procedure Delete_Timer(WhichTimer : pTimeRequest);
var
    WhichPort : pMsgPort;
begin

    WhichPort := WhichTimer^.tr_Node.io_Message.mn_ReplyPort;
    if assigned(WhichTimer) then begin
        CloseDevice(pIORequest(WhichTimer));
        DeleteExtIO(pIORequest(WhichTimer));
    end;
    if assigned(WhichPort) then
        DeletePort(WhichPort);
end;

function get_sys_time(tv : ptimeval): longint;
begin
  if not Assigned(Tr) then
    Tr := create_timer(UNIT_MICROHZ);
  { non zero return says error }
  if tr = nil then
    Result := -1;

  tr^.tr_node.io_Command := TR_GETSYSTIME;
  DoIO(pIORequest(tr));

  { structure assignment }
  tv^ := tr^.tr_time;

  Result := 0;
end;

function GetLCLTime: Int64;
var
  TV: TTimeVal;
begin
  Get_Sys_Time(@TV);
  Result := int64 (TV.TV_Secs) * 1000 + TV.TV_Micro div 1000;
end;

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
  if not Assigned(Tr) then
    Tr := create_timer(UNIT_MICROHZ);
{$ifdef Amiga68k}
  IntuitionBase := _IntuitionBase;
{$endif}

finalization
  if Assigned(Tr) then
    Delete_timer(tr);
end.
