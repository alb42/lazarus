unit MUIglobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, exec, intuition, agraphics, gadtools, utility;

type
  TStringPtrs= array of PChar;
  PStringPtrs= ^TStringPtrs;

var
  Topaz: TTextAttr;
  GlobalVisInfo: Pointer;
  GlobalScreen: pScreen;
function GetUniqueNumber:LongInt;
function Tags(a: array of Const): PTagItem;


implementation

var
  UNr: LongInt;

function GetUniqueNumber:LongInt;
begin
  Inc(UNr);
  Result := (UNr);
end;

function Tags(a: array of Const): PTagItem;
begin
  Tags := @a[0];
end;

initialization
  UNr := 1;
  Topaz.ta_Name := 'topaz.font';
  Topaz.ta_YSize := 8;
  Topaz.ta_Style := FS_NORMAL;
  Topaz.ta_Flags := FPB_ROMFONT;
  //
  GlobalScreen := LockPubScreen(NIL);
  GlobalVisInfo := GetVisualInfoA(GlobalScreen, NIL);

finalization
  FreeVisualInfo(GlobalVisInfo);
  UnlockPubScreen(NIL, GlobalScreen);
end.

