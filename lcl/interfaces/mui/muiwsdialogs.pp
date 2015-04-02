{ $Id: muiwsdialogs.pp 29734 2011-03-06 12:45:59Z juha $}
{
 *****************************************************************************
 *                              MuiWSDialogs.pp                              *
 *                              ---------------                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    * 
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit MuiWSDialogs;

{$mode objfpc}{$H+}

interface

uses
  // RTL + LCL
  SysUtils, Classes, LCLType, LCLProc, Dialogs, Controls, Forms, Graphics,
  exec, asl, utility, tagsarray, mui, intuition, MuibaseUnit, MUIformsunit,
  AmigaDos,
  // Widgetset
  WSDialogs, WSLCLClasses;

type

  { TMuiWSCommonDialog }

  TMuiWSCommonDialog = class(TWSCommonDialog)
  private
  protected
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TMuiWSFileDialog }

  TMuiWSFileDialog = class(TWSFileDialog)
  private
  protected
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TMuiWSOpenDialog }

  TMuiWSOpenDialog = class(TWSOpenDialog)
  published
  end;

  { TMuiWSSaveDialog }

  TMuiWSSaveDialog = class(TWSSaveDialog)
  published
  end;

  { TMuiWSSelectDirectoryDialog }

  TMuiWSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TMuiWSColorDialog }

  TMuiWSColorDialog = class(TWSColorDialog)
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TMuiWSColorButton }

  TMuiWSColorButton = class(TWSColorButton)
  published
  end;

  { TMuiWSFontDialog }

  TMuiWSFontDialog = class(TWSFontDialog)
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;


implementation

  
{ TMuiWSCommonDialog }

{------------------------------------------------------------------------------
  Function: TMuiWSCommonDialog.CreateHandle
  Params:  None
  Returns: Nothing

  Dummy handle creator. On Qt we don´t need a Handle for common dialogs
 ------------------------------------------------------------------------------}
class function TMuiWSCommonDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin

end;

{------------------------------------------------------------------------------
  Function: TMuiWSCommonDialog.DestroyHandle
  Params:  None
  Returns: Nothing

  Dummy handle destructor. On Qt we don´t need a Handle for common dialogs
 ------------------------------------------------------------------------------}
class procedure TMuiWSCommonDialog.DestroyHandle(const ACommonDialog: TCommonDialog);
begin
  if (ACommonDialog.HandleAllocated) then
    FreeAslRequest(Pointer(ACommonDialog.Handle))

end;

class procedure TMuiWSCommonDialog.ShowModal(const ACommonDialog: TCommonDialog);
begin

end;

{ TMuiWSFileDialog }

class function TMuiWSFileDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
var
  FileDialog: PFileRequester;
begin
  FileDialog := PFileRequester(AllocAslRequest(ASL_FileRequest, [TAG_DONE]));
  Result := THandle(FileDialog);
end;

procedure IntuiMsgFunc(iMsg: PIntuiMessage; Req: PFileRequester); cdecl;
begin
  //writeln('test');
  DoMethod(MUIApp.obj, MUIM_Application_CheckRefresh, []);
end;

{------------------------------------------------------------------------------
  Function: TMuiWSFileDialog.ShowModal
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TMuiWSFileDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  FileDialog: TFileDialog;
  MuiDialog: PFileRequester;
  TagsList: TTagsList;
  MultiSelect: Boolean;
  i: LongInt;
  Hook: THook;
  Win: IPTR;

  function GetFilename(FDir, FName: string): string;
  begin
    FDir := Trim(FDir);
    if Length(FDir) = 0 then
      Result := FName
    else
    begin
      if (FDir[Length(FDir)] = DIRECTORYSEPARATOR) or (FDir[Length(FDir)] = ':') then
        Result := FDir + FName
      else
        Result := FDir + DIRECTORYSEPARATOR + FName;
    end;
  end;

begin
  {------------------------------------------------------------------------------
    Initialization of variables
   ------------------------------------------------------------------------------}
  MultiSelect:= False;
  SetLength(TagsList, 0);
  FileDialog := TFileDialog(ACommonDialog);
  MuiDialog := PFileRequester(FileDialog.Handle);
  
  //Win := 0;  
  //GetAttr(MUIA_Window_Window, MUIApp.MainWin, @Win);
  
  
  AddTags(TagsList, [
    //ASLFR_Window, Win,    
    ASLFR_TitleText,	Pchar(ACommonDialog.Title),
    ASLFR_InitialDrawer, PChar(TFileDialog(ACommonDialog).InitialDir),
    ASLFR_InitialFile, PChar(TFileDialog(ACommonDialog).FileName)
  ]);

  If FileDialog.Filter <> '' then
  begin
    //writeln(FileDialog.Filter);
    AddTags(TagsList, [
    ASLFR_InitialPattern, PChar(FileDialog.Filter),
    ASLFR_DoPatterns, True
      ]);
  end;

  if ACommonDialog is TSaveDialog then
  begin
    AddTags(TagsList, [ASLFR_DoSaveMode, True]);
  end else
  begin
    if (ofAllowMultiSelect in TOpenDialog(FileDialog).Options) then
    begin
      MultiSelect:= True;
      AddTags(TagsList, [ASLFR_DoMultiSelect, True]);
    end;
  end;
  if ACommonDialog is TSelectDirectoryDialog then
  begin
    AddTags(TagsList, [ASLFR_DrawersOnly, True]);
  end;
  //
  Hook.h_Entry := IPTR(@IntuiMsgFunc);
  Hook.h_SubEntry := 0;
  Hook.h_Data := MuiDialog;
  //AddTags(TagsList, [ASLFR_UserData, MUIApp, ASLFR_IntuiMsgFunc, @Hook]);//}
  
  //if AslRequestA(MuiDialog, GetTagPtr(TagsList)) then
  if MUI_AslRequest(MuiDialog, GetTagPtr(TagsList)) then
  begin
    FileDialog.FileName := GetFilename(string(MuiDialog^.rf_Dir), string(MuiDialog^.rf_file));
    if MultiSelect then
    begin
      FileDialog.Files.Clear;
      for i := 1 to  MuiDialog^.rf_NumArgs do
      begin
        FileDialog.Files.add(GetFilename(string(MuiDialog^.rf_Dir), string(MuiDialog^.rf_ArgList^[i].wa_Name)));
      end;
    end;
    FileDialog.UserChoice := mrOK;
  end else
    FileDialog.UserChoice := mrCancel;
end;

{ TMuiWSSelectDirectoryDialog }

{class procedure TMuiWSSelectDirectoryDialog.UpdateProperties(
  const AFileDialog: TSelectDirectoryDialog; QtFileDialog: TQtFileDialog);
var
  ATitle: WideString;
begin
  ATitle := GetUtf8String(AFileDialog.Title);
  QtFileDialog.setWindowTitle(@ATitle);
  QtFileDialog.setDirectory(GetUtf8String(AFileDialog.InitialDir));
  QtFileDialog.setSizeGripEnabled(ofEnableSizing in TSelectDirectoryDialog(AFileDialog).Options);

  if ofViewDetail in TSelectDirectoryDialog(AFileDialog).Options then
    QtFileDialog.setViewMode(QFileDialogDetail)
  else
    QtFileDialog.setViewMode(QFileDialogList);
  {$ifndef QT_NATIVE_DIALOGS}
  // set kbd shortcuts in case when we are not native dialog.
  QtFileDialog.setShortcuts(False);
  {$endif}
end; }

class function TMuiWSSelectDirectoryDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;

begin

end;

{------------------------------------------------------------------------------
  Function: TMuiWSSelectDirectoryDialog.ShowModal
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TMuiWSSelectDirectoryDialog.ShowModal(const ACommonDialog: TCommonDialog);

begin

end;

{ TMuiWSColorDialog }

class function TMuiWSColorDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin
  Result := 1;
end;

{------------------------------------------------------------------------------
  Function: TMuiWSColorDialog.ShowModal
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
 
function ColLongWord(c: Byte): LongWord;
begin
  Result := c or (c shl 8) or (c shl 16) or (c shl 24);
end;
 
class procedure TMuiWSColorDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  ColorDialog: TColorDialog absolute ACommonDialog; 
  AppTags: TTagsList;
  GrpTags: TTagsList;
  BGrpTags: TTagsList;
  WinTags: TTagsList;
  PalTags: TTagsList;
  LocalApp: PObject_; 
  Win: PObject_;
  Palette: PObject_;
  Group: PObject_;
  BGroup: PObject_;
  but1, but2: PObject_;
  sigs: LongWord;
  Res: Integer;
  r,g,b: LongWord;
  DefWidth, DefHeight: Integer;
begin 
  R := ColLongWord(Red(ColorDialog.Color));
  G := ColLongWord(Green(ColorDialog.Color));
  B := ColLongWord(Blue(ColorDialog.Color));
  //
  AddTags(PalTags, [
    MUIA_Coloradjust_Red, R,
    MUIA_Coloradjust_Green, G,
    MUIA_Coloradjust_Blue, B
  ]);
  Palette := MUI_NewObjectA(MUIC_ColorAdjust, GetTagPtr(PalTags));

  but1 := MUI_MakeObject(MUIO_Button, [PChar('OK')]);
  but2 := MUI_MakeObject(MUIO_Button, [PChar('Cancel')]);
  
  AddTags(BGrpTags, [
    MUIA_Group_Child, but1,
    MUIA_Group_Child, but2,
    MUIA_Group_HorizSpacing, 20,
    MUIA_Group_Horiz, True]);
  BGroup := MUI_NewObjectA(MUIC_Group, GetTagPtr(BGrpTags));
  
  AddTags(GrpTags, [
    MUIA_Group_Child, Palette,
    MUIA_Group_Child, BGroup,
    MUIA_Group_Horiz, False]);
  
  Group := MUI_NewObjectA(MUIC_Group, GetTagPtr(GrpTags));
  
  DefWidth := 300;
  DefHeight := 300;
  
  if ColorDialog.Width > 0 then
    DefWidth := ColorDialog.Width;
  if ColorDialog.Height > 0 then
    DefHeight := ColorDialog.Height;
  
  AddTags(WinTags, [
    MUIA_Window_Title, PChar(ColorDialog.Title),
    MUIA_Window_RootObject, Group,
    MUIA_Window_Width, DefWidth,
    MUIA_Window_Height, DefHeight]);
  Win := MUI_NewObjectA(MUIC_Window, GetTagPtr(WinTags));  
  
  AddTags(AppTags, [MUIA_Application_Window, Win]);
  LocalApp := MUI_NewObjectA(MUIC_Application, GetTagPtr(AppTags));
  
  CallHook(PHook(OCLASS(Win)), Win,
    [MUIM_Notify, MUIA_Window_CloseRequest, True,
    LocalApp, 2, MUIM_Application_ReturnID, MUIV_Application_ReturnID_Quit]);
  
  CallHook(PHook(OCLASS(Win)), but2,
    [MUIM_Notify, MUIA_Pressed, True,
    LocalApp, 2, MUIM_Application_ReturnID, MUIV_Application_ReturnID_Quit]);
  
  CallHook(PHook(OCLASS(Win)), but1,
    [MUIM_Notify, MUIA_Pressed, True,
    LocalApp, 2, MUIM_Application_ReturnID, 42]);
  
  SetAttrs(Win, [MUIA_Window_Open, True, TAG_END]);
  Res := -1; 
  while true  do
  begin
    Res := Integer(CallHook(PHook(OCLASS(localapp)), LocalApp, [MUIM_Application_NewInput, @sigs]));
    case Res of
      MUIV_Application_ReturnID_Quit: begin
        ACommonDialog.UserChoice := mrCancel;
        Break;
      end;  
      42: begin
        ACommonDialog.UserChoice := mrOK; 
        Break;
      end;  
    end;
    if sigs <> 0 then
    begin
      sigs := Wait(sigs or SIGBREAKF_CTRL_C);
      if (Sigs and SIGBREAKF_CTRL_C) <> 0 then
        Break;
    end;
  end;
  MUI_DisposeObject(LocalApp);
  
  GetAttr(MUIA_Coloradjust_Red, Palette, @R);
  GetAttr(MUIA_Coloradjust_Green, Palette, @G);
  GetAttr(MUIA_Coloradjust_Blue, Palette, @B);
  
  ColorDialog.Color := RGBToColor((R shr 24) and $FF, (G shr 24) and $FF, (B shr 24) and $FF);
end;

{ TMuiWSFontDialog }

class function TMuiWSFontDialog.CreateHandle(const ACommonDialog: TCommonDialog
  ): THandle;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: TMuiWSFontDialog.ShowModal
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TMuiWSFontDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  //ReturnFont, CurrentFont: QFontH;
  ReturnBool: Boolean;
  Str: WideString;
begin
  {------------------------------------------------------------------------------
    Code to call the dialog
   ------------------------------------------------------------------------------}
  {CurrentFont := TQtFont(TFontDialog(ACommonDialog).Font.Reference.Handle).Widget;

  ReturnFont := QFont_create;
  try
    QFontDialog_getFont(ReturnFont, @ReturnBool, CurrentFont,
      TMuiWSCommonDialog.GetDialogParent(ACommonDialog));
   
    QFont_family(ReturnFont, @Str);
    TFontDialog(ACommonDialog).Font.Name := UTF16ToUTF8(Str);
   
    if QFont_pixelSize(ReturnFont) = -1 then
      TFontDialog(ACommonDialog).Font.Size := QFont_pointSize(ReturnFont)
    else
      TFontDialog(ACommonDialog).Font.Height := QFont_pixelSize(ReturnFont);
      
    TFontDialog(ACommonDialog).Font.Style := [];
   
   if QFont_bold(ReturnFont) then
     TFontDialog(ACommonDialog).Font.Style := TFontDialog(ACommonDialog).Font.Style + [fsBold];
   
   if QFont_italic(ReturnFont) then
     TFontDialog(ACommonDialog).Font.Style := TFontDialog(ACommonDialog).Font.Style + [fsItalic];
   
   if QFont_strikeOut(ReturnFont) then
     TFontDialog(ACommonDialog).Font.Style := TFontDialog(ACommonDialog).Font.Style + [fsStrikeOut];
   
   if QFont_underline(ReturnFont) then
     TFontDialog(ACommonDialog).Font.Style := TFontDialog(ACommonDialog).Font.Style + [fsUnderline];
   
   if QFont_fixedPitch(ReturnFont) then
     TFontDialog(ACommonDialog).Font.Pitch := fpFixed
   else
     TFontDialog(ACommonDialog).Font.Pitch := fpDefault;
   
  finally
    QFont_destroy(ReturnFont);
  end;

  if ReturnBool then
    ACommonDialog.UserChoice := mrOk
  else
    ACommonDialog.UserChoice := mrCancel;}
end;

end.
