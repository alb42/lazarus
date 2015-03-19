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
  exec, asl, utility, tagsarray, mui,
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

  AddTags(TagsList, [
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

  (*end else
  begin
    {$ifdef QT_NATIVE_DIALOGS}
    saveFilter := GetQtFilterString(TOpenDialog(ACommonDialog), selectedFilter);
    saveFileName := GetUtf8String(FileDialog.InitialDir+FileDialog.Filename);
    saveTitle := GetUTF8String(FileDialog.Title);

    Flags := 0;
    if (ofReadOnly in TOpenDialog(FileDialog).Options) then
      Flags := Flags or QFileDialogReadOnly;

    if (ofAllowMultiSelect in TOpenDialog(FileDialog).Options) then
    begin
      ReturnText := '';
      ReturnList := QStringList_create;
      {$IFDEF HASX11}
      Clipboard.BeginX11SelectionLock;
      {$ENDIF}
      try
        QFileDialog_getOpenFileNames(ReturnList,
          QWidget_parentWidget(QtFileDialog.Widget), @SaveTitle, @saveFileName,
          @saveFilter, @selectedFilter, Flags);
        FileDialog.Files.Clear;
        for i := 0 to QStringList_size(ReturnList) - 1 do
        begin
          QStringList_at(ReturnList, @ReturnText, i);
          FileDialog.Files.Add(UTF16ToUTF8(ReturnText));
          if i = 0 then
            FileDialog.FileName := UTF16ToUTF8(ReturnText);
        end;
        {assign to ReturnText first filename}
        if QStringList_size(ReturnList) > 0 then
          QStringList_at(ReturnList, @ReturnText, 0);

      finally
        QStringList_destroy(ReturnList);
        {$IFDEF HASX11}
        Clipboard.EndX11SelectionLock;
        {$ENDIF}
      end;
    end else
    begin
      {$IFDEF HASX11}
      Clipboard.BeginX11SelectionLock;
      try
      {$ENDIF}
        QFileDialog_getOpenFileName(@ReturnText,
          QWidget_parentWidget(QtFileDialog.Widget), @SaveTitle, @saveFileName,
          @saveFilter, @selectedFilter, Flags);
      {$IFDEF HASX11}
      finally
        Clipboard.EndX11SelectionLock;
      end;
      {$ENDIF}
    end;

    if ReturnText <> '' then
    begin
      FileDialog.FileName := UTF16ToUTF8(ReturnText);
      FileDialog.UserChoice := mrOK;
    end else
      FileDialog.UserChoice := mrCancel;
    {$else}
    FileDialog.UserChoice := QtDialogCodeToModalResultMap[QDialogDialogCode(QtFileDialog.exec)];
    ReturnList := QStringList_create;
    try
      QtFileDialog.selectedFiles(ReturnList);
      FileDialog.Files.Clear;
      for i := 0 to QStringList_size(ReturnList) - 1 do
      begin
        QStringList_at(ReturnList, @ReturnText, i);
        FileDialog.Files.Add(UTF16ToUTF8(ReturnText));
        if i = 0 then
          FileDialog.FileName := UTF16ToUTF8(ReturnText);
      end;
      ReturnText := FileDialog.Files.Text;
    finally
      QStringList_destroy(ReturnList);
    end;
    {$endif}

  end;*)

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
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: TMuiWSColorDialog.ShowModal
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TMuiWSColorDialog.ShowModal(const ACommonDialog: TCommonDialog);
{var
  AColor: TColorRef;
  AQColor: TQColor;
  AQtColor: QColorH;
  ARgb: QRgb;
  ReturnBool: Boolean;
  ColorDialog: TColorDialog absolute ACommonDialog;

  procedure FillCustomColors;
  var
    i, AIndex, CustomColorCount: integer;
    AColor: TColor;
  begin
    CustomColorCount := QColorDialog_customCount();
    for i := 0 to ColorDialog.CustomColors.Count - 1 do
      if ExtractColorIndexAndColor(ColorDialog.CustomColors, i, AIndex, AColor) then
        if AIndex < CustomColorCount then
          QColorDialog_setCustomColor(AIndex, QRgb(AColor));
  end;
 }
begin
  {AColor := ColorToRgb(ColorDialog.Color);
  AQColor.Alpha := $FFFF;
  AQColor.ColorSpec := 1;
  AQColor.Pad := 0;
  ColorRefToTQColor(AColor, AQColor);
  AQtColor := QColor_create(PQColor(@AQColor));
  ARgb := QColor_rgba(AQtColor);
  FillCustomColors;

  ARgb := QColorDialog_getRgba(ARgb, @ReturnBool,
    TMuiWSCommonDialog.GetDialogParent(ACommonDialog));

  QColor_fromRgba(PQColor(AQtColor), ARgb);
  try
    QColor_toRgb(AQtColor, @AQColor);
    TQColorToColorRef(AQColor, AColor);
    ColorDialog.Color := TColor(AColor);
  finally
    QColor_destroy(AQtColor);
  end;

  if ReturnBool then
    ACommonDialog.UserChoice := mrOk
  else
    ACommonDialog.UserChoice := mrCancel;}
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
