unit muicomctrls;
{$mode objfpc}{$H+}
interface

uses
  controls, muibaseunit, mui, exec, utility, sysutils, strings, tagsarray, Intuition, Types;

type

  { TMUIGauge }

  TMUIGauge = class(TMUIArea)
  private
    FMinPos: Integer;
    FMaxPos: Integer;
    FShowText: Boolean;
    Text: string;
    function GetMaxPos: Integer;
    function GetMinPos: Integer;
    function GetPosition: Integer;
    function GetShowText: boolean;
    procedure SetMaxPos(AValue: Integer);
    procedure SetMinPos(AValue: Integer);
    procedure SetPosition(AValue: Integer);
    procedure SetShowText(AValue: boolean);
    procedure UpdateText;
  public
    Horiz: Boolean;
    constructor Create(AClassName: PChar; Tags: PTagItem); override;
    property Position: Integer read GetPosition write SetPosition;
    property MinPos: Integer read GetMinPos write SetMinPos;
    property MaxPos: Integer read GetMaxPos write SetMaxPos;
    property ShowText: boolean read GetShowText write SetShowText;
  end;
  
  TMUIGroup = class(TMUIArea)
  private
    function GetActivePage: Integer;
    procedure SetActivePage(AValue: Integer);
  protected
    procedure BasicInitOnCreate(); override;  
  public
    property ActivePage: Integer read GetActivePage write SetActivePage;    
  end;
  
  TMUIRegister = class(TMUIGroup)
  private
    
  protected
    procedure BasicInitOnCreate(); override;
      
  public
    FNames: array[0..100] of PChar;
    constructor Create(AClassName: PChar; var TagList: TTagsList); overload; reintroduce; virtual;
    destructor Destroy; override;
    procedure AddChild(APage: TMUIObject); override;
  end;  


implementation

{ TMUIGauge }

constructor TMUIGauge.Create(AClassName: PChar; Tags: PTagItem);
begin
  inherited;
  FMinPos := 0;
  FMaxPos := 100;
end;

function TMUIGauge.GetMaxPos: Integer;
begin
  Result := GetAttribute(IPTR(MUIA_Gauge_Max)) + FMinPos;
end;

function TMUIGauge.GetMinPos: Integer;
begin
  Result := FMinPos;
end;

function TMUIGauge.GetPosition: Integer;
begin
  Result := GetAttribute(IPTR(MUIA_Gauge_Current)) + FMinPos;
end;

function TMUIGauge.GetShowText: boolean;
begin
  Result := FShowText;
end;

procedure TMUIGauge.SetMaxPos(AValue: Integer);
begin
  FMaxPos := AValue;
  if FMaxPos - FMinPos > 0 then
  begin
    SetAttribute([LongInt(MUIA_Gauge_Max), FMaxPos - FMinPos, TAG_END]);
    if FShowText then
      UpdateText;
  end;
end;

procedure TMUIGauge.SetMinPos(AValue: Integer);
begin
  FMinPos := AValue;
  SetMaxPos(FMaxPos);
end;

procedure TMUIGauge.SetPosition(AValue: Integer);
begin
  SetAttribute([LongInt(MUIA_Gauge_Current), AValue - FMinPos, TAG_END]);
  if FShowText then
    UpdateText;
end;

procedure TMUIGauge.SetShowText(AValue: boolean);
begin
  FShowText := AValue;
  UpdateText;
end;

procedure TMUIGauge.UpdateText;
var
  Pos: Integer;
begin
  Text := '';
  if FShowText and ((FMaxPos - FMinPos) > 0) then
  begin
    Pos := Position;
    if not Horiz then
      Text := IntToStr(Pos)
    else
      Text := IntToStr(Pos) + ' from ['+IntToStr(FMinPos)+'-'+IntToStr(FMaxPos)+']('+IntToStr(Round(100*(Pos/(FMaxPos-FMinPos))))+'%%)'
  end;
  SetAttribute([LongInt(MUIA_Gauge_InfoText), PChar(Text)]);
end;

{ TMUIGroup }

function TMUIGroup.GetActivePage: Integer;
begin
  Result := GetAttribute(MUIA_Group_ActivePage);
end;

procedure TMUIGroup.SetActivePage(AValue: Integer);
var
  i: Integer;
begin
  for i := 0 to FChilds.Count - 1 do
  begin
    TMUIObject(FChilds[i]).Visible := i = AValue;
  end;
  SetAttribute([MUIA_Group_ActivePage, AValue]);
end;

procedure TMUIGroup.BasicInitOnCreate();
begin
  inherited;
end;

{ TMUIRegister }

constructor TMUIRegister.Create(AClassName: PChar; var TagList: TTagsList);
var
  i: Integer;
  MyStr: string;
begin
  //writeln('I got it');
  //FNames[0] := GetMem(100);
  //StrCopy(FNames[0], 'Page 1');
  //FNames[1] := GetMem(100);
  //StrCopy(FNames[1], 'Page 2');
  //FNames[2] := GetMem(100);
  //StrCopy(FNames[2], 'Page3');
  //FNames[0] := nil;
  
  //AddTags(TagList, [MUIA_Register_Titles, @FNames[0], MUIA_Register_Frame, True]);
  inherited Create(AClassName, GetTagPtr(TagList));
end;

procedure TMUIRegister.BasicInitOnCreate();
begin
  inherited;
end;

destructor TMUIRegister.Destroy;
var
  i: Integer;
begin
  inherited;
  //for i := 0 to High(FNames) do
  //  if Assigned(FNames[i]) then
  //    FreeMem(FNames[i]);
end;

procedure TMUIRegister.AddChild(APage: TMUIObject);
//var
  //sIdx: Integer;
  //Idx: Integer;
  //i: Integer;
  //MyStr: string;
begin
  
  {writeln('recreate ', FChilds.Count + 1); // Elements  
  for i := 0 to FChilds.Count do
  begin
    if Assigned(FNames[i]) then
      FreeMem(FNames[i]);
    MyStr := 'MyPage ' + IntToStr(i + 1);//APage.Title + #0;
    writeln(MyStr);
    FNames[i] := GetMem(Length(FNames) + 1);
    strings.StrCopy(FNames[i], PChar(MyStr));
  end;
  FNames[FChilds.Count + 1] := nil;}
  inherited;
  ActivePage := FChilds.Count - 1;
  {Idx := FChilds.Count - 1;
  sIdx := High(FNames);
  for i := sIdx to Idx do
  begin
    if not Assigned(FNames[i]) then
    begin
      FNames[i] := GetMem(10);
      MyStr := 'Page ' + IntToStr(i + 1);
      strcopy(MyStr, FNames[i])
    end;      
  end;
  FNames[Idx + 1] := nil;
  }
end;

end.
