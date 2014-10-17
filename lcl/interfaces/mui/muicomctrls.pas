unit muicomctrls;
{$mode objfpc}{$H+}
interface

uses
  muibaseunit, mui, exec, utility, sysutils;

type

  { TMUIGauge }

  TMUIGauge = class(TMUIArea)
  private
    FMinPos: Integer;
    FMaxPos: Integer;
    FShowText: Boolean;
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
  Result := GetAttribute(LongInt(MUIA_Gauge_Max)) + FMinPos;
end;

function TMUIGauge.GetMinPos: Integer;
begin
  Result := FMinPos;
end;

function TMUIGauge.GetPosition: Integer;
begin
  Result := GetAttribute(LongInt(MUIA_Gauge_Current)) + FMinPos;
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
  Text: string;
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
  SetAttribute([MUIA_Gauge_InfoText, PChar(Text)]);
end;

end.
