unit SettingsManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, INIFiles;

type

  { TSettingsManager }

  TSettingsManager = class
  private
    FFilename: string;
    Settings: TINIFile;

    function GetCheckoutURL: TStrings;
    function GetCommitMsg: TStrings;
    procedure SetCheckoutURL(AValue: TStrings);
    procedure SetCommitMsg(AValue: TStrings);
  public
    constructor Create(AFileName: string);
    destructor Destroy;

    property CommitMsg: TStrings read GetCommitMsg write SetCommitMsg;
    property CheckoutURL: TStrings read GetCheckoutURL write SetCheckoutURL;
  end;

var
  SettingsMgr: TSettingsManager = nil;

implementation

{ TSettingsManager }

function TSettingsManager.GetCommitMsg: TStrings;
var
  index: integer = 0;
  s: String;
begin
  Result := TStringList.Create;
  repeat
    s := Settings.ReadString('CommitMsg', 'Msg' + IntToStr(index), '');
    if s <> '' then
      Result.Add(s);
    inc(index);
  until s = '';
end;

function TSettingsManager.GetCheckoutURL: TStrings;
var
  index: integer = 0;
  s: String;
begin
  Result := TStringList.Create;
  repeat
    s := Settings.ReadString('CheckoutURL', 'URL' + IntToStr(index), '');
    if s <> '' then
      Result.Add(s);
    inc(index);
  until s = '';
end;

procedure TSettingsManager.SetCheckoutURL(AValue: TStrings);
var
  index: integer;
begin
  for index := 0 to AValue.Count - 1 do
    Settings.WriteString('CheckoutURL', 'URL' + IntToStr(index), AValue[index]);
end;

procedure TSettingsManager.SetCommitMsg(AValue: TStrings);
var
  index: integer;
begin
  for index := 0 to AValue.Count - 1 do
    Settings.WriteString('CommitMsg', 'Msg' + IntToStr(index), AValue[index]);
end;

constructor TSettingsManager.Create(AFileName: string);
begin
  FFileName := AFileName;
  Settings := TINIFile.Create(FFileName);
end;

destructor TSettingsManager.Destroy;
begin
  Settings.UpdateFile;
  Settings.Free;
end;

initialization
  SettingsMgr := TSettingsManager.Create(GetAppConfigDir(False) + 'turtlesvn.cfg');

finalization
  SettingsMgr.Free;

end.
