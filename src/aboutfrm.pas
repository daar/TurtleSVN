{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit AboutFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCAdds, Forms, Controls, Graphics, StdCtrls, Buttons,
  ExtCtrls, Menus, LCLIntf,
  Clipbrd, ButtonPanel, LazFileUtils;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    BuildDateLabel: TLabel;
    ButtonPanel1: TButtonPanel;
    DocumentationURLLabel: TLabel;
    FPCVersionLabel: TLabel;
    DeveloperLabel: TLabel;
    AppNameLabel: TLabel;
    LazarusVersionLabel: TLabel;
    LogoImage: TImage;
    OfficialURLLabel: TLabel;
    VersionLabel: TLabel;
    procedure AboutFormCreate(Sender: TObject);
    procedure URLLabelMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure URLLabelMouseEnter(Sender: TObject);
    procedure URLLabelMouseLeave(Sender: TObject);
  private
  public
  end;

function ShowAboutForm: TModalResult;

const
  TurtleSVNVersion = {$i turtlesvnver.inc};

{$i revision.inc}

var
  LazarusRevisionStr: string;

implementation

{$R *.lfm}

function ShowAboutForm: TModalResult;
var
  AboutForm: TAboutForm;
begin
  AboutForm := TAboutForm.Create(nil);
  Result := AboutForm.ShowModal;
  AboutForm.Free;
end;

{ TAboutForm }

procedure TAboutForm.AboutFormCreate(Sender: TObject);
  {The compiler generated date string is always of the form y/m/d.
   This function gives it a string respresentation according to the
   shortdateformat}
  function GetLocalizedBuildDate(): string;
  var
    BuildDate: string;
    SlashPos1, SlashPos2: integer;
    Date: TDateTime;
  begin
    BuildDate := {$I %date%};
    SlashPos1 := Pos('/', BuildDate);
    SlashPos2 := SlashPos1 + Pos('/', Copy(BuildDate, SlashPos1 + 1,
      Length(BuildDate) - SlashPos1));
    Date := EncodeDate(StrToWord(Copy(BuildDate, 1, SlashPos1 - 1)),
      StrToWord(Copy(BuildDate, SlashPos1 + 1, SlashPos2 - SlashPos1 - 1)),
      StrToWord(Copy(BuildDate, SlashPos2 + 1, Length(BuildDate) - SlashPos2)));
    Result := FormatDateTime(ShortDateFormat, Date);
  end;

begin
  VersionLabel.Caption := Format('%s (%s)', [TurtleSVNVersion, RevisionStr]);
  BuildDateLabel.Caption := 'Build date: ' + GetLocalizedBuildDate;
  FPCVersionLabel.Caption := 'FPC: ' + {$I %FPCVERSION%};
  LazarusVersionLabel.Caption := 'Lazarus: ' + {$I version.inc};

  OfficialURLLabel.Caption := 'http://daar.github.io/TurtleSVN/';
  DocumentationURLLabel.Caption := 'https://github.com/daar/TurtleSVN';
end;

procedure TAboutForm.URLLabelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  OpenURL(TLabel(Sender).Caption);
end;

procedure TAboutForm.URLLabelMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [];
  TLabel(Sender).Font.Color := clBlue;
  TLabel(Sender).Cursor := crDefault;
end;

procedure TAboutForm.URLLabelMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsUnderLine];
  TLabel(Sender).Font.Color := clRed;
  TLabel(Sender).Cursor := crHandPoint;
end;

end.

