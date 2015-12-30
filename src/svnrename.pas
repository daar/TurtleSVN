unit SVNRename;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls;

type

  { TSVNRenameForm }

  TSVNRenameForm = class(TForm)
    SelectLocationButton: TButton;
    ButtonPanel1: TButtonPanel;
    MoveCheckBox: TCheckBox;
    OriginalLocationEdit: TEdit;
    NewLocationEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    procedure SelectLocationButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SVNRenameForm: TSVNRenameForm;

procedure ShowSVNRenameFrm(AFileName: string);

implementation

{$R *.lfm}

uses
  SVNClasses, SVNCommitForm;

procedure ShowSVNRenameFrm(AFileName: string);
begin
  if not Assigned(SVNRenameForm) then
    SVNRenameForm := TSVNRenameForm.Create(nil);

  SVNRenameForm.OriginalLocationEdit.Text := AFileName;
  SVNRenameForm.NewLocationEdit.Text := AFileName;
  SVNRenameForm.Show;
end;

{ TSVNRenameForm }

procedure TSVNRenameForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSVNRenameForm.SelectLocationButtonClick(Sender: TObject);
begin
  SelectDirectoryDialog.FileName := NewLocationEdit.Text;
  if SelectDirectoryDialog.Execute then
    NewLocationEdit.Text := SelectDirectoryDialog.FileName;
end;

procedure TSVNRenameForm.OKButtonClick(Sender: TObject);
begin
  if MoveCheckBox.Checked then
    ShowSVNCommitFrm(Format('%s move %s %s', [SVNExecutable, OriginalLocationEdit.Text, NewLocationEdit.Text]))
  else
    ShowSVNCommitFrm(Format('%s copy %s %s', [SVNExecutable, OriginalLocationEdit.Text, NewLocationEdit.Text]));

  Close;
end;

end.

