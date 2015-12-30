unit SVNUpdateToRevision;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, ComCtrls;

type

  { TSVNUpdateToRevisionForm }

  TSVNUpdateToRevisionForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ComboBox2: TComboBox;
    UpdateDepthGroupBox: TGroupBox;
    GroupBox3: TGroupBox;
    HEADRadioButton: TRadioButton;
    OmmitExternalsCheckBox: TCheckBox;
    RevisionNumberEdit: TEdit;
    RevisionRadioButton: TRadioButton;
    ShowLogButton: TButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure ShowLogButtonClick(Sender: TObject);
  private
    FRepositoryPath: string;
    { private declarations }
  public
    {path the root of the local working copy}
    property RepositoryPath: string read FRepositoryPath write FRepositoryPath;
  end;

var
  SVNUpdateToRevisionForm: TSVNUpdateToRevisionForm;

procedure ShowSVNUpdateToRevisionFrm(ARepoDirectory: string);

implementation

{$R *.lfm}

uses
  SVNClasses, SVNLogForm, SVNUpdateForm;

procedure ShowSVNUpdateToRevisionFrm(ARepoDirectory: string);
begin
  if not Assigned(SVNUpdateToRevisionForm) then
    SVNUpdateToRevisionForm := TSVNUpdateToRevisionForm.Create(nil);

  SVNUpdateToRevisionForm.RepositoryPath := ARepoDirectory;
  SVNUpdateToRevisionForm.Show;
end;

{ TSVNUpdateToRevisionForm }

procedure TSVNUpdateToRevisionForm.ShowLogButtonClick(Sender: TObject);
var
  li: TListItem;
begin
  if not Assigned(SVNLogFrm) then
    SVNLogFrm := TSVNLogFrm.Create(nil);

  SVNLogFrm.RepositoryPath := RepositoryPath;
  SVNLogFrm.ShowModal;

  li := SVNLogFrm.LogListView.Selected;
  if Assigned(li) then
  begin
    RevisionNumberEdit.Text := li.Caption;
    RevisionRadioButton.Checked := True;
  end;
end;

procedure TSVNUpdateToRevisionForm.FormCreate(Sender: TObject);
begin

end;

procedure TSVNUpdateToRevisionForm.OKButtonClick(Sender: TObject);
var
  o: string;
  r: string;
begin
  if OmmitExternalsCheckBox.Checked then
    o := ' --ignore-externals '
  else
    o := '';

  if RevisionRadioButton.Checked and (RevisionNumberEdit.Text <> '') then
    r := ' --revision ' + RevisionNumberEdit.Text
  else
    r := '';

  ShowSVNUpdateFrm(RepositoryPath,
    Format('%s update %s %s "%s"',
    [SVNExecutable,
    r,
    o,
    RepositoryPath]));

  Close;
end;

procedure TSVNUpdateToRevisionForm.FormActivate(Sender: TObject);
begin
  RevisionNumberEdit.Text := '';
end;

procedure TSVNUpdateToRevisionForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

end.

