unit SettingsDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ButtonPanel, ExtCtrls, StdCtrls;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    ButtonPanel: TButtonPanel;
    CategoryPanel: TPanel;
    CategoryTree: TTreeView;
    CatTVSplitter: TSplitter;
    EditorsPanel: TScrollBox;
    FilterEdit: TTreeFilterEdit;
    ImageList1: TImageList;
    SettingsPanel: TPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure CategoryTreeChange(Sender: TObject; Node: TTreeNode);
    procedure OKButtonClick(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
  private
    { private declarations }
    ActiveFrame: TFrame;
  public
    { public declarations }
  end;

var
  SettingsForm: TSettingsForm;

procedure ShowSettingsFrm;

implementation

{$R *.lfm}

uses
  GeneralOptionsFrm, AdvancedOptionsFrm;

procedure ShowSettingsFrm;
begin
  if not Assigned(SettingsForm) then
    SettingsForm := TSettingsForm.Create(nil);

  SettingsForm.Show;
end;

{ TSettingsForm }

procedure TSettingsForm.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin

end;

procedure TSettingsForm.CategoryTreeChange(Sender: TObject; Node: TTreeNode);
begin
  if not Assigned(Node) then
    exit;

  if Assigned(ActiveFrame) then
    ActiveFrame.Free;

  case Node.Text of
    'General': ActiveFrame := TGeneralOptionsFrame.Create(EditorsPanel);
    'Advanced': ActiveFrame := TAdvancedOptionsFrame.Create(EditorsPanel);
  end;

  ActiveFrame.Parent := EditorsPanel;
  ActiveFrame.Align := alClient;
end;

procedure TSettingsForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSettingsForm.OKButtonClick(Sender: TObject);
begin
  Close;
end;

end.
