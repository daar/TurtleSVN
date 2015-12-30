unit SVNCheckout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, ExtCtrls, ComCtrls;

type

  { TSVNCheckoutForm }

  TSVNCheckoutForm = class(TForm)
    RepositoryBrowserButton: TButton;
    CheckoutDirectoryButton: TButton;
    ChooseItemsButton: TButton;
    ShowLogButton: TButton;
    ButtonPanel1: TButtonPanel;
    OmmitExternalsCheckBox: TCheckBox;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    URLComboBox: TComboBox;
    ComboBox2: TComboBox;
    CheckoutDirectoryEdit: TEdit;
    RevisionNumberEdit: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    RevisionRadioButton: TRadioButton;
    HEADRadioButton: TRadioButton;
    procedure ChooseItemsButtonClick(Sender: TObject);
    procedure ShowLogButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure RepositoryBrowserButtonClick(Sender: TObject);
    procedure CheckoutDirectoryButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

procedure ShowSVNCheckoutFrm(ACheckoutDirectory: string);

var
  SVNCheckoutForm: TSVNCheckoutForm;

implementation

{$R *.lfm}

uses
  Math, SettingsManager, SVNClasses, SVNUpdateForm, SVNLogForm;

procedure ShowSVNCheckoutFrm(ACheckoutDirectory: string);
begin
  if not Assigned(SVNCheckoutForm) then
    SVNCheckoutForm := TSVNCheckoutForm.Create(nil);

  SVNCheckoutForm.CheckoutDirectoryEdit.Text := ACheckoutDirectory;
  SVNCheckoutForm.Show;
end;

{ TSVNCheckoutForm }

procedure TSVNCheckoutForm.FormCreate(Sender: TObject);
begin
  URLComboBox.Items.AddStrings(SettingsMgr.CheckoutURL);
end;

procedure TSVNCheckoutForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  index: integer;
  URL: TStrings;
begin
  try
    //write all URLs to INI
    URL := TStringList.Create;
    URL.AddStrings(URLComboBox.Items);

    //delete a previous same mesage from the list, so we don't store duplicates
    index := URL.IndexOf(URLComboBox.Text);
    if index <> -1 then
      URL.Delete(index);

    URL.Insert(0, URLComboBox.Text);

    //limit to 100 entries
    while URL.Count > 99 do
      URL.Delete(100);

    SettingsMgr.CheckoutURL := URL;
  finally
    URL.Free;
  end;
end;

procedure TSVNCheckoutForm.CheckoutDirectoryButtonClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
    CheckoutDirectoryEdit.Text := SelectDirectoryDialog.FileName;
end;

procedure TSVNCheckoutForm.RepositoryBrowserButtonClick(Sender: TObject);
begin
  //opens the repository browser
end;

procedure TSVNCheckoutForm.FormActivate(Sender: TObject);
begin
  URLComboBox.Text := '';
  RevisionNumberEdit.Text := '';
end;

procedure TSVNCheckoutForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSVNCheckoutForm.ShowLogButtonClick(Sender: TObject);
var
  li: TListItem;
begin
  if not Assigned(SVNLogFrm) then
    SVNLogFrm := TSVNLogFrm.Create(nil);

  SVNLogFrm.RepositoryPath := URLComboBox.Text;
  SVNLogFrm.ShowModal;

  li := SVNLogFrm.LogListView.Selected;
  if Assigned(li) then
  begin
    RevisionNumberEdit.Text := li.Caption;
    RevisionRadioButton.Checked := True;
  end;
end;

procedure TSVNCheckoutForm.ChooseItemsButtonClick(Sender: TObject);
begin
  //opens the repository browser
end;

procedure TSVNCheckoutForm.OKButtonClick(Sender: TObject);
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

  ShowSVNUpdateFrm(URLComboBox.Text,
    Format('%s checkout %s %s "%s" "%s"',
    [SVNExecutable,
    r,
    o, URLComboBox.Text,
    CheckoutDirectoryEdit.Text]));

  Close;
end;

end.



