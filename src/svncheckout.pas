unit SVNCheckout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, ExtCtrls, INIFiles;

type

  { TSVNCheckoutForm }

  TSVNCheckoutForm = class(TForm)
    RepositoryBrowserButton: TButton;
    CheckoutDirectoryButton: TButton;
    Button3: TButton;
    Button4: TButton;
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
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
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
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

procedure ShowSVNCheckoutFrm;

var
  SVNCheckoutForm: TSVNCheckoutForm;

implementation

{$R *.lfm}

uses
  SVNClasses, SVNUpdateForm;

procedure ShowSVNCheckoutFrm;
begin
  if not Assigned(SVNCheckoutForm) then
    SVNCheckoutForm := TSVNCheckoutForm.Create(nil);

  SVNCheckoutForm.Show;
end;

{ TSVNCheckoutForm }

procedure TSVNCheckoutForm.FormCreate(Sender: TObject);
var
  Config: TINIFile;
  count: LongInt;
  i: LongInt;
  s: String;
begin
  try
    Config := TINIFile.Create('turtlesvn.ini');

    count := Config.ReadInteger('Checkout', 'URLCount', 0);

    //limit to 100 entries
    if count > 100 then
      count := 100;

    for i := count downto 0 do
    begin
      s := Config.ReadString('Checkout', 'URL' + IntToStr(i), '');
      if s <> '' then
        URLComboBox.Items.Add(s);
    end;


  finally
    Config.Free;
  end;
end;

procedure TSVNCheckoutForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  Config: TINIFile;
  i: Integer;
begin
    try
      Config := TINIFile.Create('turtlesvn.ini');

      //write all URLs to INI
      Config.WriteInteger('Checkout', 'URLCount', URLComboBox.Items.Count + 1);
      Config.WriteString('Checkout', 'URL1', URLComboBox.Text);
      for i := 0 to URLComboBox.Items.Count - 1 do
        Config.WriteString('Checkout', 'URL' + IntToStr(i+2), URLComboBox.Items[i]);
    finally
      Config.Free;
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
  CheckoutDirectoryEdit.Text := '';
  RevisionNumberEdit.Text := '';
end;

procedure TSVNCheckoutForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSVNCheckoutForm.OKButtonClick(Sender: TObject);
begin
  ShowSVNUpdateFrm(URLComboBox.Text, SVNExecutable + ' checkout "' + URLComboBox.Text + '" "' + CheckoutDirectoryEdit.Text + '"');
  Close;
end;

end.

