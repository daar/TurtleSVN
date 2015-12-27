unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, ShellCtrls, SysUtils,
  ExtCtrls, ComCtrls, Menus;

type

  { TMainForm }

  TMainForm = class(TForm)
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    FileMenuItem: TMenuItem;
    DiffMenuItem: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    CheckOutMenuItem: TMenuItem;
    AddMenuItem: TMenuItem;
    RevertMenuItem: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    DeleteMenuItem: TMenuItem;
    MenuItem4: TMenuItem;
    LogMenuItem: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    UpdateMenuItem: TMenuItem;
    CommitMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    ShellListView: TShellListView;
    ShellTreeView: TShellTreeView;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    PUPMenu: TPopupMenu;
    procedure AddMenuItemClick(Sender: TObject);
    procedure CheckOutMenuItemClick(Sender: TObject);
    procedure CommitMenuItemClick(Sender: TObject);
    procedure DiffMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LogMenuItemClick(Sender: TObject);
    procedure DeleteMenuItemClick(Sender: TObject);
    procedure RevertMenuItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure UpdateMenuItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  SVNClasses, SVNLogForm, SVNDiffForm, SVNStatusForm, SVNUpdateForm,
  SVNCheckout;

{ TMainForm }

procedure TMainForm.QuitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.UpdateMenuItemClick(Sender: TObject);
begin
  ShowSVNUpdateFrm(ShellTreeView.Path, SVNExecutable + ' update "' + ShellTreeView.Path + '" --non-interactive');
end;

procedure TMainForm.LogMenuItemClick(Sender: TObject);
begin
  ShowSVNLogFrm(ShellTreeView.Path);
end;

procedure TMainForm.DeleteMenuItemClick(Sender: TObject);
var
  i: integer;
  filename: string;
begin
  for i := 0 to ShellListView.Items.Count - 1 do
    if ShellListView.Items.Item[i].Selected then
    begin
      filename := IncludeTrailingPathDelimiter(ShellListView.Root) +
        ShellListView.Items.Item[i].Caption;

      if FileExists(filename) then
        ExecuteSvnCommand('remove --keep-local', ExtractFilePath(filename), filename);
    end;
end;

procedure TMainForm.RevertMenuItemClick(Sender: TObject);
var
  i: integer;
  filename: string;
begin
  for i := 0 to ShellListView.Items.Count - 1 do
    if ShellListView.Items.Item[i].Selected then
    begin
      filename := IncludeTrailingPathDelimiter(ShellListView.Root) +
        ShellListView.Items.Item[i].Caption;

      if FileExists(filename) then
        ExecuteSvnCommand('revert', ExtractFilePath(filename), filename);
    end;
end;

procedure TMainForm.DiffMenuItemClick(Sender: TObject);
var
  fl: TStringList;
  i: integer;
begin
  fl := TStringList.Create;

  for i := 0 to ShellListView.Items.Count - 1 do
    if ShellListView.Items.Item[i].Selected then
      fl.Append(IncludeTrailingPathDelimiter(ShellListView.Root) +
        ShellListView.Items.Item[i].Caption);

  ShowSVNDiffFrm('', fl);
  fl.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
{$IFDEF MSWINDOWS}
var
  PIDL: PItemIDList;
  Folder: array[0..MAX_PATH] of char;
const
  CSIDL_PERSONAL = $0005;
{$ENDIF}
var
  homedir: string;
  sl: TStringList;
  Node: TTreeNode;
  i: integer;
begin
  {$IFDEF MSWINDOWS}
  SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, PIDL);
  SHGetPathFromIDList(PIDL, Folder);
  homedir := Folder;
  {$ENDIF}
  {$IFDEF UNIX}
  homedir := expandfilename('~/');
  {$ENDIF}

  ShellTreeView.Path := homedir;

  //there seems to be a bug when setting the path in ShellTreeView in linux
  //therefore I have implemented my own code here
  //remove once fixed
  sl := TStringList.Create;
  sl.Delimiter := PathDelim;
  sl.StrictDelimiter := True;
  sl.DelimitedText := homedir;

  Node := ShellTreeView.Items.GetFirstVisibleNode;

  if sl[0] = '' then
    sl.Delete(0);
  if sl[sl.Count - 1] = '' then
    sl.Delete(sl.Count - 1);

  for i := 0 to sl.Count - 1 do
  begin
    //ShowMessage(Node.Text); // [b]<- the code always fails here in the second iteration[/b]
    while (Node.Text <> sl[i]) and (Node.GetNext <> nil) do
      Node := Node.GetNext;

    if (Node.Text = sl[i]) and (i < sl.Count - 1) then
    begin
      //Node := Node.GetFirstChild;
      Node.Selected := True;
      Node.Expand(False);
    end;
  end;
  Node.Selected := True;
  Node.Expand(False);
end;

procedure TMainForm.CommitMenuItemClick(Sender: TObject);
begin
  ShowSVNStatusFrm(ShellTreeView.Path);
end;

procedure TMainForm.CheckOutMenuItemClick(Sender: TObject);
begin
  ShowSVNCheckoutFrm(ShellTreeView.Path);
end;

procedure TMainForm.AddMenuItemClick(Sender: TObject);
begin
  if Sender.ClassName = 'TShellTreeView' then
  begin
    //add folder recursively

  end
  else

end;

end.
