unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, ShellCtrls, SysUtils, Process,
  ExtCtrls, ComCtrls, Menus, StdCtrls, Buttons, FileUtil, Utils;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnGo: TBitBtn;
    edtPath: TEdit;
    ShellListView: TListView;
    ImageList1: TImageList;
    ImageList2: TImageList;
    Label1: TLabel;
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
    CleanupMenuItem: TMenuItem;
    Panel1: TPanel;
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
    Splitter1: TSplitter;
    ShellTreeView: TShellTreeView;
    UpdateMenuItem: TMenuItem;
    CommitMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    StatusBar: TStatusBar;
    PUPMenu: TPopupMenu;
    procedure AddMenuItemClick(Sender: TObject);
    procedure CheckOutMenuItemClick(Sender: TObject);
    procedure CleanupMenuItemClick(Sender: TObject);
    procedure CommitMenuItemClick(Sender: TObject);
    procedure DiffMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LogMenuItemClick(Sender: TObject);
    procedure DeleteMenuItemClick(Sender: TObject);
    procedure RevertMenuItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure UpdateMenuItemClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure ShellListViewDblClick(Sender: TObject);
    procedure ShellTreeViewChange(Sender: TObject; Node: TTreeNode);
  private
    { private declarations }
    procedure ShowDirectory(ListDir: string);
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;
  CurrentDirectory: string;

implementation

{$R *.lfm}

uses
  SVNClasses, SVNLogForm, SVNDiffForm, SVNStatusForm, SVNUpdateForm,
  SVNCheckout;

{ TMainForm }

procedure TMainForm.btnGoClick(Sender: TObject);
begin
  if DirectoryExistsUTF8(edtPath.Text) then
    ShowDirectory(edtPath.Text)
  else
    edtPath.Text := CurrentDirectory;
end;

procedure TMainForm.ShellListViewDblClick(Sender: TObject);
var
  AProcess: TProcess;
begin
  if ShellListView.Selected.ImageIndex = 1 then
    ShowDirectory(IncludeTrailingPathDelimiter(
      IncludeTrailingPathDelimiter(CurrentDirectory) + ShellListView.Selected.Caption))
  else
  begin
    AProcess := TProcess.Create(nil);
    AProcess.Executable := IncludeTrailingPathDelimiter(CurrentDirectory) +
      ShellListView.Selected.Caption;
    AProcess.Execute;
    AProcess.Free;
  end;

  // we have used ShellExecute to make things easier (from ShellApi unit)
  // but it will only work in windows
  // use TProcess for a cross platform way
end;

procedure TMainForm.ShellTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  ShowDirectory(ShellTreeView.Path);
end;

procedure TMainForm.ShowDirectory(ListDir: string);
var
  Info: TSearchRec;
  Count: longint;
  li: TListItem;
  FileTime: TDateTime;

begin

  ListDir := IncludeTrailingPathDelimiter(ListDir);
  edtPath.Text := ListDir;
  CurrentDirectory := ListDir;
  Count := 0;
  ShellListView.Items.Clear;

  ShellListView.Items.BeginUpdate;

  // This is a try block which finally runs EndUpdate
  try

    // if we have found a file...
    if FindFirstUTF8(ListDir + '*', faAnyFile and faDirectory, Info) = 0 then
    begin

      repeat

        // we increase the count so that we can show it later
        Inc(Count);

        // we do stuff with the file entry we found
        with Info do
        begin

          if (Name <> '.') and (Name <> '..') then
          begin

            li := ShellListView.Items.Add;
            li.Caption := Info.Name;

            // If we find a folder
            if (Attr and faDirectory) <> 0 then
            begin
              li.ImageIndex := 1;
              li.SubItems.Add('--'); // folders can't show size
            end
            // if we find a file
            else
            begin
              li.ImageIndex := 2;
              li.SubItems.Add(ConvertBytes(Info.Size));
            end;

            FileTime := FileDateToDateTime(Info.Time);
            li.SubItems.Add(FormatDateTime('c', FileTime));
          end;

        end;

      until FindNextUTF8(info) <> 0;

    end;

  finally
    ShellListView.Items.EndUpdate;
  end;

  // we are done with file list
  FindCloseUTF8(Info);
  StatusBar.SimpleText := IntToStr(Count) + ' items';
end;

procedure TMainForm.QuitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.UpdateMenuItemClick(Sender: TObject);
begin
  ShowSVNUpdateFrm(ShellTreeView.Path, SVNExecutable + ' update "' +
    ShellTreeView.Path + '" --non-interactive');
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
      filename := IncludeTrailingPathDelimiter(ShellTreeView.Path) +
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
      filename := IncludeTrailingPathDelimiter(ShellTreeView.Path) +
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
      fl.Append(IncludeTrailingPathDelimiter(ShellTreeView.Path) +
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

procedure TMainForm.CleanupMenuItemClick(Sender: TObject);
begin
  ExecuteSvnCommand('cleanup', ShellTreeView.Path, ShellTreeView.Path);
end;

procedure TMainForm.AddMenuItemClick(Sender: TObject);
begin
  if Sender.ClassName = 'TShellTreeView' then
  begin
    //add folder recursively

  end
  else;

end;

end.
