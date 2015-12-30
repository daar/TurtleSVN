unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, ShellCtrls, SysUtils, Types, Process,
  ExtCtrls, ComCtrls, Menus, StdCtrls, Buttons, FileUtil, LCLIntf,
  LazFileUtils;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnGo: TBitBtn;
    edtPath: TEdit;
    HelpMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    MenuItem2: TMenuItem;
    OnlineHelpMenuItem: TMenuItem;
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
    SVNHelpMenuItem: TMenuItem;
    MenuItem19: TMenuItem;
    CheckOutMenuItem: TMenuItem;
    AddMenuItem: TMenuItem;
    CleanupMenuItem: TMenuItem;
    Panel1: TPanel;
    RevertMenuItem: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    SettingsMenuItem: TMenuItem;
    SVNAboutMenuItem: TMenuItem;
    DeleteMenuItem: TMenuItem;
    RenameMenuItem: TMenuItem;
    LogMenuItem: TMenuItem;
    UpdateToRevisionMenuItem: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    LocalModificationMenuItem: TMenuItem;
    Splitter1: TSplitter;
    ShellTreeView: TShellTreeView;
    UpdateMenuItem: TMenuItem;
    CommitMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    StatusBar: TStatusBar;
    PUPMenu: TPopupMenu;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure AddMenuItemClick(Sender: TObject);
    procedure CheckOutMenuItemClick(Sender: TObject);
    procedure CleanupMenuItemClick(Sender: TObject);
    procedure CommitMenuItemClick(Sender: TObject);
    procedure DiffMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LocalModificationMenuItemClick(Sender: TObject);
    procedure LogMenuItemClick(Sender: TObject);
    procedure DeleteMenuItemClick(Sender: TObject);
    procedure RenameMenuItemClick(Sender: TObject);
    procedure SettingsMenuItemClick(Sender: TObject);
    procedure SVNHelpMenuItemClick(Sender: TObject);
    procedure SVNAboutMenuItemClick(Sender: TObject);
    procedure PUPMenuPopup(Sender: TObject);
    procedure RevertMenuItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure UpdateMenuItemClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure ShellListViewDblClick(Sender: TObject);
    procedure ShellTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure UpdateToRevisionMenuItemClick(Sender: TObject);
  private
    { private declarations }
    PopupComponent: string;
    PopupPosition: TPoint;
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
  Utils, SVNClasses, SVNLogForm, SVNDiffForm, SVNStatusForm, SVNUpdateForm,
  SVNCheckout, SettingsDialog, SVNUpdateToRevision, AboutFrm, SVNRename,
  SVNLocalStatusForm;

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
  if not Assigned(ShellListView.Selected) then
    exit;

  if ShellListView.Selected.ImageIndex = 1 then
    ShowDirectory(IncludeTrailingPathDelimiter(CurrentDirectory + ShellListView.Selected.Caption))
  else
  begin
    AProcess := TProcess.Create(nil);
    AProcess.Executable := 'xdg-open';
    AProcess.Parameters.Add(CurrentDirectory + ShellListView.Selected.Caption);
    AProcess.Execute;
    AProcess.Free;
  end;
end;

procedure TMainForm.ShellTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  ShowDirectory(ShellTreeView.Path);
end;

procedure TMainForm.UpdateToRevisionMenuItemClick(Sender: TObject);
begin
  if PopupComponent = 'TShellTreeView' then
  begin
    //update folder recursively
    ShowSVNUpdateToRevisionFrm(CurrentDirectory);
    ShowDirectory(CurrentDirectory);
  end
  else
  begin
    if Assigned(ShellListView.Selected) then
    begin
      ShowSVNUpdateToRevisionFrm(CurrentDirectory + ShellListView.Selected.Caption);
      ShowDirectory(CurrentDirectory);
    end;
  end;
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
          //if (Name <> '.') and (Name <> '..') then
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

  ShellListView.AlphaSort;

  // we are done with file list
  FindCloseUTF8(Info);
  StatusBar.SimpleText := IntToStr(Count) + ' items';
end;

procedure TMainForm.QuitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.UpdateMenuItemClick(Sender: TObject);
var
  folder: string;
  pt: TPoint;
  li: TListItem;
begin
  folder := IncludeTrailingPathDelimiter(CurrentDirectory);

  if PopupComponent = 'TListView' then
  begin
    //get listview item from mouse position, this way
    //even when not selected the update can be executed
    pt := ShellListView.ScreenToClient(PopupPosition);
    li := ShellListView.GetItemAt(pt.x, pt.y);

    folder := folder + li.Caption;

    //in case a file was selected, we will update the folder it belongs to
    if not DirectoryExists(folder) then
      folder := ExtractFilePath(folder);
  end;

  ShowSVNUpdateFrm(folder, Format('%s update "%s" --non-interactive',
    [SVNExecutable, folder]));
end;

procedure TMainForm.LogMenuItemClick(Sender: TObject);
begin
  ShowSVNLogFrm(CurrentDirectory);
end;

procedure TMainForm.DeleteMenuItemClick(Sender: TObject);
var
  i: integer;
  filename: string;
begin
  for i := 0 to ShellListView.Items.Count - 1 do
    if ShellListView.Items.Item[i].Selected then
    begin
      filename := IncludeTrailingPathDelimiter(CurrentDirectory) +
        ShellListView.Items.Item[i].Caption;

      if FileExists(filename) then
        ExecuteSvnCommand('remove --keep-local', ExtractFilePath(filename), filename);
    end;
end;

procedure TMainForm.RenameMenuItemClick(Sender: TObject);
begin
  if PopupComponent = 'TShellTreeView' then
  begin
    ShowSVNRenameFrm(CurrentDirectory);
    ShowDirectory(CurrentDirectory);
  end
  else
  begin
    if Assigned(ShellListView.Selected) then
    begin
      ShowSVNRenameFrm(CurrentDirectory + ShellListView.Selected.Caption);
      ShowDirectory(CurrentDirectory);
    end;
  end;
end;

procedure TMainForm.SettingsMenuItemClick(Sender: TObject);
begin
  ShowSettingsFrm;
end;

procedure TMainForm.SVNHelpMenuItemClick(Sender: TObject);
begin
  OpenURL('https://github.com/daar/TurtleSVN/wiki');
end;

procedure TMainForm.SVNAboutMenuItemClick(Sender: TObject);
begin
  ShowAboutForm;
end;

procedure TMainForm.PUPMenuPopup(Sender: TObject);
begin
  //add here context sensitive menu's
  PopupComponent := TPopupMenu(Sender).PopupComponent.ClassName;
  PopupPosition := Mouse.CursorPos;

  if PopupComponent = 'TShellTreeView' then
  begin
    //only treeview items here
  end
  else;
  //only listview items here
end;

procedure TMainForm.RevertMenuItemClick(Sender: TObject);
var
  i: integer;
  filename: string = '';
  filelist: string = '';
  Path: string;
begin
  Path := IncludeTrailingPathDelimiter(CurrentDirectory);

  if PopupComponent = 'TListView' then
  begin
    for i := 0 to ShellListView.Items.Count - 1 do
      if ShellListView.Items.Item[i].Selected then
      begin
        filelist := filelist + ' ' + Path + ShellListView.Items.Item[i].Caption;

        ////check if item is a folder, if so then do a recursive revert
        //if DirectoryExists(filename) then
        //  SVNUpdateForm(filename, 'revert -R')
        //else
        //begin
        //  //check if item is an existing file, if so then ad to file list
        //  if FileExists(Path + ShellListView.Items.Item[i].Caption) then
        //    SVNUpdateForm(filename, 'revert -R');
        //end;
      end;
    ShowSVNUpdateFrm('', SVNExecutable + ' revert -R ' + filelist);
  end
  else
    ShowSVNUpdateFrm('', SVNExecutable + ' revert -R ' + filename);
end;

procedure TMainForm.DiffMenuItemClick(Sender: TObject);
var
  fl: TStringList;
  i: integer;
begin
  fl := TStringList.Create;

  for i := 0 to ShellListView.Items.Count - 1 do
    if ShellListView.Items.Item[i].Selected then
      fl.Append(IncludeTrailingPathDelimiter(CurrentDirectory) +
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

procedure TMainForm.LocalModificationMenuItemClick(Sender: TObject);
var
  folder: string;
  pt: TPoint;
  li: TListItem;
begin
  folder := IncludeTrailingPathDelimiter(CurrentDirectory);

  if PopupComponent = 'TListView' then
  begin
    //get listview item from mouse position, this way
    //even when not selected the update can be executed
    pt := ShellListView.ScreenToClient(PopupPosition);
    li := ShellListView.GetItemAt(pt.x, pt.y);

    folder := folder + li.Caption;

    //in case a file was selected, we will commit the folder it belongs to
    if not DirectoryExists(folder) then
      folder := ExtractFilePath(folder);
  end;

  ShowSVNLocalStatusFrm(folder);
end;

procedure TMainForm.CommitMenuItemClick(Sender: TObject);
var
  folder: string;
  pt: TPoint;
  li: TListItem;
begin
  folder := CurrentDirectory;

  if PopupComponent = 'TListView' then
  begin
    //get listview item from mouse position, this way
    //even when not selected the update can be executed
    pt := ShellListView.ScreenToClient(PopupPosition);
    li := ShellListView.GetItemAt(pt.x, pt.y);

    folder := folder + li.Caption;

    //in case a file was selected, we will commit the folder it belongs to
    if not DirectoryExists(folder) then
      folder := ExtractFilePath(folder);
  end;

  ShowSVNStatusFrm(folder);
end;

procedure TMainForm.CheckOutMenuItemClick(Sender: TObject);
begin
  ShowSVNCheckoutFrm(CurrentDirectory);
end;

procedure TMainForm.CleanupMenuItemClick(Sender: TObject);
begin
  ExecuteSvnCommand('cleanup', CurrentDirectory, CurrentDirectory);
end;

procedure TMainForm.AddMenuItemClick(Sender: TObject);
var
  i: Integer;
  filelist: string = '';
begin
  if PopupComponent = 'TShellTreeView' then
    //add folder recursively
    ShowSVNUpdateFrm(CurrentDirectory, Format('%s add %s', [SVNExecutable, CurrentDirectory]))
  else
  begin
    for i := 0 to ShellListView.Items.Count - 1 do
      if ShellListView.Items.Item[i].Selected then
        filelist := filelist + ' ' + CurrentDirectory + ShellListView.Items.Item[i].Caption;

    ShowSVNUpdateFrm(CurrentDirectory, Format('%s add %s', [SVNExecutable, filelist]));
  end;
end;

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  ShowAboutForm;
end;

end.
