{ Copyright (C) 2008 Darius Blaszijk

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit SVNLocalStatusForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, LCLProc,
  Forms, Controls, Dialogs, ComCtrls, StdCtrls, ButtonPanel, ExtCtrls, Menus,
  // LazUtils
  FileUtil, LazFileUtils,
  // LazSvn
  SVNClasses;

type
  { TSVNLocalStatusFrm }

  TSVNLocalStatusFrm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ImageList: TImageList;
    mnuOpen: TMenuItem;
    mnuRemove: TMenuItem;
    mnuAdd: TMenuItem;
    mnuRevert: TMenuItem;
    mnuShowDiff: TMenuItem;
    PopupMenu1: TPopupMenu;
    Splitter: TSplitter;
    SVNFileListView: TListView;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuAddClick(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure mnuRemoveClick(Sender: TObject);
    procedure mnuRevertClick(Sender: TObject);
    procedure mnuShowDiffClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure SVNFileListViewColumnClick(Sender: TObject; Column: TListColumn);
  private
    FRepositoryPath: string;
    SVNStatus: TSVNStatus;
    procedure Initialize({%H-}Data: PtrInt);
    procedure UpdateFilesListView;
    procedure ChangeCursor(ACursor: TCursor);
    procedure UpdateCheckedStatus;
  public
    {path the root of the local working copy}
    property RepositoryPath: string read FRepositoryPath write FRepositoryPath;
  end;

procedure ShowSVNLocalStatusFrm(ARepoPath: string);

var
  SVNLocalStatusFrm: TSVNLocalStatusFrm;

implementation

{$R *.lfm}

uses
  SettingsManager, SVNDiffForm;

procedure ShowSVNLocalStatusFrm(ARepoPath: string);
begin
  if not Assigned(SVNLocalStatusFrm) then
    SVNLocalStatusFrm := TSVNLocalStatusFrm.Create(nil);

  SVNLocalStatusFrm.ChangeCursor(crHourGlass);
  SVNLocalStatusFrm.RepositoryPath:=ARepoPath;
  SVNLocalStatusFrm.Show;
end;

{ TSVNLocalStatusFrm }

procedure TSVNLocalStatusFrm.FormShow(Sender: TObject);
begin
  Caption := Format('%s - %s', ['Working copy', RepositoryPath]);
  Application.QueueAsyncCall(@Initialize, 0);
end;

procedure TSVNLocalStatusFrm.Initialize(Data: PtrInt);
begin
  SVNStatus := TSVNStatus.Create(RepositoryPath, false);
  SVNStatus.Sort(siChecked, sdAscending);
  UpdateFilesListView;
  ChangeCursor(crDefault);
end;

procedure TSVNLocalStatusFrm.mnuRevertClick(Sender: TObject);
begin
  if Assigned(SVNFileListView.Selected) then
  begin
    ExecuteSvnCommand('revert', RepositoryPath, SVNFileListView.Selected.SubItems[0]);

    //now delete the entry from the list
    SVNStatus.List.Delete(SVNFileListView.Selected.Index);

    //update the listview again
    UpdateFilesListView;
  end;
end;

procedure TSVNLocalStatusFrm.mnuAddClick(Sender: TObject);
begin
  if Assigned(SVNFileListView.Selected) then
  begin
    ExecuteSvnCommand('add', RepositoryPath, SVNFileListView.Selected.SubItems[0]);

    // completely re-read the status
    SVNStatus.Free;
    SVNStatus := TSVNStatus.Create(RepositoryPath, false);
    SVNStatus.Sort(siChecked, sdAscending);
    UpdateFilesListView;
  end;
end;

procedure TSVNLocalStatusFrm.mnuOpenClick(Sender: TObject);
var
  FileName: String;
begin
  if Assigned(SVNFileListView.Selected) then
  begin
    FileName := CreateAbsolutePath(SVNFileListView.Selected.SubItems[0], RepositoryPath);
    //TODO: open in default application
  end;
end;

procedure TSVNLocalStatusFrm.mnuRemoveClick(Sender: TObject);
begin
  if Assigned(SVNFileListView.Selected) then
  begin
    ExecuteSvnCommand('remove --keep-local', RepositoryPath, SVNFileListView.Selected.SubItems[0]);

    // completely re-read the status
    SVNStatus.Free;
    SVNStatus := TSVNStatus.Create(RepositoryPath, false);
    SVNStatus.Sort(siChecked, sdAscending);
    UpdateFilesListView;
  end;
end;

procedure TSVNLocalStatusFrm.mnuShowDiffClick(Sender: TObject);

begin
  if Assigned(SVNFileListView.Selected) then
  begin
    debugln('TSVNStatusFrm.mnuShowDiffClick Path=' ,SVNFileListView.Selected.SubItems[0]);

    if pos(RepositoryPath,SVNFileListView.Selected.SubItems[0]) <> 0 then
      ShowSVNDiffFrm('-r BASE', SVNFileListView.Selected.SubItems[0])
    else
      ShowSVNDiffFrm('-r BASE', AppendPathDelim(RepositoryPath) + SVNFileListView.Selected.SubItems[0]);
  end;
end;

procedure TSVNLocalStatusFrm.OKButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSVNLocalStatusFrm.PopupMenu1Popup(Sender: TObject);
var
  P: TPoint;
  LI: TListItem;
begin
  // make sure the row under the mouse is selected
  P := SVNFileListView.ScreenToControl(Mouse.CursorPos);
  LI := SVNFileListView.GetItemAt(P.X, P.Y);
  if LI <> nil then begin
    SVNFileListView.Selected := LI;
    {$note: using hardcoded column index!}
    if LI.SubItems[2] = 'unversioned' then
      mnuRevert.Enabled := False
    else
      mnuRevert.Enabled := True;
    if (LI.SubItems[2] = 'unversioned') or (LI.SubItems[2] = 'deleted') then begin
      mnuShowDiff.Enabled := False;
      mnuRemove.Enabled := False;
      mnuAdd.Enabled := True;
    end else begin
      mnuShowDiff.Enabled := True;
      mnuRemove.Enabled := True;
      mnuAdd.Enabled := False;
    end;
  end;
end;

procedure TSVNLocalStatusFrm.SVNFileListViewColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  case Column.Index of
    0: SVNStatus.ReverseSort(siChecked);
    1: SVNStatus.ReverseSort(siPath);
    2: SVNStatus.ReverseSort(siExtension);
    3: SVNStatus.ReverseSort(siItemStatus);
    4: SVNStatus.ReverseSort(siPropStatus);
    5: SVNStatus.ReverseSort(siAuthor);
    6: SVNStatus.ReverseSort(siRevision);
    7: SVNStatus.ReverseSort(siCommitRevision);
    8: SVNStatus.ReverseSort(siDate);
  end;

  UpdateFilesListView;
end;

procedure TSVNLocalStatusFrm.UpdateFilesListView;
var
  i: integer;
  StatusItem : TSVNStatusItem;
  Path: string;
begin
  SVNFileListView.BeginUpdate;
  SVNFileListView.Clear;
  for i := 0 to SVNStatus.List.Count - 1 do
  begin
    with SVNFileListView.Items.Add do
    begin
      StatusItem := SVNStatus.List.Items[i];
      //checkboxes
      Caption := '';
      Checked := StatusItem.Checked;
      //path
      Path := StatusItem.Path;
      if pos(RepositoryPath, Path) = 1 then
        path := CreateRelativePath(path, RepositoryPath, false);
      SubItems.Add(Path);
      //extension
      SubItems.Add(StatusItem.Extension);
      //file status
      SubItems.Add(StatusItem.ItemStatus);
      //property status
      SubItems.Add(StatusItem.PropStatus);
      //check if file is versioned
      if (LowerCase(StatusItem.ItemStatus) <> 'unversioned') and
         (LowerCase(StatusItem.ItemStatus) <> 'added') then
      begin
        //revision
        SubItems.Add(IntToStr(StatusItem.Revision));
        //commit revision
        SubItems.Add(IntToStr(StatusItem.CommitRevision));
        //author
        SubItems.Add(StatusItem.Author);
        //date
        SubItems.Add(DateTimeToStr(StatusItem.Date));
      end;
    end;
  end;
  SVNFileListView.EndUpdate;
end;

procedure TSVNLocalStatusFrm.ChangeCursor(ACursor: TCursor);
begin
  Cursor := ACursor;
  SVNFileListView.Cursor := ACursor;
  Self.Cursor := ACursor;
  Application.ProcessMessages;
end;

procedure TSVNLocalStatusFrm.UpdateCheckedStatus;
var
  i : Integer;
begin
  for i := 0 to SVNFileListView.Items.Count - 1 do
    with SVNFileListView.Items[i] do
      SVNStatus.List[Index].Checked := Checked;
end;

procedure TSVNLocalStatusFrm.FormCreate(Sender: TObject);
begin
  mnuShowDiff.Caption := rsShowDiff;
  mnuOpen.Caption := rsOpenFileInEditor;
  mnuRevert.Caption := rsRevert;
  mnuAdd.Caption := rsAdd;
  mnuRemove.Caption := rsRemove;

  SetColumn(SVNFileListView, 0, 25, '', False);
  SetColumn(SVNFileListView, 1, 300, rsPath, False);
  SetColumn(SVNFileListView, 2, 75, rsExtension, True);
  SetColumn(SVNFileListView, 3, 100, rsFileStatus, True);
  SetColumn(SVNFileListView, 4, 125, rsPropertyStatus, True);
  SetColumn(SVNFileListView, 5, 75, rsRevision, True);
  SetColumn(SVNFileListView, 6, 75, rsCommitRevision, True);
  SetColumn(SVNFileListView, 7, 75, rsAuthor, True);
  SetColumn(SVNFileListView, 8, 75, rsDate, True);

  ImageList.AddResourceName(HInstance, 'menu_svn_diff');
  ImageList.AddResourceName(HInstance, 'menu_svn_revert');
end;

procedure TSVNLocalStatusFrm.FormDestroy(Sender: TObject);
begin
  SVNLocalStatusFrm := nil;
end;

procedure TSVNLocalStatusFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SVNStatus.Free;
  CloseAction := caFree;
end;

procedure TSVNLocalStatusFrm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

end.

