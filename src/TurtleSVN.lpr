program TurtleSVN;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, Main, svnstatusform, svndiffform, svnupdateform,
  svnaddprojectform, svnclasses, svnlogform, svncommitform, SVNCheckout,
  fdiffer, idiff, Utils, SettingsDialog, GeneralOptionsFrm, AdvancedOptionsFrm,
  SettingsManager, SVNUpdateToRevision;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSVNCheckoutForm, SVNCheckoutForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TSVNUpdateToRevisionForm, SVNUpdateToRevisionForm);
  Application.Run;
end.

