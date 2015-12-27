program TurtleSVN;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, svnstatusform, svndiffform, svnupdateform, svnaddprojectform,
  svnclasses, svnlogform, svncommitform, SVNCheckout, fdiffer, idiff, Utils;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSVNCheckoutForm, SVNCheckoutForm);
  Application.Run;
end.

