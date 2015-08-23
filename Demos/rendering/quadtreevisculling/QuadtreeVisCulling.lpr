program QuadtreeVisCulling;

{$MODE Delphi}

uses
  {$ifndef windows}
  cThreads,
  {$endif}
  Interfaces,
  Forms,
  fQuadtreeVisCulling in 'fQuadtreeVisCulling.pas' {frmQuadtreeVisCulling};


begin
  Application.Initialize;
  Application.CreateForm(TfrmQuadtreeVisCulling, frmQuadtreeVisCulling);
  Application.Run;
end.
