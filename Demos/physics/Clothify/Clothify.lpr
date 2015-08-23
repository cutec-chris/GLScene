program Clothify;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  fClothify in 'fClothify.pas' {frmClothify};

begin
  Application.Initialize;
  Application.CreateForm(TfrmClothify, frmClothify);
  Application.Run;
end.
