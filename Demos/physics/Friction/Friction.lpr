program Friction;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  fFriction in 'fFriction.pas' {frmFriction};

begin
  Application.Initialize;
  Application.CreateForm(TfrmFriction, frmFriction);
  Application.Run;
end.
