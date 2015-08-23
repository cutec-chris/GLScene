program FurBall;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  fFurBall in 'fFurBall.pas' {frmFurBall};

begin
  Application.Initialize;
  Application.CreateForm(TfrmFurBall, frmFurBall);
  Application.Run;
end.
