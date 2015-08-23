program oxBoxStack;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  oxMain in 'oxMain.pas' {oxBoxStackFrm};

begin
  Application.Initialize;
  Application.CreateForm(ToxBoxStackFrm, oxBoxStackFrm);
  Application.Run;
end.
