program Shadows;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  Main in 'Main.pas' {MainFm};

begin
  Application.Initialize;
  Application.CreateForm(TMainFm, MainFm);
  Application.Run;
end.
