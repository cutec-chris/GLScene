program actorproxy;

{$MODE Delphi}

uses
  Forms, Interfaces,
  unit1, glscene_runtime, glscene_designtime;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
