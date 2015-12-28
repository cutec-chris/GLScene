program ExPolygon;

{$MODE Delphi}

uses
  Forms, Interfaces,
  expolygon1, glscene_designtime, glscene_runtime;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
