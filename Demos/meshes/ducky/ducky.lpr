program ducky;

{$MODE Delphi}

uses
  Forms, Interfaces,
  unit1, glscene_designtime, glscene_runtime;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
