program Actor;

{$MODE Delphi}

uses
  Forms, Interfaces,
  demo in 'demo.pas',
  glscene_designtime, glscene_runtime ;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
