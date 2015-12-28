program csgdemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  mainformunit, glscene_designtime, glscene_runtime {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
