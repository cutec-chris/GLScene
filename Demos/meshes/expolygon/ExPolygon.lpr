program ExPolygon;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  ExPolygon1 in 'ExPolygon1.pas' {Form1};



begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
