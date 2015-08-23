program shadowvolumes;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  GLSilhouette in '..\..\Source\GLSilhouette.pas';

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
