program multiproxy;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  GLMultiProxy in '..\..\Source\GLMultiProxy.pas';

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
