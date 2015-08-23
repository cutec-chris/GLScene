program beer;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  unit1 in 'Unit1.pas' {Form1};

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
