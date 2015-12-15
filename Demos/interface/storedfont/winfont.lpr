program winfont;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  Unit2 in 'Unit2.pas' {Form2};

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
