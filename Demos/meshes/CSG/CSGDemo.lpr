program CSGDemo;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  MainFormUnit in 'MainFormUnit.pas' {Form1};


begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
