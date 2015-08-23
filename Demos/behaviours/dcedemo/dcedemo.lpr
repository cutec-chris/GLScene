program dceDemo;

{$MODE Delphi}

uses
  {$ifndef windows}
  cThreads,
  {$endif}
  Interfaces,
  Forms,
  Unit1;

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
