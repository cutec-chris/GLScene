program CameraControllerDemo;

{$MODE Delphi}

uses
  Forms, LResources, Interfaces,
  Main in 'Main.pas', glscenelazarus {Form1};

{ $R *.res}

{$IFDEF WINDOWS}{$R CameraControllerDemo.rc}{$ENDIF}

begin
  { $I CameraControllerDemo.lrs}
  Application.Initialize;
  Application.Title := 'CameraControllerDemo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
