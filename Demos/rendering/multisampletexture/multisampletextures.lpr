program multisampletextures;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  umain,
  glscene_designtime, glscene_runtime;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGLDemoForm, GLDemoForm);
  Application.Run;
end.
