program DiffuseSpecularShaderDemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  umainform, glscene_designtime, glscene_runtime;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGLSLTestForm, GLSLTestForm);
  Application.Run;
end.
