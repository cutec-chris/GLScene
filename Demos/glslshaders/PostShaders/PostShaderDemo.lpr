program PostShaderDemo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Forms, Interfaces, umainform in 'umainform.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPostShaderDemoForm, PostShaderDemoForm);
  Application.Run;
end.
