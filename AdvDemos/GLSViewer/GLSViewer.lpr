program GLSViewer;

{$MODE Delphi}

{: GLScene Viewer based on GLSViewer by Eric Grange
   http://www.sourceforge.net/projects/glscene
}

uses
  Forms, Interfaces,
  FMain;

{.$R *.res}

begin
  Application.Initialize;
  Application.Title := 'GLSViewer';
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
