program OctreeDemo;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  fOctreeDemo in 'fOctreeDemo.pas' {frmOctreeDemo};

{ $R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmOctreeDemo, frmOctreeDemo);
  Application.Run;
end.
