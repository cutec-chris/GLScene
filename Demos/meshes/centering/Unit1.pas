{: Illustrates the effects of AutoCentering for FreeForms.<p>

   The same mesh is loaded three times and centered with different options
   (by default, the polyhedron is not centered in its mesh).
}

unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLVectorFileObjects, GLObjects, ComCtrls, StdCtrls,
  GLViewer, GLFile3DS, LResources, GLCadencer;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLCadencer1: TGLCadencer;
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    DummyCube2: TGLDummyCube;
    DummyCube3: TGLDummyCube;
    FreeForm1: TGLFreeForm;
    FreeForm2: TGLFreeForm;
    FreeForm3: TGLFreeForm;
    GLLightSource1: TGLLightSource;
    DCCamera: TGLDummyCube;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation


procedure TForm1.FormCreate(Sender: TObject);
const
   cFileName = '..' + PathDelim + '..' + PathDelim + 'media' + PathDelim + 'polyhedron.3ds';
begin
   // left one
   FreeForm3.AutoCentering:=[macCenterX, macCenterZ];
   FreeForm3.LoadFromFile(cFileName);
   // central one
   FreeForm2.AutoCentering:=[macCenterY];
   FreeForm2.LoadFromFile(cFileName);
   // right one
   FreeForm1.AutoCentering:=[macCenterX, macCenterY, macCenterZ];
   FreeForm1.LoadFromFile(cFileName); 
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
   DCCamera.PitchAngle:=TrackBar1.Position;
end;

initialization
  {$i Unit1.lrs}

end.
