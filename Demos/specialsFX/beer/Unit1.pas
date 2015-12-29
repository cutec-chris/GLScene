{: Beer demo.<p>

   The scene is defined in the DFM, the code only takes care of loading
   meshes, textures and reacting to mouse clicks.<br>
   A PerlinPFX is used for the foam, a simpler PolyPFX for the bubbles.
   The glass effect uses a texture in sphere mapping mode, the grouping
   of faces in the 3DS model does the rest.<p>

   Original idea by Vitomir Savic.
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  GLScene, GLVectorFileObjects, GLObjects, GLLCLViewer,
  GLFile3DS, GLCadencer, GLGeomObjects, GLVectorGeometry,
  GLShadowPlane, GLParticleFX, GLPerlinPFX, GLCrossPlatform, GLCoordinates,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLFreeForm1: TGLFreeForm;
    GLCadencer1: TGLCadencer;
    GLCylinder1: TGLCylinder;
    GLCylinder2: TGLCylinder;
    GLShadowPlane1: TGLShadowPlane;
    GLPerlinPFXManager1: TGLPerlinPFXManager;
    GLDummyCube3: TGLDummyCube;
    GLParticleFXRenderer1: TGLParticleFXRenderer;
    GLPolygonPFXManager1: TGLPolygonPFXManager;
    GLParticleFXRenderer2: TGLParticleFXRenderer;
    procedure FormActivate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses GLUtils;

procedure TForm1.FormActivate(Sender: TObject);
begin
   SetGLSceneMediaDir();

   GLFreeForm1.LoadFromFile('beer.3ds');

   GLFreeForm1.Material.Texture.Image.LoadFromFile('clouds.jpg');
   GLShadowPlane1.Material.Texture.Image.LoadFromFile('ashwood.jpg');
   GetOrCreateSourcePFX(GLDummyCube3).Burst(0, 150);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLCamera1.MoveAroundTarget(0, 10*deltatime);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then
      GLCamera1.MoveAroundTarget(my-y, mx-x);
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1DblClick(Sender: TObject);
begin
   GLCadencer1.Enabled:=not GLCadencer1.Enabled;
end;

end.
