{: Beer demo.<p>

   The scene is defined in the DFM, the code only takes care of loading
   meshes, textures and reacting to mouse clicks.<br>
   A PerlinPFX is used for the foam, a simpler PolyPFX for the bubbles.
   The glass effect uses a texture in sphere mapping mode, the grouping
   of faces in the 3DS model does the rest.<p>

   Original idea by Vitomir Savic.
}
unit Unit1;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  GLVectorFileObjects, GLObjects, GLFile3ds, GLCadencer, jpeg,
  GLGeomObjects, VectorGeometry, GLShadowPlane, GLParticleFX, GLPerlinPFX,
  LResources, GLScene, GLLCLViewer, GLViewer;

type

  { TForm1 }

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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetCurrentDir('..' + PathDelim + '..' + PathDelim + 'media');

  GLFreeForm1.LoadFromFile('beer.3ds');

  GLFreeForm1.Material.Texture.Image.LoadFromFile('clouds.jpg');
  GLShadowPlane1.Material.Texture.Image.LoadFromFile('ashwood.jpg');
  GetOrCreateSourcePFX(GLDummyCube3).Burst(0, 150);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  GLCadencer1.Free;
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

initialization
  {$i Unit1.lrs}

end.
