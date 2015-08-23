// Example using GLSkybox. Use the W,A,S,D keys (or Z,Q,S,D) to move
// and the mouse to look around.
// The scene contains two GLSkyBox objects: GLSkyBox1 and GLSkyBox2 :
// GLSkyBox1 renders the 6 faces of the skybox cube showing
// the landscape, plus one cloudy-textured plane over our head;
// GLSkyBox2 renders only an extra cloudy-textured plane over our head, to
// make a parallattic effect with the other cloudy plane.
// The parallattic effect is achieved setting the "CloudsPlaneSize" and ""CloudsPlaneOffset"
// differently in GLSkyBox1 and GLSkyBox2, and by shifting
// the clouds texture gradually in the cadencer event.
// CloudsPlaneSize is the size of the clouds plane, while
// CloudsPlaneOffset is the distance from the center of the skybox cube (range : 0 to 0.5)
// Note that the moons are children of a GLSkyBox object, and therefore
// they are always rendered relatively to the camera

unit Unit1;

{$MODE Delphi}

interface

uses
  LCLType, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, GLTexture,
  GLCadencer, GLNavigator, GLViewer, GLKeyboard, GLLensFlare, GLObjects,
  JPeg, GLSkyBox, LResources, GLScene, GLMaterial;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLLightSource1: TGLLightSource;
    Castle: TGLDummyCube;
    GLCube1: TGLCube;
    GLCube11: TGLCube;
    GLCube111: TGLCube;
    GLCube112: TGLCube;
    GLCube2: TGLCube;
    GLCube21: TGLCube;
    GLCube211: TGLCube;
    GLCube212: TGLCube;
    GLNavigator1: TGLNavigator;
    GLCadencer1: TGLCadencer;
    GLUserInterface1: TGLUserInterface;
    Timer1: TTimer;
    GLLensFlare1: TGLLensFlare;
    GLSceneViewer1: TGLSceneViewer;
    GLSkyBox1: TGLSkyBox;
    GLSkyBox2: TGLSkyBox;
    GLSphere1: TGLSphere;
    GLSphere2: TGLSphere;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure HandleKeys(d: double);
    function LoadTexture(Matname,Filename : string) : TGLLibMaterial;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation



function mediaPath : string;
begin
   Result := ExtractFilePath(Paramstr(0))+'..' + PathDelim + '..' + PathDelim + 'media' + PathDelim + '';
end;

function TForm1.LoadTexture(Matname,Filename : string) : TGLLibMaterial;
begin
   Result := GLMaterialLibrary1.AddTextureMaterial(Matname,mediaPath + Filename);
   Result.Material.Texture.Disabled := false;
   Result.Material.Texture.TextureMode := tmDecal;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     // --------------
     // Load graphics
     // --------------

     // Skybox textures
     LoadTexture('Left','icecraterlf.jpg');
     LoadTexture('Right','icecraterrt.jpg');
     LoadTexture('Top','icecraterup.jpg');
     LoadTexture('Bottom','icecraterdn.jpg');
     LoadTexture('Front','icecraterft.jpg');
     LoadTexture('Back','icecraterbk.jpg');
     with LoadTexture('Clouds','clouds.jpg') do
     begin
          // Add transparency to clouds
          Material.BlendingMode := bmTransparency;
          Material.FrontProperties.Diffuse.Alpha := 0.2;

          // scale the clouds texture
          TextureScale.X := 8;
          TextureScale.y := 8;
     end;

     // bricks
     with LoadTexture('Bricks','rawwall.jpg') do
     begin
          TextureScale.X := 1;
          TextureScale.y := 32;
          Material.Texture.TextureMode := tmModulate;
     end;
     with LoadTexture('Bricks2','marbletiles.jpg') do
     begin
          TextureScale.X := 6;
          TextureScale.y := 1;
          Material.Texture.TextureMode := tmModulate;
     end;


     //Moon
     LoadTexture('Moon','unwrapped moon.jpg').Material.Texture.TextureMode := tmModulate;





     //-----------------------------------------
     // Assign materials to objects
     //-----------------------------------------
     GLCube1.Material.LibMaterialName := 'Bricks';
     GLCube11.Material.LibMaterialName := 'Bricks';
     GLCube111.Material.LibMaterialName := 'Bricks';
     GLCube112.Material.LibMaterialName := 'Bricks';
     GLCube2.Material.LibMaterialName := 'Bricks2';
     GLCube21.Material.LibMaterialName := 'Bricks2';
     GLCube21.Material.LibMaterialName := 'Bricks2';
     GLCube211.Material.LibMaterialName := 'Bricks2';
     GLCube212.Material.LibMaterialName := 'Bricks2';
     GLSphere1.Material.LibMaterialName := 'Moon';
     GLSphere2.Material.LibMaterialName := 'Moon';

     GLUserInterface1.MouseLookActive:=true;


end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
     // Make clouds Texture slide
     with GLMaterialLibrary1.Materials.GetLibMaterialByName('Clouds') do
     begin
          TextureOffset.X := TextureOffset.X + deltaTime * 0.02;
          TextureOffset.Y := TextureOffset.Y + deltaTime * 0.03;
     end;


     // Rotate moons
     GLSphere1.Turn(deltaTime * 7);
     GLSphere2.Turn(deltaTime * 10);

     HandleKeys(deltaTime);
     GLUserInterface1.Mouselook;
     GLUserInterface1.MouseUpdate;
     GLSceneViewer1.Invalidate;
end;


procedure TForm1.HandleKeys(d: double);
begin
      if IsKeyDown('W') or IsKeyDown('Z') then
           GLCamera1.Move(d);
      if IsKeyDown('S') then
           GLCamera1.Move(-d);
      if IsKeyDown('A') or IsKeyDown('A') then
           GLCamera1.Slide(-d);
      if IsKeyDown('D') then
           GLCamera1.Slide(d);

      if IsKeyDown(VK_ESCAPE) then Close;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
     caption := inttostr(Round(GLSceneViewer1.FramesPerSecond));
     GLSceneViewer1.ResetPerformanceMonitor;
end;

initialization
  {$i Unit1.lrs}

end.
