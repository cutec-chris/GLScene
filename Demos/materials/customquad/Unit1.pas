{: Using materials in a TGLDirectOpenGL OnRender.<p>

   This demo shows how to dynamically create materials in a material library
   and use them in a TGLDirectOpenGL to render your own stuff.<br>
   The render is quite simple: two quads, each with its own texture. The
   TGLDirectOpenGL is placed in a small hierarchy with a torus and dummy cube,
   and the rotation animation are handled by those two object to show that
   the OnRender code uses the hierarchy.
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, GLColor,
  GLCadencer, GLScene, GLObjects, GLTexture, GLBehaviours,
  GLViewer, GLGeomObjects, LResources, GLMaterial, GLRenderContextInfo;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    Torus1: TGLTorus;
    DirectOpenGL1: TGLDirectOpenGL;
    GLLightSource1: TGLLightSource;
    GLCadencer1: TGLCadencer;
    procedure DirectOpenGL1Render(Sender : TObject; var rci: TRenderContextInfo);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation


uses OpenGL1x, JPeg;

procedure TForm1.FormCreate(Sender: TObject);
begin
   // dynamically create 2 materials and load 2 textures
   with GLMaterialLibrary do begin
      with AddTextureMaterial('wood', '..' + PathDelim + '..' + PathDelim + 'media' + PathDelim + 'ashwood.jpg') do
         Material.FrontProperties.Emission.Color:=clrGray50;
      with AddTextureMaterial('stone', '..' + PathDelim + '..' + PathDelim + 'media' + PathDelim + 'walkway.jpg') do
         Material.FrontProperties.Emission.Color:=clrGray50;
   end;
end;

procedure TForm1.DirectOpenGL1Render(Sender : TObject; var rci: TRenderContextInfo);
var
   material : TGLLibMaterial;
begin
   // disable face culling
   glDisable(GL_CULL_FACE);
   // 1st quad, textured with 'wood', using standard method
   GLMaterialLibrary.ApplyMaterial('wood', rci);
   glBegin(GL_QUADS);
      glTexCoord2f(0, 1);  glVertex3f(0.5, 0.5, -0.5);
      glTexCoord2f(0, 0);  glVertex3f(-0.5, 0.5, -0.5);
      glTexCoord2f(1, 0);  glVertex3f(-0.5, 0, 0.5);
      glTexCoord2f(1, 1);  glVertex3f(0.5, 0, 0.5);
   glEnd;
   GLMaterialLibrary.UnApplyMaterial(rci);
   // 2nd quad, textured with 'stone'
   // we "manually" apply the material, this can be usefull if you want to have
   // some dynamic material control
   material:=GLMaterialLibrary.Materials.GetLibMaterialByName('stone');
   material.Material.Apply(rci);
   glBegin(GL_QUADS);
      glTexCoord2f(0, 1);  glVertex3f(0.5, -0.5, -0.5);
      glTexCoord2f(0, 0);  glVertex3f(0.5, 0, 0.5);
      glTexCoord2f(1, 0);  glVertex3f(-0.5, 0, 0.5);
      glTexCoord2f(1, 1);  glVertex3f(-0.5, -0.5, -0.5); 
   glEnd;
   material.Material.UnApply(rci);
   // enable face culling again
   glEnable(GL_CULL_FACE);
end;

initialization
  {$i Unit1.lrs}

end.
