{: Simple TGLShader based multipass demo.<p>

   This demo uses a custom TGLShader subclass to implement the classic
   multipass hidden lines rendering technique on a torus: first pass renders
   model with filled triangles, second pass does the wireframe.<p>

   You'll also note the glPolygonOffset call, it displaces fragments depths
   value a little "farther away" so that surface fill depth values do not
   interact with the rendering of the lines (comment out the call and you'll
   see).<br>
   The axis and sphere allow you to see the limit of that simple technique:
   it actually "paints" between the lines, so you cannot use it to make
   transparent wireframed objects with hidden lines - if that thought ever
   blossomed in your mind ;)<p>

   Additionnal objects around the show a glow/toon edges effect achieved in two
   passes too: the 1st pass activate lines and gives them a width, the second
   is used to fill the surface (and clear the lines that aren't on edges).
   (TOutLineShader thanks to Delauney Jerome, jdelauney@free.fr)
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, GLColor, GLRenderContextInfo,
  Dialogs, GLScene, GLObjects, GLTexture, OpenGL1x, GLMaterial,
  StdCtrls, GLGeomObjects, GLState, LResources,Buttons, GLViewer;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Torus1: TGLTorus;
    BUBind: TButton;
    Sphere1: TGLSphere;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLAnnulus1: TGLAnnulus;
    GLAnnulus2: TGLAnnulus;
    GLCube1: TGLCube;
    GLSphere1: TGLSphere;
    procedure BUBindClick(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation


type

   THiddenLineShader = class (TGLShader)
      private
         BackgroundColor, LineColor : TColorVector;
         PassCount : Integer;
      public
         procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
         function DoUnApply(var rci : TRenderContextInfo) : Boolean; override;
   end;

   TOutLineShader = class (TGLShader)
      private
         BackgroundColor, LineColor : TColorVector;
         OutlineSmooth, Lighting : Boolean;
         OutlineWidth, Oldlinewidth : Single;
         PassCount : Integer;
      public
         procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
         function DoUnApply(var rci : TRenderContextInfo) : Boolean; override;
   end;

procedure THiddenLineShader.DoApply(var rci : TRenderContextInfo; Sender : TObject);
begin
   // new object getting rendered, 1st pass
   PassCount:=1;

   // backup state
   glPushAttrib(GL_ENABLE_BIT);
   // disable lighting, this is a solid fill
   glDisable(GL_LIGHTING);
   rci.GLStates.SetGLPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
   // use background color
   glColor3fv(@BackgroundColor);
   // enable and adjust polygon offset
   glEnable(GL_POLYGON_OFFSET_FILL);
   glPolygonOffset(1, 2);
end;

function THiddenLineShader.DoUnApply(var rci : TRenderContextInfo) : Boolean;
begin
   case PassCount of
      1 : begin
         // 1st pass completed, we setup for the second
         PassCount:=2;

         // switch to wireframe and its color
         rci.GLStates.SetGLPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
         glColor3fv(@LineColor);
         // disable polygon offset
         glDisable(GL_POLYGON_OFFSET_LINE);

         Result:=True;
      end;
      2 : begin
         // restore state
         glPopAttrib;

         // we're done
         Result:=False;
      end;
   else
      // doesn't hurt to be cautious
      Assert(False);
      Result:=False;
   end;
end;

procedure TOutLineShader.DoApply(var rci : TRenderContextInfo; Sender : TObject);
begin
   PassCount:=1;
   glPushAttrib(GL_ENABLE_BIT);
   glDisable(GL_LIGHTING);

   if outlineSmooth then begin
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
      glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
      glEnable(GL_LINE_SMOOTH);
   end else glDisable(GL_LINE_SMOOTH);

   glGetFloatv(GL_LINE_WIDTH,@oldlinewidth);
   glLineWidth(OutLineWidth);
   glPolygonMode(GL_BACK, GL_LINE);
   glCullFace(GL_FRONT);
   glDepthFunc(GL_LEQUAL);
   glColor3fv(@lineColor);
end;

function TOutLineShader.DoUnApply(var rci : TRenderContextInfo) : Boolean;
begin
   case PassCount of
      1 : begin
         PassCount:=2;
         if lighting then
           glEnable(GL_LIGHTING)
         else glColor3fv(@backGroundColor);
         glDepthFunc(GL_LESS);
         glCullFace(GL_BACK);
         glPolygonMode(GL_BACK, GL_FILL);

         Result:=True;
      end;
      2 : begin
         glPopAttrib;
         glLineWidth(oldLineWidth);
         Result:=False;
      end;
   else
      Assert(False);
      Result:=False;
   end;
end;

procedure TForm1.BUBindClick(Sender: TObject);
var
   shader1 : THiddenLineShader;
   shader2 ,shader3: TOutLineShader;

begin
   BUBind.Enabled:=False;

   // instantiate our shaders
   
   shader1:=THiddenLineShader.Create(Self);
   shader1.BackgroundColor:=ConvertWinColor(GLSceneViewer1.Buffer.BackgroundColor);
   shader1.LineColor:=clrBlue;

   shader2:=TOutLineShader.Create(Self);
   with shader2 do begin
      BackgroundColor:=ConvertWinColor(GLSceneViewer1.Buffer.BackgroundColor);
      Outlinesmooth:=true;
      OutLineWidth:=2;
      Lighting:=false;
      LineColor:=clrBlack;
   end;

   shader3:=TOutLineShader.Create(Self);
   with shader3 do begin
      BackgroundColor:=ConvertWinColor(GLSceneViewer1.Buffer.BackgroundColor);
      Outlinesmooth:=false;
      OutLineWidth:=4;
      Lighting:=true;
      LineColor:=clrRed;
   end;

   // binds the shaders to the materials
   GLMaterialLibrary1.Materials[0].Shader:=shader1;
   GLMaterialLibrary1.Materials[1].Shader:=shader2;
   GLMaterialLibrary1.Materials[2].Shader:=shader3;

end;

//
// Classic mouse movement bits
//

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift=[ssLeft] then
      GLCamera1.MoveAroundTarget(my-y, mx-x)
   else if Shift=[ssRight] then
      GLCamera1.RotateTarget(my-y, mx-x);
   mx:=x; my:=y;
end;

initialization
  {$i Unit1.lrs}

end.
