{: This demo shows how to use a GLFeedback object to
   extract mesh data from regular GLScene objects.<p>

   The GLFeedback object uses the GL_FEEDBACK render
   mode to gather vertex information passed through
   the OpenGL render pipeline. The resultant buffer
   can store information like point, line and polygon
   vertex position, texture coordinate and color.

   The BuildMeshFromBuffer function will parse the
   buffer for polygon data and apply it to the assigned
   lists. If the VertexIndices list is assigned it will
   perform a mesh recalculation to eliminate any
   duplicate vertex positions which will allow for
   smooth normal calculation. The texture coords
   returned will be per index value. They can either
   be used in a TFGIndexTexCoordList or resampled for
   use with other facegroup types.

}

unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLVectorFileObjects, GLScene, GLObjects,
  StdCtrls, GLFeedback, GLTeapot, GLPolyhedron, LResources, Buttons,
  GLViewer;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLCube1: TGLCube;
    GLFreeForm1: TGLFreeForm;
    Button1: TButton;
    GLFeedback1: TGLFeedback;
    GLDodecahedron1: TGLDodecahedron;
    MeshObject1: TGLDummyCube;
    MeshObject2: TGLDummyCube;
    GLSphere1: TGLSphere;
    procedure Button1Click(Sender: TObject);
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


procedure TForm1.Button1Click(Sender: TObject);
var
  mo : TMeshObject;
  fg : TFGIndexTexCoordList;
begin
  // Clear our freeform of any meshes
  GLFreeForm1.MeshObjects.Clear;

  // Set feedback to active, will feedback render child
  // objects into it's buffer
  GLFeedback1.Active:=True;

  // Process the first mesh object (GLCube and GLDodecahedron)

  // Make the objects visible that we want to buffer
  MeshObject1.Visible:=True;

  // Render the feedback object to buffer it's child object
  // that are visible
  GLSceneViewer1.Buffer.Render(GLFeedback1);

  // Hide the child objects we rendered
  MeshObject1.Visible:=False;

  // Create a new mesh object in our freeform
  mo:=TMeshObject.CreateOwned(GLFreeForm1.MeshObjects);
  mo.Mode:=momTriangles;

  // Process the feedback buffer for polygon data
  // and build a mesh (normals are recalculated
  // since feedback only yields position and
  // texcoords)
  GLFeedback1.BuildMeshFromBuffer(
    mo.Vertices, mo.Normals, mo.TexCoords, nil);

  // Process the second mesh object (GLSphere)
  // (comments from first mesh object apply here also)
  MeshObject2.Visible:=True;
  GLSceneViewer1.Buffer.Render(GLFeedback1);
  MeshObject2.Visible:=False;

  // Vertex indices are required for smooth normals
  mo:=TMeshObject.CreateOwned(GLFreeForm1.MeshObjects);
  mo.Mode:=momFaceGroups;
  fg:=TFGIndexTexCoordList.CreateOwned(mo.FaceGroups);
  fg.Mode:=fgmmTriangles;
  GLFeedback1.BuildMeshFromBuffer(
    mo.Vertices, mo.Normals, fg.TexCoords, fg.VertexIndices);

  // Deactivate the feedback object
  GLFeedback1.Active:=False;

  GLFreeForm1.StructureChanged;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx:=x;
  my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my-y, mx-x);
  mx:=x;
  my:=y;
end;

initialization
  {$i Unit1.lrs}

end.
