{: Early mesh subdivision refinement demo.<p>

   Yes, it's slow, the edge data construction is not optimized, and the MD2
   format isn't really suited for refinement (that's approx 200 frames we have
   to subdivide and keep in memory... but it's good for benchmarking!).<p>
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GLVectorFileObjects, GLObjects, GLTexture,
  VectorLists, ComCtrls, ExtCtrls, GLMaterial,
  LResources, GLScene, GLCadencer, Buttons, GLViewer;

type

  { TForm1 }

  TForm1 = class(TForm)
    BULoad1: TButton;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    BUSubdivide: TButton;
    TrackBar1: TTrackBar;
    RBWireFrame: TRadioButton;
    RBSolid: TRadioButton;
    CBAnimate: TCheckBox;
    GLActor1: TGLActor;
    Label1: TLabel;
    LASubdivideTime: TLabel;
    GLLightSource1: TGLLightSource;
    procedure BULoadClick(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure BUSubdivideClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure RBWireFrameClick(Sender: TObject);
    procedure RBSolidClick(Sender: TObject);
    procedure CBAnimateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation


uses MeshUtils, VectorGeometry, Jpeg, GLFileObj, GLCrossPlatform,
     GLFile3DS, GLFileMD2;

procedure TForm1.BULoadClick(Sender: TObject);
begin
   BUSubdivide.Enabled:=True;

   GLMaterialLibrary1.TexturePaths:='..' + PathDelim + '..' + PathDelim + 'media';

//   GLFreeForm1.LoadFromFile('e:\sf\glscene\demos\media\polyhedron.3ds');
//   GLFreeForm1.LoadFromFile('e:\sf\glscene\demos\media\mushroom.3ds');
//   GLFreeForm1.LoadFromFile('e:\sf\glscene\demos\media\trinityrage.smd');

{   GLActor1.LoadFromFile('e:\sf\glscene\demos\media\trinityrage.smd');
   GLActor1.AddDataFromFile('e:\sf\glscene\demos\media\run.smd');
   GLActor1.Animations[1].MakeSkeletalTranslationStatic;
   GLActor1.SwitchToAnimation(GLActor1.Animations[1]);}

   GLActor1.LoadFromFile('..' + PathDelim + '..' + PathDelim + 'media' + PathDelim + 'waste.md2');
   GLActor1.Material.Texture.Image.LoadFromFile('..' + PathDelim + '..' + PathDelim + 'media' + PathDelim + 'waste.jpg');
   GLActor1.Material.Texture.Enabled:=True;
   GLActor1.SwitchToAnimation(GLActor1.Animations[0]); 

//   GLFreeForm1.LoadFromFile('e:\sf\glscene\demos\media\HighPolyObject.3ds');

   CBAnimateClick(Self);
end;

procedure TForm1.BUSubdivideClick(Sender: TObject);
var
   i, j : Integer;
   tris, norms, tex, buf, morphTris, morphNorms : TAffineVectorList;
   indices, texIndices : TIntegerlist;
   firstRemap, subdivideRemap, bufRemap : TIntegerList;
   t : Int64;
begin
   BUSubdivide.Enabled:=False;

   Screen.Cursor:=crHourGlass;
   //t:=StartPrecisionTimer;

   for i:=0 to GLActor1.MeshObjects.Count-1 do begin
      tex:=TAffineVectorList.Create;
      with GLActor1.MeshObjects[i] do begin
         tris:=ExtractTriangles(tex);
      end;
      indices:=BuildVectorCountOptimizedIndices(tris);
      firstRemap:=TIntegerList(indices.CreateClone);
      RemapAndCleanupReferences(tris, indices);

      norms:=BuildNormals(tris, indices);

      // subdivide geometry
      SubdivideTriangles(TrackBar1.Position*0.1, tris, indices, norms);

      texIndices:=BuildVectorCountOptimizedIndices(tex);
      RemapAndCleanupReferences(tex, texIndices);

      // subdivide texture space
      SubdivideTriangles(0, tex, texIndices);

      // Re-expand everything
      buf:=TAffineVectorList.Create;
      try
         ConvertIndexedListToList(tris, indices, buf);
         tris.Assign(buf);
         buf.Count:=0;
         ConvertIndexedListToList(norms, indices, buf);
         norms.Assign(buf);
         buf.Count:=0;
         ConvertIndexedListToList(tex, texIndices, buf);
         tex.Assign(buf);
      finally
         buf.Free;
      end;

      // Pack & Optimize the expanded stuff
      indices.Free;
      indices:=BuildVectorCountOptimizedIndices(tris, norms, tex);
      subdivideRemap:=TIntegerList(indices.CreateClone);
      RemapReferences(norms, indices);
      RemapReferences(tex, indices);
      RemapAndCleanupReferences(tris, indices);

      IncreaseCoherency(indices, 13);

      with GLActor1.MeshObjects[i] as TMorphableMeshObject do begin

         bufRemap:=TIntegerList.Create;
         for j:=0 to MorphTargets.Count-1 do begin
            MorphTo(j);

            morphTris:=ExtractTriangles;
            bufRemap.Assign(firstRemap);
            RemapAndCleanupReferences(morphTris, bufRemap);

            morphNorms:=MeshUtils.BuildNormals(morphTris, bufRemap);

            SubdivideTriangles(TrackBar1.Position*0.1, morphTris, bufRemap, morphNorms);

            buf:=TAffineVectorList.Create;
            try
               ConvertIndexedListToList(morphTris, bufRemap, buf);
               morphTris.Assign(buf);
               ConvertIndexedListToList(morphNorms, bufRemap, buf);
               morphNorms.Assign(buf);
            finally
               buf.Free;
            end;
            RemapReferences(morphTris, subdivideRemap);
            RemapReferences(morphNorms, subdivideRemap);

            MorphTargets[j].Vertices:=morphTris;
            MorphTargets[j].Normals:=morphNorms;

            morphTris.Free;
            morphNorms.Free;
         end;
         bufRemap.Free;

         Vertices:=tris;
         Normals:=norms;
         TexCoords:=tex;
         FaceGroups.Clear;
         with TFGVertexIndexList.CreateOwned(FaceGroups) do begin
            VertexIndices:=indices;
            Mode:=fgmmTriangles;
         end;
      end;

      texIndices.Free;
      subdivideRemap.Free;
      firstRemap.Free;
      tex.Free;
      indices.Free;
      norms.Free;
      tris.Free;
   end;

//   (GLActor1.MeshObjects[0] as TSkeletonMeshObject).PrepareBoneMatrixInvertedMeshes;

   LASubdivideTime.Caption:=Format('%.1f ms', [StopPrecisionTimer(t)*1000]);
   // Initial perf: 1412 ms
   // Basic Edges Hash: 464 ms
   // Several transfer optims: 377 ms
   // morph & subdivide normals too : 527 ms
   Screen.Cursor:=crDefault;

   GLActor1.StructureChanged;
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
   if Shift=[ssLeft] then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
   end else if Shift=[ssRight] then begin
      GLCamera1.MoveAroundTarget(mx-x, my-y);
   //Bug With Lazarus k00m
   //end else if Shift=[ssRight] then begin
   //   GLCamera1.RotateTarget(my-y, mx-x);
   end;
   mx:=x;
   my:=y;
end;

procedure TForm1.RBWireFrameClick(Sender: TObject);
begin
   GLActor1.Material.FrontProperties.PolygonMode:=pmLines;
   GLActor1.Material.BackProperties.PolygonMode:=pmLines;
end;

procedure TForm1.RBSolidClick(Sender: TObject);
begin
   GLActor1.Material.FrontProperties.PolygonMode:=pmFill;
   GLActor1.Material.BackProperties.PolygonMode:=pmFill;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.1f FPS -  %d Triangles',
                   [GLSceneViewer1.FramesPerSecond,
                    GLActor1.MeshObjects.TriangleCount]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   GLSceneViewer1.Invalidate;
end;

procedure TForm1.CBAnimateClick(Sender: TObject);
begin
   // not only turns on/off animation, but also forces the TGLActor
   // to generate a display list when animation is off
   if not CBAnimate.Checked then begin
      GLActor1.AnimationMode:=aamLoop;
      GLActor1.ObjectStyle:=GLActor1.ObjectStyle+[osDirectDraw];
      GLActor1.Reference:=aarMorph;
   end else begin
      GLActor1.AnimationMode:=aamNone;
      GLActor1.MeshObjects.MorphTo(0);
      GLActor1.Reference:=aarNone;
      GLActor1.StructureChanged;
      GLActor1.ObjectStyle:=GLActor1.ObjectStyle-[osDirectDraw];
   end;
end;

initialization
  {$i Unit1.lrs}

end.
