{: TGLTilePlane demo.<p>

   Illustrates the use of TGLTilePlane to render an area made of tiled
   textures placed in a grid. The components links to a materiallibrary
   (containing tile materials, referred by index) and renders the area
   with quads sorted by material.<br>
   The size of the area for TGLTilePlane is infinite (i.e. limited by
   available memory) and adjusts itself dynamically.<p>

   The tile overlap can be adjusted by the texture coordinates scaling
   of the material, for instance, the "marbletiles" texture covers 4 tiles
   and the "walkway" texture covers 2 tiles in this demo.<p>

   Note that if you don't have a "pro" OpenGL card, the grid with its smoothed
   lines may cost you a lot of FPS, so you may want to turn it off for
   performance assessments.
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, LCLType, LCLIntf,
  Dialogs, ExtCtrls, GLObjects, GLGraph, GLScene, GLViewer,
  VectorGeometry, GLTilePlane, GLTexture, GLCadencer, Jpeg, StdCtrls,
  OpenGL1x, LResources, GLMaterial, GLRenderContextInfo;

type
  TForm1 = class(TForm)
    GLScene: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera: TGLCamera;
    DCTarget: TGLDummyCube;
    GLXYZGrid: TGLXYZGrid;
    Panel1: TPanel;
    GLMaterialLibrary: TGLMaterialLibrary;
    GLLightSource: TGLLightSource;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    Label1: TLabel;
    CBMaterial: TComboBox;
    GLTilePlane: TGLTilePlane;
    GLDirectOpenGL: TGLDirectOpenGL;
    DCSelection: TGLDummyCube;
    GLLines1: TGLLines;
    BUPack: TButton;
    Label2: TLabel;
    CBShowGrid: TCheckBox;
    CBSortByMaterials: TCheckBox;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure GLDirectOpenGLRender(Sender : TObject; var rci: TRenderContextInfo);
    procedure BUPackClick(Sender: TObject);
    procedure CBShowGridClick(Sender: TObject);
    procedure CBSortByMaterialsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
    tileX, tileY : Integer;
    mip, translateOffset : TVector;
    translating : Boolean;
  end;

var
  Form1: TForm1;

implementation


uses GLKeyboard;

procedure TForm1.FormCreate(Sender: TObject);
var
   i, j : Integer;
begin
   // adjust the path
   GLMaterialLibrary.TexturePaths:=ExtractFilePath(Application.ExeName)+'..\..\media';

   // fill the tiled area with random tiles
   RandSeed:=0;
   for i:=-20 to 20 do for j:=-20 to 20 do
      GLTilePlane.Tiles[i, j]:=Random(GLMaterialLibrary.Materials.Count-1)+1;

   // set all tile materials to anisotropic,
   // add them to the material selection combo      
   for i:=1 to GLMaterialLibrary.Materials.Count-1 do
      with GLMaterialLibrary.Materials[i] do begin
         Material.Texture.FilteringQuality:=tfAnisotropic;
         CBMaterial.Items.Add(Name);
      end;
   CBMaterial.ItemIndex:=0;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x;
   my:=y;
   if Shift=[ssLeft] then begin
      GLTilePlane.Tiles[tileX, tileY]:=CBMaterial.ItemIndex+1;
      GLTilePlane.StructureChanged;
   end else if Shift=[ssRight] then begin
      GLTilePlane.Tiles[tileX, tileY]:=0;
      GLTilePlane.StructureChanged;
   end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
   GLCamera.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   ip : TVector;
   mp : TPoint;
   shiftDown : Boolean;
begin
   shiftDown:=(IsKeyDown(VK_LSHIFT) or IsKeyDown(VK_RSHIFT));
   DCSelection.Visible:=not shiftDown;
   if DCSelection.Visible then
      GLSceneViewer1.Cursor:=crDefault
   else GLSceneViewer1.Cursor:=crHandPoint;

   mp := Mouse.CursorPos;
   mp:=GLSceneViewer1.ScreenToClient(mp);
   if PtInRect(GLSceneViewer1.ClientRect, mp) then begin
      GLSceneViewer1.Buffer.ScreenVectorIntersectWithPlaneXY(
              VectorMake(mp.x, GLSceneViewer1.Height-mp.y, 0), 0, ip);
      tileX:=Round(ip[0]-0.5);
      tileY:=Round(ip[1]-0.5);
      DCSelection.Position.SetPoint(tileX, tileY, 0);

      if shiftDown then begin
         if IsKeyDown(VK_LBUTTON) then begin
            if not translating then begin
               translateOffset:=ip;
               translating:=True;
            end;
            DCTarget.Position.Translate(VectorAdd(VectorSubtract(mip, ip), translateOffset))
         end else translating:=False;
         if IsKeyDown(VK_RBUTTON) then begin
            GLCamera.MoveAroundTarget((my-mp.y)*0.5, (mx-mp.x)*0.5);
         end;
      end else begin
         translating:=False;
         if IsKeyDown(VK_LBUTTON) then begin
            GLTilePlane.Tiles[tileX, tileY]:=CBMaterial.ItemIndex+1;
            GLTilePlane.StructureChanged;
         end;
         if IsKeyDown(VK_RBUTTON) then begin
            GLTilePlane.Tiles[tileX, tileY]:=0;
            GLTilePlane.StructureChanged;
         end;
      end;
      mx:=mp.x;
      my:=mp.y;
   end;

   GLSceneViewer1.Invalidate;
end;

procedure TForm1.GLDirectOpenGLRender(Sender : TObject; var rci: TRenderContextInfo);
begin
   // we clear the depth buffer, so that the grid is always in front of the
   // tile plane and won't Z-Fight with it
   glClear(GL_DEPTH_BUFFER_BIT);
end;

procedure TForm1.BUPackClick(Sender: TObject);
begin
   // packing a tile area removes unused area from the in-memory structures
   GLTilePlane.Tiles.Pack;
end;

procedure TForm1.CBShowGridClick(Sender: TObject);
begin
   GLXYZGrid.Visible:=not CBShowGrid.Checked;
end;

procedure TForm1.CBSortByMaterialsClick(Sender: TObject);
begin
   GLTilePlane.SortByMaterials:= not CBSortByMaterials.Checked;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=GLSceneViewer1.FramesPerSecondText;
   GLSceneViewer1.ResetPerformanceMonitor;
end;

initialization
  {$i Unit1.lrs}

end.
