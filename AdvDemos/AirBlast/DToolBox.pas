// DToolBox
{: Rendering utility toolbox.
}
unit DToolBox;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, GLVectorFileObjects, GLScene, Graphics,
  GLCoordinates, GLCrossPlatform, GLBaseClasses;

type
  TDMToolBox = class(TDataModule)
    GLScene: TGLScene;
    MemoryViewer: TGLMemoryViewer;
    GLCamera: TGLCamera;
    FreeForm: TGLFreeForm;
  private
    { Private declarations }
  public
    { Public declarations }
    function CreateTopDownTexture(const modelName : String; texSize : Integer;
                                  var scale : Single) : TBitmap;
  end;

var
  DMToolBox: TDMToolBox;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R *.lfm}

uses GLContext;

// ------------------
// ------------------ TDMToolBox ------------------
// ------------------

// CreateTopDownTexture
//
function TDMToolBox.CreateTopDownTexture(const modelName : String; texSize : Integer;
                                         var scale : Single) : TBitmap;
var
   radius : Single;
begin
   FreeForm.LoadFromFile(modelName);
   radius:=FreeForm.BoundingSphereRadius;
   scale:=1/radius;
   GLCamera.Position.Z:=radius*2;
   GLCamera.DepthOfView:=radius*4;
   GLCamera.SceneScale:=scale;
   MemoryViewer.Buffer.BackgroundAlpha:=0;
   MemoryViewer.Width:=texSize;
   MemoryViewer.Height:=texSize;
   MemoryViewer.Render;
   Result:=MemoryViewer.Buffer.CreateSnapShotBitmap;
end;

end.
