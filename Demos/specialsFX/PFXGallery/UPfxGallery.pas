// Simple effects gallery.

unit UPfxGallery;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  GLVectorGeometry,GLLCLViewer, GLScene, GLHUDObjects, GLParticleFX,
  GLVectorFileObjects, GLVectorTypes,GLObjects, GLBitmapFont,
  GLUtils,GLCadencer,  GLTexture, GLNavigator,
  ExtCtrls, GLGeomObjects, GLKeyboard, GLSpaceText,
  GLBehaviours,GLPerlinPFX, StdCtrls, GLBlur, GLCrossPlatform, GLCoordinates,
  GLBaseClasses;

const
     cRunBoost = 10;
     cWalkStep = 20;
     cStrafeStep =20;

type
  TFrmMain = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    PFXRail: TGLPerlinPFXManager;
    PFXBurning: TGLPerlinPFXManager;
    PFXSmoke: TGLPerlinPFXManager;
    BitmapFont1: TGLBitmapFont;
    GLScene1: TGLScene;
    WorldRoot: TGLDummyCube;
    PfxRenderer: TGLParticleFXRenderer;
    GLCamera1: TGLCamera;
    PFXBlueArea: TGLPerlinPFXManager;
    Timer1: TTimer;
    PFXElectro: TGLPerlinPFXManager;
    GLNavigator1: TGLNavigator;
    GLUserInterface1: TGLUserInterface;
    ICE: TGLSpaceText;
    GLPlane1: TGLPlane;
    PFXRedArea: TGLPerlinPFXManager;
    MAGMA: TGLSpaceText;
    SMOKE: TGLSpaceText;
    RAIL: TGLSpaceText;
    FIRE: TGLSpaceText;
    ELECTRIC: TGLSpaceText;
    FOG: TGLSpaceText;
    PFXFog: TGLPerlinPFXManager;
    PFXWaterfall: TGLPerlinPFXManager;
    WATER: TGLSpaceText;
    Panel1: TPanel;
    chkMouseLook: TCheckBox;
    GLLightSource1: TGLLightSource;
    chkFloor: TCheckBox;
    GLBlur1: TGLBlur;
    chkBlur: TCheckBox;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure chkMouseLookClick(Sender: TObject);
    procedure chkFloorClick(Sender: TObject);
    procedure chkBlurClick(Sender: TObject);
  private
    procedure HandleKeys(const deltaTime: Double);
    { Private declarations }
  public
    { Public declarations }

  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}



procedure TFrmMain.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
     HandleKeys(deltaTime);
     GLUserInterface1.Mouselook;

     GLSceneViewer1.Invalidate;
     GLUserInterface1.MouseUpdate;

     GLSceneViewer1.Invalidate;
end;


procedure TFrmMain.HandleKeys(const deltaTime: Double);
var
   boost : Single;
begin
   if IsKeyDown(VK_ESCAPE) then begin
      chkMouseLook.Checked := false;
      chkMouseLookClick(self);
   end;

   if IsKeyDown(VK_SHIFT) then begin
      boost:=cRunBoost*deltaTime
   end else
   if IsKeyDown(VK_CONTROL) then begin
      boost:=cRunBoost*0.01*deltaTime
   end else
   begin
      boost:=deltaTime;
   end;

   if IsKeyDown('W') then begin
      GLCamera1.Move(cWalkStep*boost);
   end;
   if IsKeyDown('S') then begin
      GLCamera1.Move(-cWalkStep*boost);
   end;

   if IsKeyDown('A') then begin
          GLCamera1.Slide(-cStrafeStep*boost)
   end;
   if IsKeyDown('D') then begin
          GLCamera1.Slide(cStrafeStep*boost)
   end;
end;

procedure TFrmMain.Timer1Timer(Sender: TObject);
begin
     Caption := Inttostr(Round(GLSceneViewer1.FramesPerSecond))+' FPS';
     GLSceneViewer1.ResetPerformanceMonitor;
end;







procedure TFrmMain.chkMouseLookClick(Sender: TObject);
begin
     GLUserInterface1.MouseLookActive:= chkMouseLook.Checked;
end;

procedure TFrmMain.chkFloorClick(Sender: TObject);
begin
     GLPlane1.Visible := chkFloor.Checked;
end;

procedure TFrmMain.chkBlurClick(Sender: TObject);
begin
     GLBlur1.Visible := chkBlur.Checked;
end;

end.
