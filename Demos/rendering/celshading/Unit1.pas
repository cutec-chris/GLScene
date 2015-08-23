unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, LCLType,
  Dialogs, GLCadencer, GLViewer, GLVectorFileObjects,
  GLCelShader, GLGeomObjects, GLTexture, GLObjects,
  LResources, GLScene, ExtCtrls, GLMaterial;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLActor1: TGLActor;
    GLTexturedCelShader: TGLCelShader;
    GLColoredCelShader: TGLCelShader;
    GLTorus1: TGLTorus;
    Timer1: TTimer;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my, lx, ly : Integer;
  end;

var
  Form1: TForm1;

implementation


uses
  GLFileMD2, JPEG, GLKeyboard;

procedure TForm1.FormCreate(Sender: TObject);
var
  r : Single;
begin
  SetCurrentDir(ExtractFilePath(Application.ExeName)+'..' + PathDelim + '..' + PathDelim + 'media');

  GLActor1.LoadFromFile('waste.md2');
  r:=GLActor1.BoundingSphereRadius;
  GLActor1.Scale.SetVector(2.5/r,2.5/r,2.5/r);
  GLActor1.AnimationMode:=aamLoop;
  GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile('wastecell.jpg');
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx:=x;
  my:=y;
  lx:=x;
  ly:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  mx:=x;
  my:=y;
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  Form1.Caption:=Format('Cel Shading Demo - %.2f FPS',[GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if IsKeyDown(VK_LBUTTON) then begin
    GLCamera1.MoveAroundTarget(ly-my,lx-mx);
    lx:=mx;
    ly:=my;
  end;

  GLTorus1.TurnAngle:=15*Sin(newTime*5);
  GLTorus1.PitchAngle:=15*Cos(newTime*5);
end;

initialization
  {$i Unit1.lrs}

end.
