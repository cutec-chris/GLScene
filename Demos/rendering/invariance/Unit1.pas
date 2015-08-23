unit Unit1;

{$MODE Delphi}

interface

uses
  LCLType, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLTexture, JPeg,
  GLGeomObjects, LResources, GLViewer, GLMaterial;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera: TGLCamera;
    DCCamera: TGLDummyCube;
    PLGround: TGLPlane;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary: TGLMaterialLibrary;
    GLCube1: TGLCube;
    DCPositionInvariant: TGLDummyCube;
    GLCylinder1: TGLCylinder;
    GLArrowLine1: TGLArrowLine;
    DCOrientationInvariant: TGLDummyCube;
    procedure FormCreate(Sender: TObject);
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


procedure TForm1.FormCreate(Sender: TObject);
var
  i:Integer;
begin
   GLMaterialLibrary.TexturePaths:=ExtractFilePath(ParamStr(0)) + '..' + PathDelim + '..' + PathDelim + 'media';
   for i:=0 to GLMaterialLibrary.Materials.Count-1 do
      with GLMaterialLibrary.Materials[i] do begin
         Material.Texture.ImageClassName:=TGLPersistentImage.ClassName;
         Material.Texture.Image.LoadFromFile(ExtractFilePath(ParamStr(0)) + '..' + PathDelim + '..' + PathDelim + 'media' + PathDelim + Name + '.jpg');
      end;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then
      GLCamera.MoveAroundTarget(my-y, mx-x);
   if ssRight in Shift then
      GLCamera.MoveTargetInEyeSpace((y-my)*0.05, (mx-x)*0.05, 0);
   mx:=x; my:=y;
end;

initialization
  {$i Unit1.lrs}

end.
