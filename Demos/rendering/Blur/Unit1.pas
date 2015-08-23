{
 Example showing usage of GLBlur

 Adding it to the scene root will blur all the scene.
 Adding a GLBlur to an object will make it blur only that object
 (note that you might need to sort objects to avoid z-order issues
  or you can set GLScene1.ObjectSorting = osRenderFarthestFirst)

 You can choose a GLBlur effect from the "presets" property or
 set the parameters yourself (see GLBlur.pas)

}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, GLMaterial,
  Dialogs, GLScene, GLObjects, GLTexture, GLHudObjects,Jpeg,
  GLCadencer, StdCtrls, ExtCtrls,GLBlur, LResources, GLViewer;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCube1: TGLCube;
    GLLightSource1: TGLLightSource;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLSphere1: TGLSphere;
    Panel1: TPanel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox2: TComboBox;
    Timer1: TTimer;
    GLDummyCube1: TGLDummyCube;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure ComboBox1Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Private declarations }
    oldx,oldy: integer;
  public
    { Public declarations }
    B : TGLBlur;

  end;

var
  Form1: TForm1;

implementation


procedure TForm1.FormCreate(Sender: TObject);
begin
     // Add GLBlur to scene
     B := TGLBlur.Create(self);
     GLCube1.AddChild(B); // Adding GLBlur to an object will make it blur only that object
     B.RenderWidth := 256;
     B.RenderHeight := 256;

     // Load texture for objects
     GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile('..' + PathDelim + '..' + PathDelim + 'media' + PathDelim + 'marbletiles.jpg');
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
     GLCube1.Turn(deltatime*10);
     GLSphere1.Turn(deltatime*50);
end;

procedure TForm1.ComboBox1Click(Sender: TObject);
begin
     B.Preset := TGLBlurPreset(ComboBox1.itemIndex);
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
     B.RenderWidth := StrToInt(ComboBox2.Items[ComboBox2.ItemIndex]);
     B.RenderHeight := B.RenderWidth;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
     Caption := Floattostr(Trunc(GLSceneViewer1.FramesPerSecond));
     GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
     if ssLeft in Shift then
     begin
          GLCamera1.MoveAroundTarget(0.2*(oldy-y),0.2*(oldx-x));
     end;
     oldx :=x;
     oldy :=y;
end;

initialization
  {$i Unit1.lrs}

end.
