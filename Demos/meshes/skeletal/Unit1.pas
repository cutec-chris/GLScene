{: Basic Skeletal Animation sample.<p>

   This demo loads a SMD model and 3 of its animation (SMD are part of and
   Half-Life MDL file, and may be extracted to individual files with tools
   like MilkShape).<br>
   SMD loading order matters: the "model" SMD must be loaded first, it contains
   bones and vertex data, the "animation" SMD are loaded afterwards with
   AddDataFromFile, they contain only bone animation data. Don't forget to link
   the actor to a material library, SMD models commonly use several textures!<p>

   If you hit one of the jump buttons, the character will perform the jump,
   and once it has been completed, revert to the walk or run animation.<p>

   The slider makes the character look left/right by using blending (through
   and animation controler).<p>

   Why, why, why didn't the model moves it arms? Because it's not
   in the animations frames! HL uses blends to move the arms and accomodate gestures
   such has aiming a weapon...<p>
   Side note: the look_left_right.smd animation was added by me, so don't blame
   the model's author if it ain't anatomically correct (hand edited smd with
   only three keyframes).<p>

   Model Author: Neal 'Guplik' Corbett, edited by ~A.u.s.t.i.n. (manny@cgocable.net)<br>
   Thanks!
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLVectorFileObjects, GLScene, StdCtrls, GLObjects, GLTexture,
  ExtCtrls, GLViewer, GLGraph, ComCtrls, GLFileSMD, LResources,
  GLCadencer, Buttons, JPEG, GLMaterial;

type

  { TForm1 }

  TForm1 = class(TForm)
    BULongJump1: TButton;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Actor1: TGLActor;
    DummyCube1: TGLDummyCube;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    BUHighJump: TButton;
    XYZGrid1: TGLXYZGrid;
    RBWalk: TRadioButton;
    RBRun: TRadioButton;
    AnimationControler1: TGLAnimationControler;
    Panel2: TPanel;
    TrackBar1: TTrackBar;
    CBBlend: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure BULongJumpClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Actor1EndFrameReached(Sender: TObject);
    procedure BUHighJumpClick(Sender: TObject);
    procedure RBWalkClick(Sender: TObject);
    procedure RBRunClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure CBBlendClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    baseAnimation : String;
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation


procedure TForm1.FormCreate(Sender: TObject);
begin
   SetCurrentDir(ExtractFilePath(Application.ExeName)+'..' + PathDelim + '..' + PathDelim + 'media');
   // We load the SMD model here
   // Note the actor was linked to a material library, and textures are loaded
   // automatically (4 textures are used by this model)
   //
   // Kind thanks to ~A.u.s.t.i.n. & Neal 'Guplik' Corbett for the model
   // and allowing its use ;)
   Actor1.LoadFromFile('TRINITYrage.smd');
   // Now we load the walk & run animations and "fix" their translation
   // (HL walk/run animations have a built-in "slide" that we don't want here)
   Actor1.AddDataFromFile('walk.smd');
   Actor1.Animations[1].MakeSkeletalTranslationStatic;
   Actor1.AddDataFromFile('run.smd');
   Actor1.Animations[2].MakeSkeletalTranslationStatic;
   // Then load the two jumps
   Actor1.AddDataFromFile('long_jump.smd');
   Actor1.AddDataFromFile('jump.smd');
   // And the 'look_left_right' blending animations, that we immediately
   // assign to the controler. The MakeSkeletalRotationDelta removes absolute
   // information from the SMD (which HL may use, but GLScene doesn't)
   Actor1.AddDataFromFile('look_left_right.smd');
   Actor1.Animations[5].MakeSkeletalRotationDelta;
   AnimationControler1.AnimationName:='look_left_right';
   // Skeleton visible, and start with walk animation
   // (pseudo-animation 0 is for the static model in its default attitude)
   Actor1.OverlaySkeleton:=True;
   baseAnimation:='walk';
   Actor1.SwitchToAnimation(baseAnimation);
end;

procedure TForm1.RBWalkClick(Sender: TObject);
begin
   // user requested 'walk'
   baseAnimation:='walk';
   Actor1.SwitchToAnimation(baseAnimation, True);
end;

procedure TForm1.RBRunClick(Sender: TObject);
begin
   // user requested 'run'
   baseAnimation:='run';
   Actor1.SwitchToAnimation(baseAnimation, True);
end;

procedure TForm1.BULongJumpClick(Sender: TObject);
begin
   // Smoothly switch to Long Jump
   Actor1.SwitchToAnimation(3, True);
end;

procedure TForm1.BUHighJumpClick(Sender: TObject);
begin
   // Smoothly switch to High Jump
   Actor1.SwitchToAnimation(4, True);
end;

procedure TForm1.Actor1EndFrameReached(Sender: TObject);
begin
   // If we weren't walking, switch back to walk
   if Actor1.CurrentAnimation<>baseAnimation then
      Actor1.SwitchToAnimation(baseAnimation, True);
end;

procedure TForm1.CBBlendClick(Sender: TObject);
begin
   // Enable/disable blending by binding or unbinding the animation controler
   // to the actor 
   if CBBlend.Checked then begin
      AnimationControler1.Actor:=Actor1;
      TrackBar1Change(Self);
   end else AnimationControler1.Actor:=nil;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
   // Blending along the controler's animation is just a matter of adjusting
   // the ratio, with 0 = first frame and 1 = last frame.
   AnimationControler1.Ratio:=TrackBar1.Position*0.01;
end;

// Nothing fancy below, just the same old stuff

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
   Actor1.OverlaySkeleton:=CheckBox1.Checked;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x; my:=y;
   end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   GLScene1.NotifyChange(nil);
end;

initialization
  {$i Unit1.lrs}

end.
