{******************************************************************************
*                                                                             *
* Version: MPL 1.1                                                            *
*                                                                             *
* The contents of this file are subject to the Mozilla Public License Version *
* 1.1 (the "License"); you may not use this file except in compliance with    *
* the License. You may obtain a copy of the License at                        *
* http://www.mozilla.org/MPL/                                                 *
*                                                                             *
*                                                                             *
* Software distributed under the License is distributed on an "AS IS" basis,  *
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License    *
* for the specific language governing rights and limitations under the        *
* License.                                                                    *
*                                                                             *
* The Original Code is "oxGLODE opengl-physic-delphi-component".              *
*                                                                             *
* The Initial Developer of the Original Code is                               *
* Dave Gravel, OrionX3D Opengl & Delphi Programming, dave.gravel@cgocable.ca. *
*                       http://www.Dave.ServeUsers.com/compo.html             *
*                       http://www.k00m.sexidude.com/compo.html               *
*                       http://k00m.sytes.net/compo.html                      *
*                       http://24.122.8.52/compo.html                         *
*                                                                             *
* Portions created by Dave Gravel are Copyright (C) 2004 - 2005.              *
* Dave Gravel. All Rights Reserved.                                           *
*                                                                             *
* Contributor(s): GLScene (http://www.glscene.org) - ODE (http://ode.org) -   *
* DelphiODE (http://www.cambrianlabs.com/Mattias/DelphiODE) -                 *
* Stuart Gooding (N/A)                                                        *
*                                                                             *
*                                                                             *
*******************************************************************************
* oxGLODE v1.0PRE by k00m. (Dave Gravel)                                         *
*******************************************************************************}
unit Unit1;

{$MODE Delphi}

interface

uses
  LCLIntf, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLCadencer, GLViewer, GLObjects, ExtCtrls,
  GLkeyboard, GLGeomObjects, VectorGeometry, StdCtrls,
  GLTexture, GLHeightData,
  //
  GLOXOde, ODEImport, ODEGL, LResources, GLScene;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    Cam: TGLCamera;
    Ode: TGLOXOdeEngine;
    light: TGLLightSource;
    Timer1: TTimer;
    Scene: TGLDummyCube;
    DummyCam: TGLDummyCube;
    Box: TGLDummyCube;
    Box1: TGLOXStaBox;
    Ragdoll1: TGLOXRagdoll;
    Ragdoll2: TGLOXRagdoll;
    Ragdoll3: TGLOXRagdoll;
    Ragdoll4: TGLOXRagdoll;
    BodyParts1: TGLDummyCube;
    head1: TGLSphere;
    RightHand1: TGLSphere;
    LeftHand1: TGLSphere;
    AvantBrasDroite1: TGLCylinder;
    AvantBrasGauche1: TGLCylinder;
    EpauleDroite1: TGLSphere;
    EpauleGauche1: TGLSphere;
    BrasDroite1: TGLCylinder;
    BrasGauche1: TGLCylinder;
    HancheDroite1: TGLSphere;
    HancheGauche1: TGLSphere;
    Tors11: TGLCylinder;
    Tors12: TGLCylinder;
    Tors13: TGLCylinder;
    FootRight1: TGLSphere;
    FootLeft1: TGLSphere;
    BodyParts2: TGLDummyCube;
    head2: TGLSphere;
    RightHand2: TGLSphere;
    LeftHand2: TGLSphere;
    AvantBrasDroite2: TGLCylinder;
    AvantBrasGauche2: TGLCylinder;
    EpauleDroite2: TGLSphere;
    EpauleGauche2: TGLSphere;
    BrasDroite2: TGLCylinder;
    BrasGauche2: TGLCylinder;
    HancheDroite2: TGLSphere;
    HancheGauche2: TGLSphere;
    Tors21: TGLCylinder;
    Tors22: TGLCylinder;
    Tors23: TGLCylinder;
    FootRight2: TGLSphere;
    FootLeft2: TGLSphere;
    BodyParts3: TGLDummyCube;
    head3: TGLSphere;
    RightHand3: TGLSphere;
    LeftHand3: TGLSphere;
    AvantBrasDroite3: TGLCylinder;
    AvantBrasGauche3: TGLCylinder;
    EpauleDroite3: TGLSphere;
    EpauleGauche3: TGLSphere;
    BrasDroite3: TGLCylinder;
    BrasGauche3: TGLCylinder;
    HancheDroite3: TGLSphere;
    HancheGauche3: TGLSphere;
    Tors31: TGLCylinder;
    Tors32: TGLCylinder;
    Tors33: TGLCylinder;
    FootRight3: TGLSphere;
    FootLeft3: TGLSphere;
    BodyParts4: TGLDummyCube;
    head4: TGLSphere;
    RightHand4: TGLSphere;
    LeftHand4: TGLSphere;
    AvantBrasDroite4: TGLCylinder;
    AvantBrasGauche4: TGLCylinder;
    EpauleDroite4: TGLSphere;
    EpauleGauche4: TGLSphere;
    BrasDroite4: TGLCylinder;
    BrasGauche4: TGLCylinder;
    HancheDroite4: TGLSphere;
    HancheGauche4: TGLSphere;
    Tors41: TGLCylinder;
    Tors42: TGLCylinder;
    Tors43: TGLCylinder;
    FootRight4: TGLSphere;
    FootLeft4: TGLSphere;
    JambeDroite1: TGLCylinder;
    JambeDroite2: TGLCylinder;
    JambeDroite3: TGLCylinder;
    JambeDroite4: TGLCylinder;
    CuisseDroite1: TGLCylinder;
    CuisseDroite2: TGLCylinder;
    CuisseDroite3: TGLCylinder;
    CuisseDroite4: TGLCylinder;
    JambeGauche1: TGLCylinder;
    CuisseGauche1: TGLCylinder;
    JambeGauche2: TGLCylinder;
    CuisseGauche2: TGLCylinder;
    JambeGauche3: TGLCylinder;
    CuisseGauche3: TGLCylinder;
    JambeGauche4: TGLCylinder;
    CuisseGauche4: TGLCylinder;
    yr1: TGLSphere;
    yl1: TGLSphere;
    yr2: TGLSphere;
    yl2: TGLSphere;
    yr3: TGLSphere;
    yl3: TGLSphere;
    yr4: TGLSphere;
    yl4: TGLSphere;
    Car1: TGLOXDynCar;
    frame1: TGLCube;
    wheel1: TGLSphere;
    wheel2: TGLSphere;
    wheel3: TGLSphere;
    wheel4: TGLSphere;
    Box2: TGLOXStaBox;
    Box3: TGLOXStaBox;
    Box4: TGLOXStaBox;
    Box5: TGLOXStaBox;
    Front1: TGLFrustrum;
    Box6: TGLOXStaBox;
    DynBall1: TGLOXDynBall;
    DynBall2: TGLOXDynBall;
    DynBox1: TGLOXDynBox;
    DynBox2: TGLOXDynBox;
    Box7: TGLOXStaBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OdeMultiStepRender(delta: Single);
  private
    { Private declarations }
    mx,my: integer;
    procedure ODEInit;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  SSpeed : single;

implementation


procedure TForm1.ODEInit;
var i: integer;
begin
  GLScene1.Free;
  GLScene1 := TGLScene.Create(form1);
  GLScene1.LoadFromFile('ODEScene.gls');
  GLCadencer1.Scene:= GLScene1;
  GLSceneViewer1.Camera := Cam;
  Ode.InitODE;
  Ode.OnMultiStepRender := OdeMultiStepRender;
  DynBall1.InitODE;
  DynBall2.InitODE;
  DynBox1.InitODE;
  DynBox2.InitODE;  
  with Car1 do
  begin
    Position.X := 0;
    Position.Y := 0;
    Position.Z := 1;
    InitODE;
    frame1.Parent:= Frame;
    Front1.Parent:= Frame;
    wheel1.Parent:= WheelLF;
    wheel2.Parent:= WheelLB;
    wheel3.Parent:= WheelRF;
    wheel4.Parent:= WheelRB;
  end;
  Cam.TargetObject := frame1;
  with Ragdoll1 do
  begin
    Position.X := 0;
    Position.Y := 3;
    Position.Z := 0.3;
    InitODE;
    head1.Parent:= BoneBox[19];
    RightHand1.Parent:= BoneBox[18];
    LeftHand1.Parent:= BoneBox[17];
    AvantBrasDroite1.Parent:= BoneBox[16];
    AvantBrasGauche1.Parent:= BoneBox[15];
    EpauleDroite1.Parent:= BoneBox[14];
    EpauleGauche1.Parent:= BoneBox[13];
    BrasDroite1.Parent:= BoneBox[12];
    BrasGauche1.Parent:= BoneBox[11];
    HancheDroite1.Parent:= BoneBox[10];
    HancheGauche1.Parent:= BoneBox[9];
    Tors11.Parent:= BoneBox[8];
    Tors12.Parent:= BoneBox[7];
    Tors13.Parent:= BoneBox[6];
    JambeDroite1.Parent:= BoneBox[4];
    CuisseDroite1.Parent:= BoneBox[3];
    JambeGauche1.Parent:= BoneBox[1];
    CuisseGauche1.Parent:= BoneBox[0];
    FootRight1.Parent:= BoneBox[5];
    FootLeft1.Parent:= BoneBox[2];
    //for i := 0 to 20-1 do
    //  BoneBox[i].VisibleAtRunTime := True;
  end;
  with Ragdoll2 do
  begin
    Position.X := -0.5;
    Position.Y := 3;
    Position.Z := 0.3;
    InitODE;
    head2.Parent:= BoneBox[19];
    RightHand2.Parent:= BoneBox[18];
    LeftHand2.Parent:= BoneBox[17];
    AvantBrasDroite2.Parent:= BoneBox[16];
    AvantBrasGauche2.Parent:= BoneBox[15];
    EpauleDroite2.Parent:= BoneBox[14];
    EpauleGauche2.Parent:= BoneBox[13];
    BrasDroite2.Parent:= BoneBox[12];
    BrasGauche2.Parent:= BoneBox[11];
    HancheDroite2.Parent:= BoneBox[10];
    HancheGauche2.Parent:= BoneBox[9];
    Tors21.Parent:= BoneBox[8];
    Tors22.Parent:= BoneBox[7];
    Tors23.Parent:= BoneBox[6];
    JambeDroite2.Parent:= BoneBox[4];
    CuisseDroite2.Parent:= BoneBox[3];
    JambeGauche2.Parent:= BoneBox[1];
    CuisseGauche2.Parent:= BoneBox[0];
    FootRight2.Parent:= BoneBox[5];
    FootLeft2.Parent:= BoneBox[2];
    //for i := 0 to 20-1 do
    //  BoneBox[i].VisibleAtRunTime := True;
  end;
  with Ragdoll3 do
  begin
    Position.X := -1;
    Position.Y := 3;
    Position.Z := 0.3;
    InitODE;
    head3.Parent:= BoneBox[19];
    RightHand3.Parent:= BoneBox[18];
    LeftHand3.Parent:= BoneBox[17];
    AvantBrasDroite3.Parent:= BoneBox[16];
    AvantBrasGauche3.Parent:= BoneBox[15];
    EpauleDroite3.Parent:= BoneBox[14];
    EpauleGauche3.Parent:= BoneBox[13];
    BrasDroite3.Parent:= BoneBox[12];
    BrasGauche3.Parent:= BoneBox[11];
    HancheDroite3.Parent:= BoneBox[10];
    HancheGauche3.Parent:= BoneBox[9];
    Tors31.Parent:= BoneBox[8];
    Tors32.Parent:= BoneBox[7];
    Tors33.Parent:= BoneBox[6];
    JambeDroite3.Parent:= BoneBox[4];
    CuisseDroite3.Parent:= BoneBox[3];
    JambeGauche3.Parent:= BoneBox[1];
    CuisseGauche3.Parent:= BoneBox[0];
    FootRight3.Parent:= BoneBox[5];
    FootLeft3.Parent:= BoneBox[2];
    //for i := 0 to 20-1 do
    //  BoneBox[i].VisibleAtRunTime := True;
  end;
  with Ragdoll4 do
  begin
    Position.X := 0.5;
    Position.Y := 3;
    Position.Z := 0.3;
    InitODE;
    head4.Parent:= BoneBox[19];
    RightHand4.Parent:= BoneBox[18];
    LeftHand4.Parent:= BoneBox[17];
    AvantBrasDroite4.Parent:= BoneBox[16];
    AvantBrasGauche4.Parent:= BoneBox[15];
    EpauleDroite4.Parent:= BoneBox[14];
    EpauleGauche4.Parent:= BoneBox[13];
    BrasDroite4.Parent:= BoneBox[12];
    BrasGauche4.Parent:= BoneBox[11];
    HancheDroite4.Parent:= BoneBox[10];
    HancheGauche4.Parent:= BoneBox[9];
    Tors41.Parent:= BoneBox[8];
    Tors42.Parent:= BoneBox[7];
    Tors43.Parent:= BoneBox[6];
    JambeDroite4.Parent:= BoneBox[4];
    CuisseDroite4.Parent:= BoneBox[3];
    JambeGauche4.Parent:= BoneBox[1];
    CuisseGauche4.Parent:= BoneBox[0];
    FootRight4.Parent:= BoneBox[5];
    FootLeft4.Parent:= BoneBox[2];
    //for i := 0 to 20-1 do
    //  BoneBox[i].VisibleAtRunTime := True;
  end;
  for i:= 0 to Box.Count-1 do
  begin
    TGLOXStaBox(Box.Children[i]).InitODE;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLScene1.SaveToFile('ODEScene.gls');
  SSpeed:= 0.0;
  ODEInit;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  if IsKeyDown(VK_SPACE) then
  begin
    ODEInit;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  GLCadencer1.Free;
  GLScene1.Free;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin

end;

procedure TForm1.FormShow(Sender: TObject);
begin

end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption:= Format('%.2f FPS'+' Use arrow key and Space to reset.', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx:= x;
  my:= y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssright in shift then
    Cam.MoveAroundTarget(my-y,mx-x);
  mx:= x;
  my:= y;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Cam.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TForm1.OdeMultiStepRender(delta: Single);
begin
 if Car1.Steer >= 1 then
    Car1.Steer := 1;
  if Car1.Steer <= -1 then
    Car1.Steer := -1;
  if Car1.Speed >= 100 then
    Car1.Speed := 100;
  if Car1.Speed <= -25 then
    Car1.Speed := -25;
  if ( not IsKeyDown( VK_UP ) and not IsKeyDown( VK_DOWN ) ) then
  begin
    if Car1.Speed > 0 then
      Car1.Speed := Car1.Speed -0.05;
  end;
  if IsKeyDown( VK_RIGHT ) then
    Car1.Steer := Car1.Steer + Car1.TURN_SPEED
  else
  if IsKeyDown( VK_LEFT ) then
    Car1.Steer := Car1.Steer - Car1.TURN_SPEED
  else
    Car1.Steer:=Car1.Steer*0.1;
    if not IsKeyDown(' ') then
      if IsKeyDown( VK_UP ) then
      begin
        Car1.MotorRunning := true;
        if ( Car1.Speed >= 0 ) then
          Car1.Speed := Car1.Speed + Car1.ACCEL
        else
          Car1.Speed := 100;
        end
        else
        if IsKeyDown( VK_DOWN ) then
        begin
          Car1.MotorRunning := True;
          if ( Car1.Speed <= 0 ) then
            Car1.Speed := Car1.Speed - Car1.ACCEL
          else
            Car1.Speed := -0;
        end
        else
        begin
          Car1.MotorRunning := false;
          Car1.Speed := Car1.Speed;
        end;
end;

initialization
  {$i Unit1.lrs}

end.
