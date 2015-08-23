{: GLCameraController demo.<p>

   This demo shows how the TGLCameraController can be used to control the
   camera's movement around a target using minimal code.

}
unit Main;

{$MODE Delphi}

interface

uses
  SysUtils, LCLIntf, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, GLViewer, GLScene, GLObjects,
  GLVectorFileObjects, GLCameraController, GLFile3ds, GLGeomObjects, GLTexture,
  GLCadencer, StdCtrls, ComCtrls, LResources, GLMaterial,
  GLCoordinates, GLCrossPlatform,
  BaseClasses;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    eDestX: TEdit;
    eDestY: TEdit;
    eDestZ: TEdit;
    Label5: TLabel;
    GLScene1: TGLScene;
    GLDummyCube1: TGLDummyCube;
    GLSphere1: TGLSphere;
    GLCylinder1: TGLCylinder;
    GLSphere2: TGLSphere;
    GLCamera: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLSceneViewer1: TGLSceneViewer;
    GLCameraController1: TGLCameraController;
    GLSphere3: TGLSphere;
    Panel3: TPanel;
    Label6: TLabel;
    Label9: TLabel;
    eDistance: TEdit;
    Panel4: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    eSafeDistance: TEdit;
    eTimeToSafePlacement: TEdit;
    eTimeToOrbit: TEdit;
    Label12: TLabel;
    eTimeToZoomBackIn: TEdit;
    Panel5: TPanel;
    Label13: TLabel;
    eTime: TEdit;
    Label14: TLabel;
    btnMoveToPos: TButton;
    btnZoomToDistance: TButton;
    btnOrbitToPos: TButton;
    btnSafeOrbitAndZoomToPos: TButton;
    pImg: TPanel;
    Image1: TImage;
    Label4: TLabel;
    Panel6: TPanel;
    Label15: TLabel;
    btnStopMovement: TButton;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure btnMoveToPosClick(Sender: TObject);
    procedure btnZoomToDistanceClick(Sender: TObject);
    procedure btnOrbitToPosClick(Sender: TObject);
    procedure btnSafeOrbitAndZoomToPosClick(Sender: TObject);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnStopMovementClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    DextX, DextY, DextZ, Time, ZoomDistance: double;
    mx, my : Integer;
    procedure GetInput(Sender:TButton);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Math;


procedure TForm1.GetInput(Sender:TButton);
begin
  if (Sender.Name='btnMoveToPos') or
     (Sender.Name='btnOrbitToPos') or
     (Sender.Name='btnSafeOrbitAndZoomToPos') then
  begin
    DextX:=strtofloat(eDestX.text);
    DextY:=strtofloat(eDestY.text);
    DextZ:=strtofloat(eDestZ.text);
  end;
  if (Sender.Name='btnMoveToPos') or
     (Sender.Name='btnZoomToDistance') or
     (Sender.Name='btnOrbitToPos') then
  begin
    Time:= strtofloat(eTime.text);
  end;
  if (Sender.Name='btnZoomToDistance')then
  begin
    ZoomDistance:= strtofloat(eDistance.text)
  end;
  if (Sender.Name='btnSafeOrbitAndZoomToPos')then
  begin
    GLCameraController1.soSafeDistance := strtofloat(eSafeDistance.text);
    GLCameraController1.soTimeToSafePlacement := strtofloat(eTimeToSafePlacement.text);
    GLCameraController1.soTimeToOrbit := strtofloat(eTimeToOrbit.text);
    GLCameraController1.soTimeToZoomBackIn := strtofloat(eTimeToZoomBackIn.text);
  end;
end;

//MoveToPos Usage
procedure TForm1.btnMoveToPosClick(Sender: TObject);
begin
  if not GLCameraController1.AllowUserAction then exit;
  GetInput(TButton(Sender));
  GLCameraController1.MoveToPos(DextX, DextY, DextZ, Time);
end;

//ZoomToDistance Usage
procedure TForm1.btnZoomToDistanceClick(Sender: TObject);
begin
  if not GLCameraController1.AllowUserAction then exit;
  GetInput(TButton(Sender));
  GLCameraController1.ZoomToDistance(ZoomDistance,Time);
end;

//OrbitToPos Usage
procedure TForm1.btnOrbitToPosClick(Sender: TObject);
begin
  if not GLCameraController1.AllowUserAction then exit;
  GetInput(TButton(Sender));
  GLCameraController1.OrbitToPos(DextX, DextY, DextZ, Time);
end;

//SafeOrbitAndZoomToPos Usage
procedure TForm1.btnSafeOrbitAndZoomToPosClick(Sender: TObject);
begin
  if not GLCameraController1.AllowUserAction then exit;
  GetInput(TButton(Sender));
  GLCameraController1.SafeOrbitAndZoomToPos(DextX, DextY, DextZ);
end;

//GUI Implementation - Pay attention to GLCameraController1.AllowUserAction!

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if not GLCameraController1.AllowUserAction then exit;
	GLCamera.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
  GLCamera.DepthOfView:=2*GLCamera.DistanceToTarget+2*GLcamera.TargetObject.BoundingSphereRadius;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   if not GLCameraController1.AllowUserAction then exit;
   if Shift=[ssLeft] then
   begin
     mx:=x; my:=y;
   end;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not GLCameraController1.AllowUserAction then exit;
  if Shift=[ssLeft] then
  begin
    GLCamera.MoveAroundTarget(my-y, mx-x);
    mx:=x; my:=y;
    caption:= 'CameraController Demo - camera position = '+formatfloat('0.##',glcamera.position.x)+'/'+formatfloat('0.##',glcamera.position.y)+'/'+formatfloat('0.##',glcamera.position.z);
  end;
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not GLCameraController1.AllowUserAction then exit;
  caption:= 'CameraController Demo';
end;

procedure TForm1.btnStopMovementClick(Sender: TObject);
begin
  GLCameraController1.StopMovement;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCameraController1.StopMovement;
end;

initialization
  {$i Main.lrs}
  DecimalSeparator := '.';

end.
