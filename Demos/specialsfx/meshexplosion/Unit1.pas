{: Explosion FX Demo (Matheus, matheus@tilt.net)<p>

This project demonstrates the use of TGLBExplosionFx. Nothing out
of ordinary as one can see. Load the mesh, load the default settings,
click "on" to initiate the demo, "reset" to reset :)<p>

The information of the mesh is cached on the cache variable, that is
restored every time the demo is reseted. The MaxSteps property defines
the max number of frames the explosion will be rendered. Speed is the
scalar speed each face is issued in the rendering<p>
}
unit Unit1;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, GLVectorFileObjects, GLCadencer, GLExplosionFx,
  GLFile3DS, ExtCtrls, LResources,
  GLScene, Buttons, GLLCLViewer, GLViewer;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    viewer: TGLSceneViewer;
    GLScene1: TGLScene;
    Camera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    mesh: TGLFreeForm;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    CheckOn: TCheckBox;
    StepBar: TProgressBar;
    Label2: TLabel;
    MaxStepsBar: TTrackBar;
    Label1: TLabel;
    Label3: TLabel;
    SpeedBar: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure CheckOnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure viewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure viewerMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure SpeedBarChange(Sender: TObject);
    procedure MaxStepsBarChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  vx, vy: integer;
  Cache: TMeshObjectList;

implementation


procedure TForm1.FormCreate(Sender: TObject);
var
exp: TGLBExplosionFx;
begin
     //load mesh
     mesh.LoadFromFile('..' + PathDelim + '..' + PathDelim + 'media' + PathDelim + 'mushroom.3ds');
     //cache information
     cache:= TMeshObjectList.Create;
     cache.Assign(mesh.MeshObjects);
     //default settings
     exp:= TGLBExplosionFx(mesh.effects.items[0]);
     exp.MaxSteps:= 0;
     exp.Speed:= 0.1;
end;

procedure TForm1.CheckOnClick(Sender: TObject);
begin
     //turn on/off
     TGLBExplosionFx(mesh.Effects.items[0]).Enabled:= checkon.checked;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
     //reset simulation
     TGLBExplosionFx(mesh.effects.items[0]).Reset;
     checkon.checked:= false;
     //restore the mesh
     mesh.MeshObjects.Assign(Cache);
     mesh.StructureChanged;
end;

procedure TForm1.viewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
     if shift <> [ssLeft] then exit;

     camera1.MoveAroundTarget(y - vy, x - vx);
     vx:= x; vy:= y;
end;

procedure TForm1.viewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     vx:= x; vy:= y;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
     viewer.Invalidate;
     StepBar.Position:= TGLBExplosionFx(mesh.Effects.items[0]).Step;
end;

procedure TForm1.SpeedBarChange(Sender: TObject);
begin
     TGLBExplosionFx(mesh.Effects.items[0]).Speed:= speedBar.Position / 10;
end;

procedure TForm1.MaxStepsBarChange(Sender: TObject);
begin
     TGLBExplosionFx(mesh.Effects.items[0]).MaxSteps:= MaxStepsBar.Position;
     stepBar.Max:= MaxStepsBar.Position;
end;

initialization
  {$i Unit1.lrs}

end.
