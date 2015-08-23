{: Demo of the TGLPoints component.<p>

   The component is specialized in rendering large numbers of points,
   with ability to adjust point style (from fast square point to smooth
   round points) and point parameters.<p>
   The point parameters define how point size is adjusted with regard
   to eye-point distance (to make farther points smaller, see ARB_point_parameters
   for more details).<p>
   The component is also suitable for particle systems, but offers less
   flexibility than the TGLParticleFX.
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLObjects, GLViewer, StdCtrls, VectorGeometry, VectorLists,
  GLCadencer, GLTexture, GLColor, ExtCtrls, LResources;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    GLPoints1: TGLPoints;
    GLCadencer1: TGLCadencer;
    GLPoints2: TGLPoints;
    Panel1: TPanel;
    CBPointParams: TCheckBox;
    CBAnimate: TCheckBox;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure CBAnimateClick(Sender: TObject);
    procedure CBPointParamsClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    mx, my : Integer
  end;

var
  Form1: TForm1;

implementation


const
   cNbPoints = 180;

procedure TForm1.FormCreate(Sender: TObject);
begin
   // allocate points in the 1st point set
   GLPoints1.Positions.Count:=cNbPoints;
   // specify white color for the 1st point set
   // (if a single color is defined, all points will use it,
   // otherwise, it's a per-point coloring)
   GLPoints1.Colors.Add(clrWhite);
   // specify blue color for the 2nd point set
   GLPoints2.Colors.Add(clrBlue);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   i : Integer;
   f, a, ab, ca, sa : Single;
   p : TAffineVectorList;
   v : TAffineVector;
begin
   if CBAnimate.Checked then begin
      // update the 1st point set with values from a math func
      f:=1+Cos(newTime);
      p:=GLPoints1.Positions;
      ab:=newTime*0.1;
      for i:=0 to cNbPoints-1 do begin
         a:=DegToRad(4*i)+ab;
         SinCos(a, sa, ca);
         v[0]:=2*ca;
         v[1]:=2*Cos(f*a);
         v[2]:=2*sa;
         p[i]:=v;
      end;
      // replicate points in second set
      GLPoints2.Positions:=GLPoints1.Positions;
   end;
   GLSceneViewer1.Invalidate;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x;
   my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x;
      my:=y;
   end;
end;

procedure TForm1.CBAnimateClick(Sender: TObject);
begin
   GLPoints1.Static:=not CBAnimate.Checked;
   GLPoints2.Static:=not CBAnimate.Checked;
end;

procedure TForm1.CBPointParamsClick(Sender: TObject);
begin
   GLPoints1.PointParameters.Enabled:=CBPointParams.Checked;
   GLPoints2.PointParameters.Enabled:=CBPointParams.Checked;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

initialization
  {$i Unit1.lrs}

end.
