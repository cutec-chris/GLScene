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
* oxGLODE v1.0PRE alpha by k00m. (Dave Gravel)                                   *
*******************************************************************************}
unit oxMain;

{$MODE Delphi}

interface

uses
  LCLIntf, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,  GLCadencer, GLScene, ExtCtrls, GLObjects,
  VectorGeometry, StdCtrls, GLTexture, GLShadowPlane, GLGeomObjects, Math,
  GLVectorFileObjects, GLTerrainRenderer, Opengl1x, GLHeightData, GLTrail,
  GLHUDObjects, GLBlur, GLMesh, GLkeyboard, GLFile3DS,
  // ODE unit.
  ODEGL, odeimport, GLOxOde, LResources, GLViewer, Buttons;

const		       
  cADD = 0;
  cVERTEX_COUNT = 5;
  cFACE_COUNT = 4;
  cINDEX_COUNT = cFACE_COUNT * 3;

type

  { ToxBoxStackFrm }

  ToxBoxStackFrm = class(TForm)
    Button1: TButton;
    GLScene1: TGLScene;
    GLCadencer1: TGLCadencer;
    GLSceneViewer1: TGLSceneViewer;
    GLCam: TGLCamera;
    Timer1: TTimer;
    CamBox: TGLDummyCube;
    Panel1: TPanel;
    oxODE: TGLoxODEEngine;
    Button2: TButton;
    GLShadowPlane1: TGLShadowPlane;
    GLLightSource1: TGLLightSource;
    GLCone1: TGLCone;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    CheckBox_DisableStillBodies: TCheckBox;
    RadioGroup_Solver: TRadioGroup;
    Button6: TButton;
    Button7: TButton;
    Scene: TGLDummyCube;
    Button8: TButton;
    Button9: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    Button10: TButton;
    Edit5: TEdit;
    Label5: TLabel;
    Button11: TButton;
    Button12: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
    newTime: Double);
    procedure Button1Click(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
    Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
    MousePos: TPoint; var Handled: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure RadioGroup_SolverClick(Sender: TObject);
    procedure CheckBox_DisableStillBodiesClick(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure Edit2KeyPress(Sender: TObject; var Key: Char);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure oxODECollisionEvent(Geom1, Geom2: PdxGeom);
    procedure oxODEMultiStepRender(delta: Single);
    procedure oxODEStepRender(delta: Single);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
  private
    { Private declarations }
    mx,my: integer;
  public
    { Public declarations }
    DBall : TGLOXDynBall;
    DBox : TGLOXDynBox;
    DCylinder : TGLOXDynCylinder;
    DCCylinder : TGLOXDynCCylinder;
    DCone : TGLOXDynCone;
    DMesh : TGLOXDynMesh;
    SMesh : TGLOXStaMesh;
    DRagdoll : TGLOXRagdoll;
    DAMotor : TGLOXAMotor;
    DCar : TGLoxDynCar;
    IsEnabled : boolean;
    Floor : PdxGeom;
    FloorBox : PdxGeom;
    function RandomColor : TColorVector;
    procedure MaxObjs;
    procedure RandomPositionAndRotation( obj: TGLCustomSceneObject );
    function ConeMesh : TMeshObject;
    //procedure MotorUpdate;
  end;

var
  Vertices : array[0..cVERTEX_COUNT-1] of TdVector3;
  Indices : array[0..cINDEX_COUNT-1] of integer;  

var
  oxBoxStackFrm: ToxBoxStackFrm;

implementation


function ToxBoxStackFrm.ConeMesh : TMeshObject;
const
  cHeight = 0.1;
  cSign = 1;
var
  FG : TFGVertexNormalTexIndexList;
  Size : TdVector3;
  i : integer;
begin
  Size[0] := 2;
  Size[1] := 2;
  Size[2] := 1;
  Vertices[0,0] := -Size[0];
  Vertices[0,1] := -Size[1];
  Vertices[0,2] := cSign * Size[2]+cHeight;
  Vertices[1,0] := Size[0];
  Vertices[1,1] := -Size[1];
  Vertices[1,2] := cSign * Size[2]+cHeight;
  Vertices[2,0] := Size[0];
  Vertices[2,1] := Size[1];
  Vertices[2,2] := cSign * Size[2]+cHeight;
  Vertices[3,0]:=  -Size[0];
  Vertices[3,1] := Size[1];
  Vertices[3,2] := cSign * Size[2]+cHeight;
  Vertices[4,0] := 0;
  Vertices[4,1] := 0;
  Vertices[4,2] := 0+cHeight;
  Indices[0] := 0;
  Indices[1] := 1;
  Indices[2] := 4;
  Indices[3] := 1;
  Indices[4] := 2;
  Indices[5] := 4;
  Indices[6] := 2;
  Indices[7] := 3;
  Indices[8] := 4;
  Indices[9] := 3;
  Indices[10] := 0;
  Indices[11] := 4;
  Result := TMeshObject.Create;
  Result.Mode := momFaceGroups;
  for i := 0 to cVERTEX_COUNT -1 do
    Result.Vertices.Add( ( PAffineVector( @Vertices[i] ) ) ^ );
  FG := TFGVertexNormalTexIndexList.CreateOwned( Result.FaceGroups );
  FG.Mode := fgmmTriangles;
  for i := 0 to cFACE_COUNT -1 do
    FG.VertexIndices.Add( Indices[ i * 3 + 0 ], Indices[ i * 3 + 1 ], Indices[ i * 3 + 2 ] );
  Result.BuildNormals( FG.VertexIndices, momTriangles, FG.NormalIndices );
end;

procedure ToxBoxStackFrm.MaxObjs;
begin
  if oxODE.Count > StrToInt( Edit2.Text ) -1 then
  begin
    oxODE.Children[0].Free;
  end;
end;

procedure ToxBoxStackFrm.RandomPositionAndRotation( obj: TGLCustomSceneObject );
begin
  Randomize;
  with obj do
  begin
    Position.X := random * 2 - 1;
    Position.Y := random * 2 - 1;
    Position.Z := random * 2 + 3;
    if ( not (obj is TGLOXRagdoll) )
    and ( not (obj is TGLOXDynCar) )
    and ( not (obj is TGLOXAMotor) ) then
    begin
      Pitch( random * 5.0 -1.0 ); Roll( random * 5.0 -1.0 ); Turn( random * 5.0 -1.0 );
    end;  
  end;
end;

function ToxBoxStackFrm.RandomColor : TColorVector;
begin
  Randomize;
  result[0] := random;
  result[1] := random;
  result[2] := random;
  result[3] := 1;
end;

procedure ToxBoxStackFrm.FormCreate(Sender: TObject);
begin
  Show;
  Randomize;
  DecimalSeparator := '.';
  oxODE.InitODE;
  {dCreatePlane( OXEngine.oxSpace, 0, 1, 0, -GLShadowPlane1.Height/2 );
  dCreatePlane( OXEngine.oxSpace, 1, 0, 0, -GLShadowPlane1.Height/2 );
  dCreatePlane( OXEngine.oxSpace, 0,-1, 0, -GLShadowPlane1.Height/2 );
  dCreatePlane( OXEngine.oxSpace, -1, 0, 0, -GLShadowPlane1.Height/2 );}
  RadioGroup_Solver.OnClick( self );
  CheckBox_DisableStillBodies.OnClick( self );
  Edit3.OnChange( self );
  Edit4.OnChange( self );
  //Floor := dCreatePlane( oxODE.oxSpace, 0, 0, 1, 0 );
  GLShadowPlane1.Material.Texture.Disabled := False;
  GLShadowPlane1.Material.Texture.Image.LoadFromFile( 'OxEngine.bmp' );
  FloorBox := dCreateBox( oxODE.Space, 10, 10, 0.5 );
  dGeomSetPosition( FloorBox, 0, 0, -0.25 );
  GLCadencer1.Enabled := True;
  Timer1.Enabled := True;
  GLSceneViewer1.Camera := GLCam;
end;

procedure ToxBoxStackFrm.Button1Click(Sender: TObject);
begin
  MaxObjs;
  DBall := TGLOXDynBall( oxODE.AddNewChild( TGLOXDynBall ) );
  RandomPositionAndRotation( DBall );
  with DBall do
  begin
    Manager := oxODE;
    Modes := [ mdSoftCFM, mdBounce ];
    Mu := 5;
    Bounce := 0.65;
    Bounce_vel := 0.2;
    Soft_cfm := 0.01;
    Mass := 1;
    Approx1 := True;
    ContactNum := 2;
    FrictionForce := 0.035;
    RollFriction := True;
    AutoDisable := True;
    DisableThreshold := StrToFloat( Edit4.Text );
    DisableSteps := StrToFloat( Edit3.Text );
    Radius := ( random * 0.25 + 0.05 ) + cADD;
    Material.FrontProperties.Shininess := 8;
    Material.FrontProperties.Diffuse.Color := RandomColor;
    Material.FrontProperties.Specular.AsWinColor := $00B9FFFF;
    InitODE;
  end;
end;

procedure ToxBoxStackFrm.Button2Click(Sender: TObject);
begin
  MaxObjs;
  DBox := TGLOXDynBox( oxODE.AddNewChild( TGLOXDynBox ) );
  RandomPositionAndRotation( DBox );
  with DBox do
  begin
    Manager := oxODE;
    Modes := [ mdSoftCFM ];
    Mu := 5;
    Soft_cfm := 0.001;
    Mass := 1;
    ContactNum := 4;
    FrictionForce := 0.035;
    RollFriction := True;
    AutoDisable := True;
    DisableThreshold := StrToFloat( Edit4.Text );
    DisableSteps := StrToFloat( Edit3.Text );
    CubeWidth := ( random * 0.5 + 0.1 ) + cADD;
    CubeHeight := ( random * 0.5 + 0.1 ) + cADD;
    CubeDepth := ( random * 0.5 + 0.1 ) + cADD;
    Material.FrontProperties.Shininess := 8;
    Material.FrontProperties.Diffuse.Color := RandomColor;
    Material.FrontProperties.Specular.AsWinColor := $00B9FFFF;
    InitODE;
  end;
end;

procedure ToxBoxStackFrm.Button3Click(Sender: TObject);
begin
  MaxObjs;
  DCylinder := TGLOXDynCylinder( oxODE.AddNewChild( TGLOXDynCylinder ) );
  RandomPositionAndRotation( DCylinder );
  with DCylinder do
  begin
    Manager := oxODE;
    Modes := [ mdSoftCFM ];
    Mu := 5;
    Density := 0.25;
    Mass := 1;
    Soft_cfm := 0.001;
    ContactNum := 16;
    AutoDisable := True;
    RollFriction := True;
    FrictionForce := 0.035;
    DisableThreshold := StrToFloat( Edit4.Text );
    DisableSteps := StrToFloat( Edit3.Text );
    Height := ( random * 0.5 + 0.5 );
    BottomRadius := 0.1 + ( random * 0.2 + 0.05 ) + cADD;
    Material.FrontProperties.Shininess := 8;
    Material.FrontProperties.Diffuse.Color := RandomColor;
    Material.FrontProperties.Specular.AsWinColor := $00B9FFFF;
    InitODE;
  end;
end;

procedure ToxBoxStackFrm.Button4Click(Sender: TObject);
begin
  MaxObjs;
  DCCylinder := TGLOXDynCCylinder( oxODE.AddNewChild( TGLOXDynCCylinder ) );
  RandomPositionAndRotation( DCCylinder );
  with DCCylinder do
  begin
    Manager := oxODE;
    Modes := [ mdSoftCFM ];
    Mu := 5;
    Mu2 := 0;
    Density := 0.25;
    Mass := 1;
    Soft_cfm := 0.001;
    ContactNum := 16;
    RollFriction := True;
    FrictionForce := 0.035;
    AutoDisable := True;
    DisableThreshold := StrToFloat( Edit4.Text );
    DisableSteps := StrToFloat( Edit3.Text );
    Slices := 12;
    Stacks := 12;
    Height := ( random * 0.5 + 0.5 );
    BottomRadius := 0.1 + ( random * 0.15 + 0.035 ) + cADD;
    Material.FrontProperties.Shininess := 8;
    Material.FrontProperties.Diffuse.Color := RandomColor;
    Material.FrontProperties.Specular.AsWinColor := $00B9FFFF;
    InitODE;
  end;
end;

procedure ToxBoxStackFrm.Button6Click(Sender: TObject);
begin
  MaxObjs;
  DMesh := TGLOXDynMesh( oxODE.AddNewChild( TGLOXDynMesh ) );
  with DMesh do
  begin
    Manager := oxODE;
    Modes := [ mdSoftCFM, mdBounce ];
    Mu := 1000;
    Bounce := 0.5;
    Bounce_vel := 0.1;
    Soft_cfm := 0.01;
    ContactNum := 511;
    Mass := 1;
    RollFriction := True;
    FrictionForce := 0.035;
    AutoDisable := True;
    DisableThreshold := StrToFloat( Edit4.Text );
    DisableSteps := StrToFloat( Edit3.Text );
    AutoCentering := [ macUseBarycenter ];
    LoadFromFile( 'bunny.3ds' );
    Scale.X := 0.05;
    Scale.Y := 0.05;
    Scale.Z := 0.05;
    Material.FrontProperties.Shininess := 8;
    Material.FrontProperties.Diffuse.Color := RandomColor;
    Material.FrontProperties.Specular.AsWinColor := $00B9FFFF;
    InitODE;
    dBodySetPosition( Body, random * 2 - 1, random * 2 - 1, random * 2 + 3 );
  end;
end;

procedure ToxBoxStackFrm.Button9Click(Sender: TObject);
begin
  MaxObjs;
  DCone := TGLOXDynCone( oxODE.AddNewChild( TGLOXDynCone ) );
  RandomPositionAndRotation( DCone );
  with DCone do
  begin
    Manager := oxODE;
    Modes := [ mdBounce, mdSoftCFM ];
    Mu := 5;
    Mu2 := 0;
    Soft_cfm := 0.001;
    RollFriction := True;
    Mass := 1;
    FrictionForce := 0.035;
    Bounce := 0.1;
    Bounce_Vel := 0.1;
    ContactNum := 12;
    AutoDisable := True;
    DisableThreshold := StrToFloat( Edit4.Text );
    DisableSteps := StrToFloat( Edit3.Text );
    Material.FrontProperties.Shininess := 8;
    Material.FrontProperties.Diffuse.Color := RandomColor;
    Material.FrontProperties.Specular.AsWinColor := $00B9FFFF;    
    InitODE;
  end;
end;

procedure ToxBoxStackFrm.Button8Click(Sender: TObject);
begin
  MaxObjs;
  DCar := TGLoxDynCar( oxODE.AddNewChild( TGLoxDynCar ) );
  RandomPositionAndRotation( DCar );
  //DCar.OnMotor := MotorUpdate;
  with DCar do
  begin
    Manager := oxODE;
    Position.Z := 3;
    LENGTH := 2.5;
    WIDTH := 1.5;
    HEIGHT := 1;
    RADIUS := 0.3;
    Modes := [ mdBounce, mdSoftCFM ];
    Mu := 5;
    Mu2 := 0;
    Soft_cfm := 0.001;
    RollFriction := True;
    FrictionForce := 0.035;
    Bounce := 0.1;
    Bounce_Vel := 0.1;
    ContactNum := 12;    
    //oxRollFriction := True;
    //oxFrictionForce := 0.035;
    FRONTSUSPENSION_ERP := 0.4;
    FRONTSUSPENSION_CFM := 0.2;
    BACKSUSPENSION_ERP := 0.4;
    BACKSUSPENSION_CFM := 0.2;
    //oxDisableThreshold := StrToFloat( Edit4.Text );
    //oxDisableSteps := StrToFloat( Edit3.Text );

    //oxAutoDisable := True;
    InitODE;
    Frame.EdgeColor.AsWinColor := $000080FF;
    Frame.VisibleAtRunTime:= True;
    WheelLF.EdgeColor.AsWinColor := $000080FF;
    WheelLF.VisibleAtRunTime:= True;
    WheelLB.EdgeColor.AsWinColor := $000080FF;
    WheelLB.VisibleAtRunTime:= True;
    WheelRF.EdgeColor.AsWinColor := $000080FF;
    WheelRF.VisibleAtRunTime:= True;
    WheelRB.EdgeColor.AsWinColor := $000080FF;
    WheelRB.VisibleAtRunTime:= True;
  end;
end;

procedure ToxBoxStackFrm.Button7Click(Sender: TObject);
begin
  MaxObjs;
  SMesh := TGLOXStaMesh( Scene.AddNewChild( TGLOXStaMesh ) );
  with SMesh do
  begin
    Manager := oxODE;
    MeshObjects.Add( ConeMesh );
    Material.FaceCulling := fcNoCull;
    Material.BlendingMode := bmTransparency;
    Material.FrontProperties.Shininess := 8;
    Material.FrontProperties.Diffuse.Color := RandomColor;
    Material.FrontProperties.Diffuse.Alpha := 0.6;
    Material.FrontProperties.Specular.AsWinColor := $00B9FFFF;
    InitODE;
  end;
  Button7.Enabled := False;
end;

procedure ToxBoxStackFrm.Button11Click(Sender: TObject);
var
  i,RanSize: integer;
begin
  MaxObjs;
  DRagdoll := TGLOXRagdoll( oxODE.AddNewChild( TGLOXRagdoll ) );
  RandomPositionAndRotation( DRagdoll );
  with DRagdoll do
  begin
    Manager := oxODE;
    RanSize:= random(10);
    if RanSize<=1 then
      RanSize:=1;
    Modes := [ mdBounce, mdSoftCFM ];
    Mu := 5;
    Mu2 := 0;
    Soft_cfm := 0.001;
    RollFriction := True;
    FrictionForce := 0.035;
    Bounce := 0.1;
    Bounce_Vel := 0.1;
    ContactNum := 12;        
    Mass:= 0.1*RanSize;
    Size:= RanSize;
    AutoDisable := True;
    FrictionForce := 0.3;
    RollFriction := True;
    DisableThreshold := 0.8;
    DisableSteps := StrToFloat( Edit3.Text );
    InitODE;
    for i := 0 to 20-1 do
      BoneBox[i].VisibleAtRunTime := true;
  end;
end;

procedure ToxBoxStackFrm.Button12Click(Sender: TObject);
begin
  MaxObjs;
  DAMotor := TGLOXAMotor( oxODE.AddNewChild( TGLOXAMotor ) );
  RandomPositionAndRotation( DAMotor );
  with DAMotor do
  begin
    VisibleAtRunTime := True;
    Manager := oxODE;
    Modes := [ mdSoftCFM ];
    Mu := 5;
    Soft_cfm := 0.001;
    Mass := 1;
    ContactNum := 4;
    FrictionForce := 0.035;
    RollFriction := True;
    AutoDisable := True;
    DisableThreshold := StrToFloat( Edit4.Text );
    DisableSteps := StrToFloat( Edit3.Text );
    InitODE;
  end;
end;

procedure ToxBoxStackFrm.RadioGroup_SolverClick(Sender: TObject);
begin
  case RadioGroup_Solver.ItemIndex of
    0 : oxODE.WorldModes := [ mdQuickStep ];
    1 : oxODE.WorldModes := [ mdStepFast1 ];
    2 : oxODE.WorldModes := [ mdNormalStep ];
  end;
end;

procedure ToxBoxStackFrm.CheckBox_DisableStillBodiesClick(Sender: TObject);
begin
  if CheckBox_DisableStillBodies.Checked then
  begin
    dWorldSetAutoDisableFlag( oxODE.World, 1 );
  end else
  begin
    dWorldSetAutoDisableFlag( oxODE.World, 0 );
  end;
end;

procedure ToxBoxStackFrm.Button5Click(Sender: TObject);
begin
  oxODE.DeleteChildren;
  SMesh.Free;
  SMesh := nil;
  Button7.Enabled := True;
end;

procedure ToxBoxStackFrm.Edit1Change(Sender: TObject);
begin
  if StrToInt( Edit1.Text ) <= 0 then
    Edit1.Text := '1';
  oxODE.QuickStepNum := StrToInt( Edit1.Text );
end;

procedure ToxBoxStackFrm.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if not (key in [#8, '0'..'9']) then
  begin
    ShowMessage('Invalid Integer');
    key := #0;
  end;
end;

procedure ToxBoxStackFrm.Edit2KeyPress(Sender: TObject; var Key: Char);
begin
  if not (key in [#8, '0'..'9']) then
  begin
    ShowMessage('Invalid Integer');
    key := #0;
  end;
end;

procedure ToxBoxStackFrm.Edit4Change(Sender: TObject);
var
  i : integer;
begin
  CheckBox_DisableStillBodies.Checked := True;
  for i := 0 to oxODE.Count -1 do
  begin
    if ( oxODE.Children[i] is TGLOXDynBall ) then
      TGLOXDynBall( oxODE.Children[i] ).DisableThreshold := StrToFloat( Edit4.Text );
    if ( oxODE.Children[i] is TGLOXDynBox ) then
      TGLOXDynBox( oxODE.Children[i] ).DisableThreshold := StrToFloat( Edit4.Text );
    if ( oxODE.Children[i] is TGLOXDynCylinder ) then
      TGLOXDynCylinder( oxODE.Children[i] ).DisableThreshold := StrToFloat( Edit4.Text );
    if ( oxODE.Children[i] is TGLOXDynCCylinder ) then
      TGLOXDynCCylinder( oxODE.Children[i] ).DisableThreshold := StrToFloat( Edit4.Text );
    if ( oxODE.Children[i] is TGLOXDynCone ) then
      TGLOXDynCone( oxODE.Children[i] ).DisableThreshold := StrToFloat( Edit4.Text );
    if ( oxODE.Children[i] is TGLOXDynMesh ) then
      TGLOXDynMesh( oxODE.Children[i] ).DisableThreshold := StrToFloat( Edit4.Text );
    if ( oxODE.Children[i] is TGLOXRagdoll ) then
      TGLOXRagdoll( oxODE.Children[i] ).DisableThreshold := StrToFloat( Edit4.Text );
  end;
end;

procedure ToxBoxStackFrm.Edit3Change(Sender: TObject);
var
  i : integer;
begin
  CheckBox_DisableStillBodies.Checked := True;
  for i := 0 to oxODE.Count -1 do
  begin
    if ( oxODE.Children[i] is TGLOXDynBall ) then
      TGLOXDynBall( oxODE.Children[i] ).DisableSteps := StrToFloat( Edit3.Text );
    if ( oxODE.Children[i] is TGLOXDynBox ) then
      TGLOXDynBox( oxODE.Children[i] ).DisableSteps := StrToFloat( Edit3.Text );
    if ( oxODE.Children[i] is TGLOXDynCylinder ) then
      TGLOXDynCylinder( oxODE.Children[i] ).DisableSteps := StrToFloat( Edit3.Text );
    if ( oxODE.Children[i] is TGLOXDynCCylinder ) then
      TGLOXDynCCylinder( oxODE.Children[i] ).DisableSteps := StrToFloat( Edit3.Text );
    if ( oxODE.Children[i] is TGLOXDynCone ) then
      TGLOXDynCone( oxODE.Children[i] ).DisableSteps := StrToFloat( Edit3.Text );
    if ( oxODE.Children[i] is TGLOXDynMesh ) then
      TGLOXDynMesh( oxODE.Children[i] ).DisableSteps := StrToFloat( Edit3.Text );
    if ( oxODE.Children[i] is TGLOXRagdoll ) then
      TGLOXRagdoll( oxODE.Children[i] ).DisableSteps := StrToFloat( Edit3.Text );
  end;
end;

procedure ToxBoxStackFrm.oxODEStepRender(delta: Single);
var
  i,g : integer;
begin
  for i := 0 to oxODE.Count -1 do
  begin
    if CheckBox_DisableStillBodies.Checked then
    begin
      if ( oxODE.Children[i] is TGLOXRagdoll ) then
      begin
        for g:= 0 to NUM-1 do
          IsEnabled := ( dBodyIsEnabled( TGLOXRagdoll( oxODE.Children[i] ).Body[g] ) = 1 );
      end;
      if ( oxODE.Children[i] is TGLOXDynBall ) then
        IsEnabled := ( dBodyIsEnabled( TGLOXDynBall( oxODE.Children[i] ).Body ) = 1 );
      if ( oxODE.Children[i] is TGLOXDynBox ) then
        IsEnabled := ( dBodyIsEnabled( TGLOXDynBox( oxODE.Children[i] ).Body ) = 1 );
      if ( oxODE.Children[i] is TGLOXDynCylinder ) then
        IsEnabled := ( dBodyIsEnabled( TGLOXDynCylinder( oxODE.Children[i] ).Body ) = 1 );
      if ( oxODE.Children[i] is TGLOXDynCCylinder ) then
      begin
        with TGLOXDynCCylinder( oxODE.Children[i] ) do
        begin
          IsEnabled := ( dBodyIsEnabled( Body ) = 1 );
          if Cap1.Material <> Material then
            Cap1.Material := Material;
          if Cap2.Material <> Material then
            Cap2.Material := Material;
        end;
      end;
      if ( oxODE.Children[i] is TGLOXDynCone ) then
        IsEnabled := ( dBodyIsEnabled( TGLOXDynCone( oxODE.Children[i] ).Body ) = 1 );
      if ( oxODE.Children[i] is TGLOXDynMesh ) then
        IsEnabled := ( dBodyIsEnabled( TGLOXDynMesh( oxODE.Children[i] ).Body ) = 1 );
      if IsEnabled and CheckBox_DisableStillBodies.Checked then
        TGLCustomSceneObject( oxODE.Children[i] ).Material.FrontProperties.Emission.Red := 1
      else
        TGLCustomSceneObject( oxODE.Children[i] ).Material.FrontProperties.Emission.Red := 0;
      end else
      begin
        TGLCustomSceneObject( oxODE.Children[i] ).Material.FrontProperties.Emission.Red := 0;
    end;
    if ( TGLCustomSceneObject( oxODE.Children[i] ) is TGLoxDynCar ) then
    begin
      if TGLoxDynCar( oxODE.Children[i] ).Frame <> nil then
        if ( TGLoxDynCar( oxODE.Children[i] ).Frame.Position.Z < -0.25 ) then
        begin
          TGLoxDynCar( oxODE.Children[i] ).Free;
          exit;
        end;
    end;
    if ( TGLCustomSceneObject( oxODE.Children[i] ) is TGLOXRagdoll ) then
    begin
      if TGLOXRagdoll( oxODE.Children[i] ).BoneBox[18] <> nil then
        if ( TGLOXRagdoll( oxODE.Children[i] ).BoneBox[18].Position.Z < -20 ) then
        begin
          TGLOXRagdoll( oxODE.Children[i] ).Free;
          exit;
        end;
    end;
    if ( TGLCustomSceneObject( oxODE.Children[i] ).Position.Z < -0.25 ) then
    begin
      TGLCustomSceneObject( oxODE.Children[i] ).Free;
      exit;
    end;
  end;
end;

procedure ToxBoxStackFrm.oxODECollisionEvent(Geom1, Geom2: PdxGeom);
begin
  //
end;

procedure ToxBoxStackFrm.oxODEMultiStepRender(delta: Single);
begin
//
end;

procedure ToxBoxStackFrm.Edit5Change(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to oxODE.Count -1 do
  begin
    if ( oxODE.Children[i] is TGLOXDynBall ) then
      TGLOXDynBall( oxODE.Children[i] ).FrictionForce := StrToFloat( Edit5.Text );
    if ( oxODE.Children[i] is TGLOXDynCylinder ) then
      TGLOXDynCylinder( oxODE.Children[i] ).FrictionForce := StrToFloat( Edit5.Text );
    if ( oxODE.Children[i] is TGLOXDynCCylinder ) then
      TGLOXDynCCylinder( oxODE.Children[i] ).FrictionForce := StrToFloat( Edit5.Text );
    if ( oxODE.Children[i] is TGLOXRagdoll ) then
      TGLOXRagdoll( oxODE.Children[i] ).FrictionForce := StrToFloat( Edit5.Text );
  end;
end;

procedure ToxBoxStackFrm.Timer1Timer(Sender: TObject);
begin
  Caption := Format( 'oxGLODE v0.8a %.2f FPS ObjCount: ' + IntToStr( oxODE.Count )+' Component Demo '+'by Dave Gravel',
  [GLSceneViewer1.FramesPerSecond] );
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure ToxBoxStackFrm.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure ToxBoxStackFrm.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
begin
  mx := x;
  my := y;
end;

procedure ToxBoxStackFrm.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
 X, Y: Integer);
begin
  if ssright in shift then
    GLCam.MoveAroundTarget( my-y, mx-x );
  mx := x;
  my := y;
end;

procedure ToxBoxStackFrm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
 MousePos: TPoint; var Handled: Boolean);
begin
  GLCam.AdjustDistanceToTarget( Power( 1.1, WheelDelta / 120 ) );
end;

procedure ToxBoxStackFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled := False;
  GLSceneViewer1.Camera := nil;
  GLCadencer1.Enabled := False;
  dGeomDestroy( FloorBox );
  oxODE.DeleteChildren;
end;

initialization
  {$i oxmain.lrs}

end.

