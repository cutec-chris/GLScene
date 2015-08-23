unit fFriction;

{$MODE Delphi}

{*************************************************************************
 *                                                                       *
 * Open Dynamics Engine, Copyright (C) 2001,2002 Russell L. Smith.       *
 * All rights reserved.  Email: russ@q12.org   Web: www.q12.org          *
 *                                                                       *
 * This library is free software; you can redistribute it and/or         *
 * modify it under the terms of EITHER:                                  *
 *   (1) The GNU Lesser General Public License as published by the Free  *
 *       Software Foundation; either version 2.1 of the License, or (at  *
 *       your option) any later version. The text of the GNU Lesser      *
 *       General Public License is included with this library in the     *
 *       file LICENSE.TXT.                                               *
 *   (2) The BSD-style license that is included with this library in     *
 *       the file LICENSE-BSD.TXT.                                       *
 *                                                                       *
 * This library is distributed in the hope that it will be useful,       *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the files    *
 * LICENSE.TXT and LICENSE-BSD.TXT for more details.                     *
 *                                                                       *
 *************************************************************************}

{

test the Coulomb friction approximation.

a 10x10 array of boxes is made, each of which rests on the ground.
a horizantal force is applied to each box to try and get it to slide.
box[i][j] has a mass (i+1)*MASS and a force (j+1)*FORCE. by the Coloumb
friction model, the box should only slide if the force is greater than MU
times the contact normal force, i.e.

  f > MU * body_mass * GRAVITY
  (j+1)*FORCE > MU * (i+1)*MASS * GRAVITY
  (j+1) > (i+1) * (MU*MASS*GRAVITY/FORCE)
  (j+1) > (i+1) * k

this should be independent of the number of contact points, as N contact
points will each have 1/N'th the normal force but the pushing force will
have to overcome N contacts. the constants are chosen so that k=1.
thus you should see a triangle made of half the bodies in the array start to
slide.


  ********************************************************
  This ODE example was converted to Delphi
    by Mattias Fagerlund ( mattias@cambrianlabs.com)

}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLObjects, GLCadencer, GLTexture, GLzBuffer,
  GLShadowPlane, LResources, gllclviewer, GLScene, ODEGL, ODEImport,
  GLViewer, GLRenderContextInfo;

const
  cLENGTH = 0.2;	// box length & width
  cHEIGHT = 0.05;	// box height
  MASS = 0.2;	// mass of box[i][j] = (i+1) * MASS
  FORCE = 0.05;	// force applied to box[i][j] = (j+1) * FORCE
  MU = 0.5;		// the global mu to use
  GRAVITY = 0.5;	// the global gravity to use
  N1 = 10;		// number of different forces to try
  N2 = 10;		// number of different masses to try

type

  { TfrmFriction }

  TfrmFriction = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLScene1: TGLScene;
    DirectOpenGL1: TGLDirectOpenGL;
    DummyCube1: TGLDummyCube;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    ZShadows1: TGLZShadows;
    GLShadowPlane1: TGLShadowPlane;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure DirectOpenGL1Render(var rci: TRenderContextInfo);
  private
    { Private declarations }
  public
    { Public declarations }

    // dynamics and collision objects
    world : PdxWorld;
    space : PdxSpace;
    body : array[0..N1-1, 0..N2-1] of PdxBody;
    contactgroup : TdJointGroupID;
    ground : PdxGeom;
    box : array[0..N1-1, 0..N2-1] of PdxGeom;
  end;

var
  frmFriction: TfrmFriction;

implementation


procedure TfrmFriction.FormCreate(Sender: TObject);
var
  i, j : integer;
  m : TdMass;
  lh : single;
begin
  // create world
  world := dWorldCreate();
  space := dHashSpaceCreate(nil);
  contactgroup := dJointGroupCreate (0);
  dWorldSetGravity (world,0,0,-GRAVITY);
  ground := dCreatePlane (space,0,0,1,0);

  lh := (N1-1)*2*cLENGTH/2;
  // bodies
  for i := 0 to N1 - 1 do
    for j := 0 to N2 - 1 do
    begin
      body[i, j] := dBodyCreate (world);
      dMassSetBox (m,1,cLENGTH,cLENGTH,cHEIGHT);
      dMassAdjust (m,MASS*(j+1));
      dBodySetMass (body[i,j],@m);

      dBodySetPosition (body[i,j],i*2*cLENGTH - lh,j*2*cLENGTH - lh, cHEIGHT*0.5);

      box[i,j] := dCreateBox (space,cLENGTH,cLENGTH,cHEIGHT);
      dGeomSetBody (box[i,j],body[i,j]);
    end;
end;

procedure TfrmFriction.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  GLCadencer1.Enabled := false;
  
  Application.ProcessMessages;
  
  dJointGroupDestroy (contactgroup);
  dSpaceDestroy (space);
  dWorldDestroy (world);
end;

procedure nearCallback (data : pointer; o1, o2 : PdxGeom); cdecl;
var
  i : integer;
  g1,g2 : boolean;
  b1, b2 : PdxBody;
  contact : array[0..2] of TdContact;

  numc : integer;
  c : TdJointID;
begin
  // only collide things with the ground
  g1 := (o1 = frmFriction.ground);
  g2 := (o2 = frmFriction.ground);

  if (not (g1 or g2)) then
    exit;

  b1 := dGeomGetBody(o1);
  b2 := dGeomGetBody(o2);

  for i := 0 to 2 do
  begin
    contact[i].surface.mode := (ord(dContactSoftCFM) or ord(dContactApprox1));
    contact[i].surface.mu := MU;
    contact[i].surface.soft_cfm := 0.01;
  end;

  numc := dCollide (o1,o2,3,contact[0].geom,sizeof(TdContact));
  if (numc>0) then
    for i := 0 to numc - 1 do
    begin
      c := dJointCreateContact (frmFriction.world,frmFriction.contactgroup,@contact[i]);
      dJointAttach (c,b1,b2);
    end;
end;


procedure TfrmFriction.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);

var
  i,j : integer;
begin
  // apply forces to all bodies
  for i := 0 to N1 - 1 do
    for j := 0 to N2 - 1 do
      dBodyAddForce (body[i,j],FORCE*(i+1),0,0);

  dSpaceCollide (space,nil,nearCallback);
  dWorldStep (world,0.05);

  // remove all contact joints
  dJointGroupEmpty (contactgroup);

  GLSceneViewer1.Invalidate;
end;
procedure TfrmFriction.DirectOpenGL1Render(var rci: TRenderContextInfo);
var
  i, j : integer;
  sides : TdVector3;
begin
  sides[0] := cLENGTH;
  sides[1] := cLENGTH;
  sides[2] := cHEIGHT;

  for i := 0 to N1 - 1 do
    for j := 0 to N2 - 1 do
      dsDrawBox (dGeomGetPosition(box[i,j]), dGeomGetRotation(box[i,j]), sides);
end;

initialization
  {$i fFriction.lrs}

end.
