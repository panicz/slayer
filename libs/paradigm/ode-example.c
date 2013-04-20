#include <ode/ode.h>
#include "utils.h"
#include "extend.h"

// some constants

const float LENGTH = 0.7;	// chassis length
const float WIDTH = 0.5;	// chassis width
const float HEIGHT = 0.2;	// chassis height
const float RADIUS = 0.18;	// wheel radius
const float STARTZ = 0.5;	// starting height of chassis
const float CMASS = 1;		// chassis mass
const float WMASS = 0.2;	// wheel mass

int
main() {
  dInitODE();
  dAllocateODEDataForThread(dAllocateMaskAll);
  dWorldID world = dWorldCreate();
  dSpaceID space = dHashSpaceCreate(0);

  dJointGroupID contactgroup = dJointGroupCreate(0);
  dGeomID ground = dCreatePlane(space, 0, 0, 1, 0);
  dSpaceID car_space;
  dGeomID box[1];
  dGeomID sphere[3];
  dGeomID ground_box = dCreateBox (space,2,1.5,1);
  dMass m;

  dBodyID body[4];
  dJointID joint[3];

  static dReal speed=0,steer=0;	// user commands


void nearCallback (void *data, dGeomID o1, dGeomID o2)
{
  int i,n;

 
  // only collide things with the ground
  int g1 = (o1 == ground || o1 == ground_box);
  int g2 = (o2 == ground || o2 == ground_box);
  if (!(g1 ^ g2)) return;
  
  const int N = 10;
  dContact contact[N];
  n = dCollide (o1,o2,N,&contact[0].geom,sizeof(dContact));
  for (i=0; i<n; i++) {
    contact[i].surface.mode = dContactSlip1 | dContactSlip2 |
      dContactSoftERP | dContactSoftCFM | dContactApprox1;
    contact[i].surface.mu = dInfinity;
    contact[i].surface.slip1 = 0.1;
    contact[i].surface.slip2 = 0.1;
    contact[i].surface.soft_erp = 0.5;
    contact[i].surface.soft_cfm = 0.3;
    dJointID c = dJointCreateContact (world,contactgroup,&contact[i]);
    dJointAttach (c,
		  dGeomGetBody(contact[i].geom.g1),
		  dGeomGetBody(contact[i].geom.g2));
  }  
}

  // chassis body
  body[0] = dBodyCreate (world);
  dBodySetPosition (body[0],0,0,STARTZ);
  dMassSetBox (&m,1,LENGTH,WIDTH,HEIGHT);
  dMassAdjust (&m,CMASS);
  dBodySetMass (body[0],&m);

  box[0] = dCreateBox (0,LENGTH,WIDTH,HEIGHT);
  dGeomSetBody (box[0],body[0]);

  int i;
  // wheel bodies
  for (i=1; i<=3; i++) {
    body[i] = dBodyCreate (world);
    dQuaternion q;
    dQFromAxisAndAngle (q,1,0,0,M_PI*0.5);
    dBodySetQuaternion (body[i],q);
    dMassSetSphere (&m,1,RADIUS);
    dMassAdjust (&m,WMASS);
    dBodySetMass (body[i],&m);
    sphere[i-1] = dCreateSphere (0,RADIUS);
    dGeomSetBody (sphere[i-1],body[i]);
  }

  dBodySetPosition (body[1],0.5*LENGTH,0,STARTZ-HEIGHT*0.5);
  dBodySetPosition (body[2],-0.5*LENGTH, WIDTH*0.5,STARTZ-HEIGHT*0.5);
  dBodySetPosition (body[3],-0.5*LENGTH,-WIDTH*0.5,STARTZ-HEIGHT*0.5);

  // front and back wheel hinges
  for (i=0; i<3; i++) {
    joint[i] = dJointCreateHinge2 (world,0);
    dJointAttach (joint[i],body[0],body[i+1]);
    const dReal *a = dBodyGetPosition (body[i+1]);
    dJointSetHinge2Anchor (joint[i],a[0],a[1],a[2]);
    dJointSetHinge2Axis1 (joint[i],0,0,1);
    dJointSetHinge2Axis2 (joint[i],0,1,0);
  }

  // set joint suspension
  for (i=0; i<3; i++) {
    dJointSetHinge2Param (joint[i],dParamSuspensionERP,0.4);
    dJointSetHinge2Param (joint[i],dParamSuspensionCFM,0.8);
  }

  // lock back wheels along the steering axis
  for (i=1; i<3; i++) {
    // set stops to make sure wheels always stay in alignment
    dJointSetHinge2Param (joint[i],dParamLoStop,0);
    dJointSetHinge2Param (joint[i],dParamHiStop,0);
    // the following alternative method is no good as the wheels may get out
    // of alignment:
    //   dJointSetHinge2Param (joint[i],dParamVel,0);
    //   dJointSetHinge2Param (joint[i],dParamFMax,dInfinity);
  }

  // create car space and add it to the top level space
  car_space = dSimpleSpaceCreate (space);
  dSpaceSetCleanup (car_space,0);
  dSpaceAdd (car_space,box[0]);
  dSpaceAdd (car_space,sphere[0]);
  dSpaceAdd (car_space,sphere[1]);
  dSpaceAdd (car_space,sphere[2]);

  // environment
  dMatrix3 R;
  dRFromAxisAndAngle (R,0,1,0,-0.15);
  dGeomSetPosition (ground_box,2,0,-0.34);
  dGeomSetRotation (ground_box,R);


  dWorldSetGravity(world, 0, 0, -9.8);
  //dWorldQuickStepNumIterations(world, 64);

  while(1) {
    // motor
    dJointSetHinge2Param (joint[0],dParamVel2,-speed);
    dJointSetHinge2Param (joint[0],dParamFMax2,0.1);

    // steering
    dReal v = steer - dJointGetHinge2Angle1 (joint[0]);
    if (v > 0.1) v = 0.1;
    if (v < -0.1) v = -0.1;
    v *= 10.0;
    dJointSetHinge2Param (joint[0],dParamVel,v);
    dJointSetHinge2Param (joint[0],dParamFMax,0.2);
    dJointSetHinge2Param (joint[0],dParamLoStop,-0.75);
    dJointSetHinge2Param (joint[0],dParamHiStop,0.75);
    dJointSetHinge2Param (joint[0],dParamFudgeFactor,0.1);

    dSpaceCollide (space,0,&nearCallback);
    dWorldStep (world,0.05);

    // remove all contact joints
    dJointGroupEmpty (contactgroup);
  }
  

  /*
    STRUKTURA SYMULACJI:
    1. konstruujemy swiat -- tworzymy ciala, nadajemy im geometrie 
    i parametry dynamiczne i laczymy je wiezami
    
    2. petla:
    2a: wykonaj krok symulacji
    2b: dokonaj detekcji kolizji
    2c: obsluz kolizje
    2d: zastosuj sterowanie

    Z perspektywy GUILE'a
    (let* ((simulation (load-simulation ...))
           (controllers (simulation-controllers simulation)))
      (while #t
        (simulation-step! simulation)
	(for-each handle-collision! (collisions simulation))
	(for-each steer controllers)))

    czy coś takiego. No ale mniejsza. Wiele wskazuje na to, że oprócz
    kontrolerów trzeba będzie dodać obserwatory

   */
  dGeomDestroy (box[0]);
  dGeomDestroy (sphere[0]);
  dGeomDestroy (sphere[1]);
  dGeomDestroy (sphere[2]);
  dJointGroupDestroy (contactgroup);
  dSpaceDestroy (space);
  dWorldDestroy (world);
  dCloseODE();

}
