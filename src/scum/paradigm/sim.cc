#include <ode/ode.h>

#include <vector>
#include <list>
#include <map>
#include <string>
#include <cmath>
#include <cassert>
#include "utils.h"

using namespace std;
//#include "extend.h"

/*
  Implementacja symulatora wymaga podjęcia męskich decyzji:
  1. używami tylko tri-meshów (może potem to zmienimy, ale na razie 
  chodzi przede wszystkim o prostatę czy coś)

  Być może trzeba będzie nieco rozszerzyć aktualną implementację
  następująco:
  - albo stworzyć wiele światów i przenosić je miedzy sobą
  - albo stworzyć jeden świat, w którym znajduje się wiele przestrzeni
  i przenosić ciała z jednej przestrzeni do innej (i to jest chyba
  preferowane rozwiązanie)

 */

#define MAX_CONTACTS 12

typedef struct {
  map<string, int> id;
  vector<dBodyID> bodies;
  vector<dGeomID> geoms;
  vector<dJointID> joints;
  dSpaceID space;
} rig_t;

typedef struct {
  dWorldID world;
  dSpaceID space;
  list<rig_t *> rigs;
  dJointGroupID contact_group;
  list<dJointID> contacts;
  dReal dt;
} sim_t;

static void 
on_potential_collision(void *s, dGeomID a, dGeomID b) {
  sim_t *sim = (sim_t *) s;
  dContact c[MAX_CONTACTS];
  int i, n = dCollide(a, b, MAX_CONTACTS, &c[0].geom, sizeof(dContact));
  for(i = 0; i < n; ++i) {
    dJointID r = dJointCreateContact(sim->world, sim->contact_group, &c[i]);
    dJointAttach(r, dGeomGetBody(c[i].geom.g1), dGeomGetBody(c[i].geom.g2));
    sim->contacts.push_back(r);
  }
}


static void
init_simulation(sim_t *sim) {
  rig_t *rig;

  dBodyID b;
  dGeomID g;
  dJointID j;
  dMass m;
  dQuaternion q;
  const dReal *pos;

  dWorldSetGravity(sim->world, 0, 0, -9.8);
  // SET UP BODIES
  
   // 0: ground
  rig = new rig_t;
  rig->space = dSimpleSpaceCreate(sim->space);
  dBodyID ground = NULL; //dBodyCreate(sim->world);
  b = ground; 
  g = dCreatePlane(rig->space, 0, 0, 1, 0);
  
  //dBodySetKinematic(b);
  //dGeomSetBody(g, b);
  rig->id["ground"] = rig->bodies.size();
  rig->bodies.push_back(b);
  rig->geoms.push_back(g);
  sim->rigs.push_back(rig);

  // 1: car
  rig = new rig_t;
  rig->space = dSimpleSpaceCreate(sim->space);

  // 1: chassis
  dBodyID chassis = dBodyCreate(sim->world);
  b = chassis;
  g = dCreateBox(0, 0.7, 0.5, 0.2);

  dBodySetPosition(b, 0, 0, 0.5);
  dMassSetBox(&m, 1, 0.7, 0.5, 0.2);
  dMassAdjust(&m, 1);
  dBodySetMass(b, &m);
  
  dGeomSetBody(g, b);
  rig->id["chassis"] = rig->bodies.size();
  rig->bodies.push_back(b);
  rig->geoms.push_back(g);

  // 2: front wheel
  dBodyID front_wheel = dBodyCreate(sim->world);
  b = front_wheel;
  dBodySetPosition (b, 0.5*0.7, 0, 0.5-0.2*0.5);
  dQFromAxisAndAngle(q, 1, 0, 0, M_PI*0.5);
  dBodySetQuaternion(b, q);
  dMassSetCylinder(&m, 1, 3, 0.18, 0.02);
  dMassAdjust(&m, 0.2);
  dBodySetMass(b, &m);
  g = dCreateCylinder(0, 0.18, 0.02);

  dGeomSetBody(g, b);
  rig->id["front-wheel"] = rig->bodies.size();
  rig->bodies.push_back(b);
  rig->geoms.push_back(g);

  // 3: right rear wheel
  dBodyID right_rear_wheel = dBodyCreate(sim->world);
  b = right_rear_wheel;
  dBodySetPosition (b, -0.5*0.7, 0.5/2, 0.5-0.2*0.5);
  dQFromAxisAndAngle(q, 1, 0, 0, M_PI*0.5);
  dBodySetQuaternion(b, q);
  dMassSetCylinder(&m, 1, 3, 0.18, 0.02);
  dMassAdjust(&m, 0.2);
  dBodySetMass(b, &m);
  g = dCreateCylinder(0, 0.18, 0.02);

  dGeomSetBody(g, b);
  rig->id["right-rear-wheel"] = rig->bodies.size();
  rig->bodies.push_back(b);
  rig->geoms.push_back(g);

  // 4: left rear wheel
  dBodyID left_rear_wheel = dBodyCreate(sim->world);
  b = left_rear_wheel;
  dBodySetPosition (b, -0.5*0.7, -0.5/2, 0.5-0.2*0.5);
  dQFromAxisAndAngle(q, 1, 0, 0, M_PI*0.5);
  dBodySetQuaternion(b, q);
  dMassSetCylinder(&m, 1, 3, 0.18, 0.02);
  dMassAdjust(&m, 0.2);
  dBodySetMass(b, &m);
  g = dCreateCylinder(0, 0.18, 0.02);

  dGeomSetBody(g, b);
  rig->id["left-rear-wheel"] = rig->bodies.size();
  rig->bodies.push_back(b);
  rig->geoms.push_back(g);

  
  sim->rigs.push_back(rig);

  // SET UP (PERMANENT) JOINTS

  
  j = dJointCreateHinge2(sim->world,0);
  dJointAttach(j, chassis, front_wheel);
  pos = dBodyGetPosition(front_wheel);
  dJointSetHinge2Anchor(j, pos[0], pos[1], pos[2]);
  dJointSetHinge2Axis1(j, 0, 0, 1);
  dJointSetHinge2Axis2(j, 0, 1, 0);
  dJointSetHinge2Param (j, dParamSuspensionERP, 0.4);
  dJointSetHinge2Param (j, dParamSuspensionCFM, 0.8);

  j = dJointCreateHinge2(sim->world,0);
  dJointAttach(j, chassis, left_rear_wheel);
  pos = dBodyGetPosition(left_rear_wheel);
  dJointSetHinge2Anchor(j, pos[0], pos[1], pos[2]);
  dJointSetHinge2Axis1(j, 0, 0, 1);
  dJointSetHinge2Axis2(j, 0, 1, 0);
  dJointSetHinge2Param (j, dParamSuspensionERP,0.4);
  dJointSetHinge2Param (j, dParamSuspensionCFM,0.8);
  dJointSetHinge2Param (j, dParamLoStop, 0);
  dJointSetHinge2Param (j, dParamHiStop, 0);

  j = dJointCreateHinge2(sim->world,0);
  dJointAttach(j, chassis, right_rear_wheel);
  pos = dBodyGetPosition(right_rear_wheel);
  dJointSetHinge2Anchor(j, pos[0], pos[1], pos[2]);
  dJointSetHinge2Axis1(j, 0, 0, 1);
  dJointSetHinge2Axis2(j, 0, 1, 0);
  dJointSetHinge2Param (j, dParamSuspensionERP,0.4);
  dJointSetHinge2Param (j, dParamSuspensionCFM,0.8);
  dJointSetHinge2Param (j, dParamLoStop, 0);
  dJointSetHinge2Param (j, dParamHiStop, 0);
  
}

static sim_t *
make_simulation() {
  sim_t *sim = new sim_t;
  sim->world = dWorldCreate();
  sim->space = dHashSpaceCreate(0);
  sim->contact_group = dJointGroupCreate(0);
  sim->dt = 0.05;
  init_simulation(sim);
  return sim;
}

static void
simulation_step(sim_t *sim) {
  dJointGroupEmpty(sim->contact_group);
  dSpaceCollide(sim->space, sim, &on_potential_collision);
  dWorldStep(sim->world, sim->dt);
}

extern "C" int 
main(int argc, char *argv[]) {
  dInitODE();
  dAllocateODEDataForThread(dAllocateMaskAll);
  sim_t *sim = make_simulation();
  while(1) {
    simulation_step(sim);
  }
  return 0;
}



/*
    (let* ((simulation (load-simulation ...))
           (controllers (simulation-controllers simulation)))
      (while #t
        (simulation-step! simulation)
	(for-each handle-collision! (collisions simulation))
	(for-each steer controllers)))

    To może inaczej. Jak bedzie wyglądała implementacja serwera?
    1. otwórz gniazdo i nasłuchuj
    2. zainicjalizuj symulację
    / gdy klien się połączy, przydziel mu nowy obiekt z symulacji.
    obiekt symulacji skojarzony jest z dwoma typami obiektów:
    (1) kontrolerami oraz (2) obserwatorami
    kontroler składa się z procedury pobierającej wektor sterowania
    oraz opcjonalnie z opisu poszczególnych wejść
    kontroler zaś jest procedurą zero-argumentową

 */
