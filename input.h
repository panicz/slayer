#ifndef INPUT_H
#define INPUT_H
#include "extend.h"

/** initialize input subsystem */
extern void input_init();

/** use the input subsystem to handle events */
extern SCM input_handle_events();


#endif /* INPUT_H */
