#ifndef INPUT_H
#define INPUT_H
#include "extend.h"

/** initialize and finalize input subsystem */
extern void input_init();
extern void input_finish();

/** use the input subsystem to handle events */
extern SCM input_handle_events();


#endif /* INPUT_H */
