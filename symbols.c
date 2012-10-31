#include "symbols.h"

SCM s_u8;
SCM s_u16;
SCM s_u32;
SCM s_u64;

SCM s_typing;
SCM s_direct;
SCM s_unknown;
SCM s_mouse;
SCM s_input;
SCM s_active;
SCM s_gain;
SCM s_loss;
SCM s_pressed;
SCM s_released;
SCM s_keyboard;
SCM s_mousemotion;
SCM s_mousebutton;
SCM s_left;
SCM s_right;
SCM s_middle;


void
symbols_init() {
#define INIT_SYMBOL(var, val) \
  var = symbol(val);\
  hold_scm(var);

  INIT_SYMBOL(s_u8, "u8");
  INIT_SYMBOL(s_u16, "u16");
  INIT_SYMBOL(s_u32, "u32");
  INIT_SYMBOL(s_u64, "u64");

  INIT_SYMBOL(s_typing, "typing");
  INIT_SYMBOL(s_direct, "direct");
  INIT_SYMBOL(s_unknown, "unknown");
  INIT_SYMBOL(s_mouse, "mouse");
  INIT_SYMBOL(s_input, "input");
  INIT_SYMBOL(s_active, "active");
  INIT_SYMBOL(s_gain, "gain");
  INIT_SYMBOL(s_loss, "loss");
  INIT_SYMBOL(s_pressed, "pressed");
  INIT_SYMBOL(s_released, "released");
  INIT_SYMBOL(s_keyboard, "keyboard");
  INIT_SYMBOL(s_mousemotion, "mousemotion");
  INIT_SYMBOL(s_mousebutton, "mousebutton");
  INIT_SYMBOL(s_left, "left");
  INIT_SYMBOL(s_right, "right");
  INIT_SYMBOL(s_middle, "middle");


#undef INIT_SYMBOL

}
