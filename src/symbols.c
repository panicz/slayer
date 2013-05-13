#include "symbols.h"

SCM noop;

SCM s_u8;
SCM s_u16;
SCM s_u32;
SCM s_u64;

SCM s_quit;
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

SCM s_lshift;
SCM s_rshift;
SCM s_shift;
SCM s_lctrl;
SCM s_rctrl;
SCM s_ctrl;
SCM s_lalt;
SCM s_ralt;
SCM s_alt;
SCM s_lmeta;
SCM s_rmeta;
SCM s_meta;
SCM s_num;
SCM s_caps;
SCM s_mode;

void
symbols_init() {
  noop = gc_protected(eval("noop"));

#define INIT_SYMBOL(name)			\
  s_##name = gc_protected(symbol(# name));

  INIT_SYMBOL(u8);
  INIT_SYMBOL(u16);
  INIT_SYMBOL(u32);
  INIT_SYMBOL(u64);

  INIT_SYMBOL(quit);
  INIT_SYMBOL(typing);
  INIT_SYMBOL(direct);
  INIT_SYMBOL(unknown);
  INIT_SYMBOL(mouse);
  INIT_SYMBOL(input);
  INIT_SYMBOL(active);
  INIT_SYMBOL(gain);
  INIT_SYMBOL(loss);
  INIT_SYMBOL(pressed);
  INIT_SYMBOL(released);
  INIT_SYMBOL(keyboard);
  INIT_SYMBOL(mousemotion);
  INIT_SYMBOL(mousebutton);
  INIT_SYMBOL(left);
  INIT_SYMBOL(right);
  INIT_SYMBOL(middle);

  INIT_SYMBOL(lshift);
  INIT_SYMBOL(rshift);
  INIT_SYMBOL(shift);
  INIT_SYMBOL(lctrl);
  INIT_SYMBOL(rctrl);
  INIT_SYMBOL(ctrl);
  INIT_SYMBOL(lalt);
  INIT_SYMBOL(ralt);
  INIT_SYMBOL(alt);
  INIT_SYMBOL(lmeta);
  INIT_SYMBOL(rmeta);
  INIT_SYMBOL(meta);
  INIT_SYMBOL(num);
  INIT_SYMBOL(caps);
  INIT_SYMBOL(mode);

#undef INIT_SYMBOL
}
