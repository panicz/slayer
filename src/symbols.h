#ifndef SYMBOLS_H
#define SYMBOLS_H
#include "extend.h"

extern SCM noop;

extern SCM s_u8;
extern SCM s_u16;
extern SCM s_u32;
extern SCM s_u64;
extern SCM s_f32;
extern SCM s_f64;

extern SCM s_wrong_type_arg;
extern SCM s_system_error;
extern SCM s_signal;
extern SCM s_out_of_range;

extern SCM s_quit;
extern SCM s_typing;
extern SCM s_direct;
extern SCM s_unknown;
extern SCM s_mouse;
extern SCM s_input;
extern SCM s_active;
extern SCM s_gain;
extern SCM s_loss;
extern SCM s_pressed;
extern SCM s_released;
extern SCM s_keyboard;
extern SCM s_mousemotion;
extern SCM s_mousebutton;
extern SCM s_left;
extern SCM s_right;
extern SCM s_middle;

extern SCM s_copy;
extern SCM s_view;
extern SCM s_proxy;

extern SCM s_lshift;
extern SCM s_rshift;
extern SCM s_shift;
extern SCM s_lctrl;
extern SCM s_rctrl;
extern SCM s_ctrl;
extern SCM s_lalt;
extern SCM s_ralt;
extern SCM s_alt;
extern SCM s_lmeta;
extern SCM s_rmeta;
extern SCM s_meta;
extern SCM s_num;
extern SCM s_caps;
extern SCM s_mode;

extern void symbols_init();

#endif /* SYMBOLS_H */
