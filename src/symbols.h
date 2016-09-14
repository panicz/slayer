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

extern SCM s_key_down;
extern SCM s_key_up;
extern SCM s_mouse_move;

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

extern SCM s_miter;
extern SCM s_round;
extern SCM s_bevel;
extern SCM s_butt;
extern SCM s_round;
extern SCM s_square;
extern SCM s_winding;
extern SCM s_even_odd;
extern SCM s_clear;
extern SCM s_source;
extern SCM s_over;
extern SCM s_in;
extern SCM s_out;
extern SCM s_atop;
extern SCM s_dest;
extern SCM s_dest_over;
extern SCM s_dest_in;
extern SCM s_dest_out;
extern SCM s_dest_atop;
extern SCM s_xor;
extern SCM s_add;
extern SCM s_saturate;
extern SCM s_multiply;
extern SCM s_screen;
extern SCM s_overlay;
extern SCM s_darken;
extern SCM s_lighten;
extern SCM s_color_dodge;
extern SCM s_color_burn;
extern SCM s_hard_light;
extern SCM s_soft_light;
extern SCM s_difference;
extern SCM s_exclusion;
extern SCM s_hue;
extern SCM s_saturation;
extern SCM s_luminosity;
extern SCM s_default;
extern SCM s_none;
extern SCM s_gray;
extern SCM s_subpixel;
extern SCM s_fast;
extern SCM s_good;
extern SCM s_best;

extern SCM s_normal;
extern SCM s_italic;
extern SCM s_oblique;
extern SCM s_bold;

extern void symbols_init();

#endif /* SYMBOLS_H */
