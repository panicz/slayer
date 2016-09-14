#include "symbols.h"

SCM noop;

SCM s_wrong_type_arg;
SCM s_system_error;
SCM s_out_of_range;
SCM s_misc_error;

SCM s_signal;

SCM s_u8;
SCM s_u16;
SCM s_u32;
SCM s_u64;
SCM s_f32;
SCM s_f64;

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

SCM s_key_down;
SCM s_key_up;
SCM s_mouse_move;

SCM s_copy;
SCM s_view;
SCM s_proxy;

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

SCM s_miter;
SCM s_round;
SCM s_bevel;
SCM s_butt;
SCM s_round;
SCM s_square;
SCM s_winding;
SCM s_even_odd;
SCM s_clear;
SCM s_source;
SCM s_over;
SCM s_in;
SCM s_out;
SCM s_atop;
SCM s_dest;
SCM s_dest_over;
SCM s_dest_in;
SCM s_dest_out;
SCM s_dest_atop;
SCM s_xor;
SCM s_add;
SCM s_saturate;
SCM s_multiply;
SCM s_screen;
SCM s_overlay;
SCM s_darken;
SCM s_lighten;
SCM s_color_dodge;
SCM s_color_burn;
SCM s_hard_light;
SCM s_soft_light;
SCM s_difference;
SCM s_exclusion;
SCM s_hue;
SCM s_saturation;
SCM s_luminosity;
SCM s_default;
SCM s_none;
SCM s_gray;
SCM s_subpixel;
SCM s_fast;
SCM s_good;
SCM s_best;

SCM s_normal;
SCM s_italic;
SCM s_oblique;
SCM s_bold;

void
symbols_init() {
  noop = gc_protected(eval("noop"));
  
  s_wrong_type_arg = gc_protected(symbol("wrong-type-arg"));
  s_system_error = gc_protected(symbol("system-error"));
  s_out_of_range = gc_protected(symbol("out-of-range"));
  s_misc_error = gc_protected(symbol("misc-error"));

#define INIT_SYMBOL(name)			\
  s_##name = gc_protected(symbol(# name));

  INIT_SYMBOL(signal);
  INIT_SYMBOL(u8);
  INIT_SYMBOL(u16);
  INIT_SYMBOL(u32);
  INIT_SYMBOL(u64);
  INIT_SYMBOL(f32);
  INIT_SYMBOL(f64);

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

  s_key_down = gc_protected(symbol("key-down"));
  s_key_up = gc_protected(symbol("key-up"));
  s_mouse_move = gc_protected(symbol("mouse-move"));
  
  INIT_SYMBOL(copy);
  INIT_SYMBOL(view);
  INIT_SYMBOL(proxy);

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

  INIT_SYMBOL(miter);
  INIT_SYMBOL(round);
  INIT_SYMBOL(bevel);
  INIT_SYMBOL(butt);
  INIT_SYMBOL(round);
  INIT_SYMBOL(square);
  INIT_SYMBOL(winding);
  s_even_odd = gc_protected(symbol("even-odd"));
  INIT_SYMBOL(clear);
  INIT_SYMBOL(source);
  INIT_SYMBOL(over);
  INIT_SYMBOL(in);
  INIT_SYMBOL(out);
  INIT_SYMBOL(atop);
  INIT_SYMBOL(dest);
  s_dest_over = gc_protected(symbol("dest-over"));
  s_dest_in = gc_protected(symbol("dest-in"));
  s_dest_out = gc_protected(symbol("dest-out"));
  s_dest_atop = gc_protected(symbol("dest-atop"));
  INIT_SYMBOL(xor);
  INIT_SYMBOL(add);
  INIT_SYMBOL(saturate);
  INIT_SYMBOL(multiply);
  INIT_SYMBOL(screen);
  INIT_SYMBOL(overlay);
  INIT_SYMBOL(darken);
  INIT_SYMBOL(lighten);
  s_color_dodge = gc_protected(symbol("color-dodge"));
  s_color_burn = gc_protected(symbol("color-burn"));
  s_hard_light = gc_protected(symbol("hard-light"));
  s_soft_light = gc_protected(symbol("soft-light"));;
  INIT_SYMBOL(difference);
  INIT_SYMBOL(exclusion);
  INIT_SYMBOL(hue);
  INIT_SYMBOL(saturation);
  INIT_SYMBOL(luminosity);
  INIT_SYMBOL(default);
  INIT_SYMBOL(none);
  INIT_SYMBOL(gray);
  INIT_SYMBOL(subpixel);
  INIT_SYMBOL(fast);
  INIT_SYMBOL(good);
  INIT_SYMBOL(best);

  INIT_SYMBOL(normal);
  INIT_SYMBOL(italic);
  INIT_SYMBOL(oblique);
  INIT_SYMBOL(bold);
  
#undef INIT_SYMBOL
}
