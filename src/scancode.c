
struct scancode {
  const char *keyname;
  SDLKey value;
};

static struct scancode keymap[] = {
  { "backspace", SDLK_BACKSPACE },
  { "tab", SDLK_TAB },
  { "clear", SDLK_CLEAR },
  { "return", SDLK_RETURN },
  { "pause", SDLK_PAUSE },
  { "esc", SDLK_ESCAPE },
  { "space", SDLK_SPACE },
  { "!", SDLK_EXCLAIM },
  { "\"", SDLK_QUOTEDBL },
  { "#", SDLK_HASH },
  { "$", SDLK_DOLLAR },
  { "&", SDLK_AMPERSAND },
  { "'", SDLK_QUOTE },
  { "(", SDLK_LEFTPAREN },
  { ")", SDLK_RIGHTPAREN },
  { "*", SDLK_ASTERISK },
  { "+", SDLK_PLUS },
  { "-", SDLK_MINUS },
  { ",", SDLK_COMMA },
  { ".", SDLK_PERIOD },
  { "/", SDLK_SLASH },
  { "0", SDLK_0 },
  { "1", SDLK_1 },
  { "2", SDLK_2 },
  { "3", SDLK_3 },
  { "4", SDLK_4 },
  { "5", SDLK_5 },
  { "6", SDLK_6 },
  { "7", SDLK_7 },
  { "8", SDLK_8 },
  { "9", SDLK_9 },
  { ":", SDLK_COLON },
  { ";", SDLK_SEMICOLON },
  { "<", SDLK_LESS },
  { "=", SDLK_EQUALS },
  { ">", SDLK_GREATER },
  { "?", SDLK_QUESTION },
  { "@", SDLK_AT },
  { "[", SDLK_LEFTBRACKET },
  { "\\", SDLK_BACKSLASH },
  { "]", SDLK_RIGHTBRACKET },
  { "^", SDLK_CARET },
  { "_", SDLK_UNDERSCORE },
  { "`", SDLK_BACKQUOTE },
  { "a", SDLK_a },
  { "b", SDLK_b },
  { "c", SDLK_c },
  { "d", SDLK_d },
  { "e", SDLK_e },
  { "f", SDLK_f },
  { "g", SDLK_g },
  { "h", SDLK_h },
  { "i", SDLK_i },
  { "j", SDLK_j },
  { "k", SDLK_k },
  { "l", SDLK_l },
  { "m", SDLK_m },
  { "n", SDLK_n },
  { "o", SDLK_o },
  { "p", SDLK_p },
  { "q", SDLK_q },
  { "r", SDLK_r },
  { "s", SDLK_s },
  { "t", SDLK_t },
  { "u", SDLK_u },
  { "v", SDLK_v },
  { "w", SDLK_w },
  { "x", SDLK_x },
  { "y", SDLK_y },
  { "z", SDLK_z },
  { "delete", SDLK_DELETE },
  { "num0", SDLK_KP0 },
  { "num1", SDLK_KP1 },
  { "num2", SDLK_KP2 },
  { "num3", SDLK_KP3 },
  { "num4", SDLK_KP4 },
  { "num5", SDLK_KP5 },
  { "num6", SDLK_KP6 },
  { "num7", SDLK_KP7 },
  { "num8", SDLK_KP8 },
  { "num9", SDLK_KP9 },
  { "num.", SDLK_KP_PERIOD },
  { "num/", SDLK_KP_DIVIDE },
  { "num*", SDLK_KP_MULTIPLY },
  { "num-", SDLK_KP_MINUS },
  { "num+", SDLK_KP_PLUS },
  { "numret", SDLK_KP_ENTER },
  { "num=", SDLK_KP_EQUALS },
  { "up", SDLK_UP },
  { "down", SDLK_DOWN },
  { "left", SDLK_LEFT },
  { "right", SDLK_RIGHT },
  { "ins", SDLK_INSERT },
  { "home", SDLK_HOME },
  { "end", SDLK_END },
  { "pgup", SDLK_PAGEUP },
  { "pgdown", SDLK_PAGEDOWN },
  { "f1", SDLK_F1 },
  { "f2", SDLK_F2 },
  { "f3", SDLK_F3 },
  { "f4", SDLK_F4 },
  { "f5", SDLK_F5 },
  { "f6", SDLK_F6 },
  { "f7", SDLK_F7 },
  { "f8", SDLK_F8 },
  { "f9", SDLK_F9 },
  { "f10", SDLK_F10 },
  { "f11", SDLK_F11 },
  { "f12", SDLK_F12 },
  { "f13", SDLK_F13 },
  { "f14", SDLK_F14 },
  { "f15", SDLK_F15 },
  { "numlock", SDLK_NUMLOCK },
  { "caps", SDLK_CAPSLOCK },
  { "scroll", SDLK_SCROLLOCK },
  { "lshift", SDLK_LSHIFT },
  { "rshift", SDLK_RSHIFT },
  { "lctrl", SDLK_LCTRL },
  { "rctrl", SDLK_RCTRL },
  { "lalt", SDLK_LALT },
  { "ralt", SDLK_RALT },
  { "lmeta", SDLK_LMETA },
  { "rmeta", SDLK_RMETA },
  { "lsuper", SDLK_LSUPER },
  { "rsuper", SDLK_RSUPER },
  { "mode", SDLK_MODE },
  { "help", SDLK_HELP },
  { "print", SDLK_PRINT },
  { "sysrq", SDLK_SYSREQ },
  { "break", SDLK_BREAK },
  { "menu", SDLK_MENU },
  { "power", SDLK_POWER },
  { "euro", SDLK_EURO },  

  { "mouse1", SDLK_LAST + SDL_BUTTON_LEFT },
  { "mouse2", SDLK_LAST + SDL_BUTTON_RIGHT },
  { "mouse3", SDLK_LAST + SDL_BUTTON_MIDDLE },
  { "mwheelup", SDLK_LAST + SDL_BUTTON_WHEELUP },
  { "mwheeldown", SDLK_LAST + SDL_BUTTON_WHEELDOWN },
  { "", 0 }
};
