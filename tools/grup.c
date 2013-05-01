/*
  gcc grup.c -o grup -lcurses
 */

#include <curses.h>
#include <regex.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>

#define FATAL(msg, ...) do { fprintf(stderr, "FATAL ERROR: " msg "\n", ## __VA_ARGS__ ); exit(0); } while(0)

typedef struct {
  WINDOW *window;
  regex_t regex;
  char *text;
  char *label;
  int x, y, w, h;
} column_t;

static column_t *columns = NULL;
static int ncolumns = 0;
int done = 0;
struct sigaction sigact;
char *progname;
WINDOW *screen;

static void
signal_handler(int sig)
{
  switch (sig) {
  case SIGINT:
    done = 1;
    break;
  default:
    break;
  }
}

void 
init_signals(void)
{
  sigact.sa_handler = signal_handler;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags = 0;
  sigaction(SIGINT, &sigact, (struct sigaction *)NULL);
}

void
finish()
{
  sigemptyset(&sigact.sa_mask);
  free(columns);
  endwin();
}




int
main(int argc, char *argv[])
{
  int read, i, width, height, column_width;
  char *line = NULL;
  size_t length;
  progname = argv[0];

  ncolumns = argc;
  columns = calloc(sizeof (column_t), ncolumns);
  if (!columns) {
    FATAL("unable to allocate memory");
  }

  init_signals();
  atexit(finish);

  screen = initscr(); 
  nocbreak(); 
  echo(); 
  nonl();
  //intrflush(stdscr, FALSE);

  getmaxyx(screen, height, width);
  
  column_width = (width / ncolumns);

  for (i = 0; i < ncolumns; ++i) {
    columns[i].x = i*column_width;
    columns[i].y = 0;
    if (i < ncolumns - 1) {
      columns[i].w = column_width - 1;
    } else {
      columns[i].w = width - columns[i].x;
    }
    columns[i].h = height;
  
    columns[i].window = newwin(columns[i].h, columns[i].w, 
			       columns[i].y, columns[i].x);
    scrollok(columns[i].window, TRUE);
  
    if (i < ncolumns - 1) {
      mvvline(0, (i + 1)*column_width - 1,'|', height);
      refresh();
    }
    wrefresh(columns[i].window);
  }

  for (i = 0; i < ncolumns; ++i) {
    if (i < ncolumns-1) {
      regcomp(&columns[i].regex, argv[i+1], REG_EXTENDED);
      columns[i].label = argv[i+1];
    }
    else {
      regcomp(&columns[i].regex, ".*", REG_EXTENDED);
      columns[i].label = ".*";
    }
  }

  while (!done && (read = getline(&line, &length, stdin))) {
    if (read == -1) {
      continue;
    }
    for (i = 0; i < ncolumns; ++i) {
      if (!regexec(&columns[i].regex, line, 0, NULL, 0)) {
	wprintw(columns[i].window, line);
	wrefresh(columns[i].window);
	break;
      }
    }
    refresh();
    //free(line);
  }
}
