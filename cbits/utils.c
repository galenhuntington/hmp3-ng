
#include "utils.h"

/*
 * A non-macro version of getyx(3), to make writing a Haskell binding
 * easier.  Called in Yi/Curses.hsc
 */
void nomacro_getyx(WINDOW *win, int *y, int *x) {
    getyx(win, *y, *x);
}

/* A non-macro version of COLOR_PAIR(3)
 */
int get_color_pair (int pair) {
    return COLOR_PAIR (pair);
}
