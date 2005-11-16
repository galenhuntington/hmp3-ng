
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

/*
 * Specialised packed hGetLine. The caller should copy out any string it
 * is interested in. Additionally, we drop redundant @F packets arriving --
 * there's too many anyway
 */
#define BUFLEN 1024

int frame_count = 0;    /* we count frame packets, and drop 19/20 of them */

int getline(char *buf, FILE *hdl) { 
    char *p;
    int c;

    /* read first two bytes of packet, to work out if we drop it */
    getc(hdl);
    c = getc(hdl);

    /* drop packet */
    if (c == 'F' && frame_count < 20) {
        frame_count++;

        while (c != '\n') 
            c = getc(hdl);
        return getline(buf,hdl);        /* read another line */

    /* normal packet */
    } else {
        if (c == 'F') frame_count = 0;    /* reset frame count */

        p = fgets(buf+2, BUFLEN-2, hdl);  /* read rest of line */
        if (p == NULL) {
            perror("getline failed\n");
            return (-1);
        }
        buf[0] = '@';                     /* poke back these chars */
        buf[1] = c;   
        return strlen(buf);
    }
}
            
/* given a file descriptor (presumably got from a Haskell Handle) , open
 * a FILE * stream onto that fd. Don't try to use the Handle after this 
 */
FILE *openfd(int fd) {
    FILE *file = NULL;

    if ((file = fdopen(fd, "r")) != NULL) {
         return file;
    } else {
         perror("cbits.openfd failed\n\n");
         close(fd);
         return NULL;
    }
}
