
##  hmp3-ng

The `hmp3` music player was introduced in 2005, and has a nice,
curses-based interface.  I have myself been using it for over a decade.
But it has become abandonware: the last update was in June 2008,
and it no longer builds with today's Haskell and standard libraries.

This repository is an attempt to resurrect this software.  It is a
work in progress.

The original Darcs repo has vanished from the Internet.  However, I
have a copy I checked out in 2008 (to hack on!) with all the patches
through version 1.5.1 (the latest is 1.5.2.1, which I had at one point
but cannot find), and Hackage hosts tarballs for following versions.

*  I used [darcs-to-git](https://github.com/purcell/darcs-to-git)
to port to Git.

*  I added commits for the changes in the two later versions.
These were fairly small, the bulk being the regeneration of the
`configure` file using GNU `autoconf`.

*  I updated the code to compile under modern GHC (8.6.5 as of this
writing) and libraries.  Some code could not be directly ported,
mainly low-level optimizations, and so was replaced.

*  There is a Github issue tracker to cover other problems and
planned changes.

*  I have added support for building with Stack.

*  Work on other features and changes is ongoing.

I have changed around a few of the key bindings per my own preference,
and added Backspace to rewind to the start of a song.

The original README, which will have to be updated and integrated:

```
            hmp3 : an ncurses mp3 player written in Haskell
          ---------------------------------------------------

Dependencies:
    mpg321    http://mpg321.sourceforge.net/

Building:
    $ chmod +x configure Setup.hs
    $ ./Setup.hs configure --prefix=/home/dons
    $ ./Setup.hs build
    $ ./Setup.hs install

This assumes you have Cabal installed. Cabal is installed by default
with newer GHCs. If your Cabal version is too old (v1.0 that comes with
GHC 6.4.1 is too old), or if you see:
      dist/build/hmp3-tmp/cbits/utils.o: No such file or directory
errors, then you need to download a newer Cabal version first.

Use:
    To populate a playlist, and start:

    $ hmp3 ~/mp3/dir/
     or
    $ hmp3 a.mp3 b.mp3

    From then on this playlist is saved in ~/.hmp3db, and is reloaded
    automatically, if you restart hmp3 with no arguments.

    $ hmp3

    Type 'h' to display the help page. The other commands are explained
    on the help screen, which can be accessed by typing 'h'. Quit with
    'q'.

Configuration:

    Colours may be configured at runtime by editing the "~/.hmp3" file.
    An example would be:

        Config { 
                 hmp3_window      = ("brightwhite", "black")
               , hmp3_helpscreen  = ("black",       "cyan")
               , hmp3_titlebar    = ("green",       "blue")
               , hmp3_selected    = ("brightwhite", "black")
               , hmp3_cursors     = ("black",       "cyan")
               , hmp3_combined    = ("black",       "cyan")
               , hmp3_warnings    = ("brightwhite", "red")
               , hmp3_blockcursor = ("black",       "darkred")
               , hmp3_progress    = ("cyan",        "white")
             }

    After editing this file, hit 'l' to have the changes used by hmp3.

    The keymaps are configurable by adding your own bindings to
    Config.hs (as are the colours). Edit this file before compilation.

    Alternatively, you can switch between dark and light settings without
    recompiling. If you have a light xterm, set HMP_HAS_LIGHT_BG=true in
    your shell, and hmp3 will use the light color settings by default.

Limitations:
    It only plays mp3 files (and variants supported by mpg{321,123}.
    So no ogg files for now. This is on the todo list though.

    It is possible to use mpg123, however it appears to be more error
    prone, and less stable than mpg321.

Platforms and portability:
    hmp3 has been confirmed to work on:
        OpenBSD/x86
        Linux/x86
        FreeBSD/x86
        Linux/ppc
        MacOSX/ppc
        Irix/mips64

    * You need -threaded.

    * On platforms without a native code generator, you should remove the
    reference to -fasm in the hmp3.cabal file

License:
    GPL

Author:
    Don Stewart, Tue Jan 15 15:16:55 PST 2008

Contributors:
    Samuel Bronson
    Stefan Wehr
    Tomasz Zielonka
    David Himmelstrup

```