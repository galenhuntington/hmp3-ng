[![Hackage](https://img.shields.io/hackage/v/hmp3-ng.svg)](https://hackage.haskell.org/package/hmp3-ng)
![Build status](https://github.com/galenhuntington/hmp3-ng/actions/workflows/haskell.yml/badge.svg)

##  hmp3-ng

The `hmp3` music player, written in Haskell, dates to 2005, and has a
curses interface for use in a text terminal.  However, it has become
abandonware: the last update was in June 2008, and it no longer builds
with today’s Haskell and standard libraries.

This repository is an effort to resurrect this software.

The original Darcs repo has vanished from the Internet.  However, I
have a copy I checked out in 2008 (to hack on!) with all the patches
through version 1.5.1 (the latest is 1.5.2.1), and Hackage has tarballs
for the later versions.

*  I used [darcs-to-git](https://github.com/purcell/darcs-to-git) to
port to Git.  I manually added commits for the two later published
versions, which were only minor changes, mostly the automated
regeneration of a `configure` file (now gone).

*  The code has been updated to compile under recent GHC (currently
8.6, 8.8, 8.10, 9.0, and 9.2) and libraries.  This required rewriting
or entirely replacing large sections, mainly low-level optimizations.

*  I added support for building with Stack.

*  There is a public GitHub issue tracker, and a GitHub action to
continuously test builds.

*  I try to avoid “Not Invented Here” by using established,
up-to-date packages from Hackage.  Much old code has now been
“outsourced” and simplified.

*  All C code is removed, replaced with libraries from Hackage.
There is still some use of the FFI.

*  Unicode is supported in titles and filenames, and Unicode glyphs
are utilized in the interface.

*  It is much more stable.  The app used to crash frequently and
require restart, but I’ve had `hmp3-ng` running multiple times
continuously for more than a year with heavy use without any problems.

*  Several additions and changes have been made to the feature set
and the UI.  A few of the key bindings have been modified per my
preference.

*  Work on other features and changes, and documentation, is ongoing.

This is still a work in progress.  Let me know if there are problems.


##  Installation

Either `cabal install` or `stack install` will build a binary.
You will need to have `mpg321` installed, which is free software
and widely available in package managers.  Alternatively, `mpg123`
can be used by compiling with the `-DMPG123` option, but, while your
mileage may vary, in my experience it doesn’t work as well.

The build depends on the package `hscurses`, which in turn requires
curses dev files.  In Ubuntu/Debian, for example, these can be
obtained by installing `libncurses5-dev`.  You probably also need
`libncursesw5-dev`.


##  Use

The `hmp3` executable is invoked with a list of mp3 files or
directories of mp3 files.  With no arguments, it will use the
playlist from the last time it was run, which is stored in an XDG
cache directory, usually `~/.cache/hmp3/playlist.db`.

```
$ hmp3 ~/Music ~/Downloads/La-La.mp3
$ hmp3
```

Once running, `hmp3` is controlled by fairly intuitive key commands.
`h` shows a help menu, and `q` quits.  `hmp3 -h` prints a simple help
message with command line options.

A color scheme can be specified by writing out a `Config { .. }`
value in `~/.config/hmp3/style.conf` or the equivalent in your
XDG config directory.  See `Style.hs` for the definition.  The `l`
command hot-reloads this configuration.


##  Original authorship list

```
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

