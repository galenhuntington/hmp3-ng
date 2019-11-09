[![Build Status](https://travis-ci.com/galenhuntington/hmp3-ng.svg?branch=master)](https://travis-ci.com/galenhuntington/hmp3-ng)

##  hmp3-ng

The `hmp3` music player, written in Haskell, dates to 2005, and has a
curses-based interface which can be used in an ordinary text terminal.
But it has become abandonware: the last update was in June 2008,
and it no longer builds with today's Haskell and standard libraries.

This repository is a work in progress to resurrect this software.

The original Darcs repo has vanished from the Internet.  However, I
have a copy I checked out in 2008 (to hack on!) with all the patches
through version 1.5.1 (the latest is 1.5.2.1, which I had at one point
but cannot find), and Hackage hosts tarballs for several versions.

*  I used [darcs-to-git](https://github.com/purcell/darcs-to-git)
to port to Git.

*  I added commits for the changes in the two later published versions.
These were quite minor, the bulk being the automated regeneration of a
`configure` file (now gone).

*  I updated the code to compile under modern GHC (8.6.5 as of this
writing) and libraries.  Some code could not be directly ported,
mainly low-level optimizations, and so was replaced.

*  I have added support for building with Stack.

*  There is a GitHub issue tracker, and Travis integration to
continuously test builds.

*  All C code is removed.  There is still some use of the FFI.

*  Cabal is configured via the more modern
[hpack](https://github.com/sol/hpack) format.

*  I try to avoid “Not Invented Here” by using established,
up-to-date packages from Hackage.

*  Work on other features and changes, and documentation, is ongoing.

I have also made a few changes and additions to the key bindings per
my preference.

I am still working out the flaws.  Let me know if there are problems.


##  Installation

Either `cabal install` or `stack install` will build a binary.
You will need to have `mpg321` installed, which is free software
and widely available in package managers.  Alternatively, `mpg123`
can be used by compiling with the `-DMPG123` option, but, while your
mileage may vary, in my experience it doesn't work as well.

The build depends on the package `hscurses`, which in turn requires
curses dev files.  In Ubuntu/Debian, for example, these can be
gotten by installing `libncurses5-dev`.  You probably also need
`libncursesw5-dev`.


##  Use

The `hmp3` executable is called with arguments containing a list of mp3
files or directories of mp3 files.  With no arguments, it will use the
playlist from the last time it was run, which is stored in `~/.hmp3db`.

```
$ hmp3 ~/Music ~/Downloads/La-La.mp3
$ hmp3
```

Once running, `hmp3` is controlled by fairly intuitive key commands.
`h` shows a help menu, and `q` quits.

A color scheme can be specified by writing out a `Config { .. }`
value in `~/.hmp3`.  See `Style.hs` for the definition.  The `l`
command reloads this configuration.


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

