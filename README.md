[![Hackage](https://img.shields.io/hackage/v/hmp3-ng.svg)](https://hackage.haskell.org/package/hmp3-ng)
![Build status](https://github.com/galenhuntington/hmp3-ng/actions/workflows/haskell.yml/badge.svg)

##  hmp3-ng

The original `hmp3` music player, written in Haskell, dates to 2005,
and has a curses interface for use in a text terminal.  However,
it has become abandonware: the last update was in June 2008, and
it no longer builds with today’s Haskell and standard libraries.
This repository is an effort to resurrect this software.

The original Darcs repo has vanished from the Internet.  However, I
have a copy I checked out in 2008 (to hack on!) with all the patches
through version 1.5.1 (the latest is 1.5.2.1), and Hackage has tarballs
for the later versions.

*  I used [darcs-to-git](https://github.com/purcell/darcs-to-git) to
port to Git.  I manually added commits for the two later published
versions, which were only minor changes, mostly the automated
regeneration of a `configure` file (now gone).

*  The code has been updated to compile under recent GHC (tested
through 9.10) and libraries.  This required rewriting or entirely
replacing large sections, mainly low-level optimizations.

*  I added support for building with Stack.  It can also be installed
from Nix.

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
You will need to have `mpg123` installed, which is free software and
widely available in package managers.  Alternatively, `mpg321` can
be used by compiling with the `-DMPG321` option.  In my experience,
the latter worked better, but it too is abandoned, with no update
since 2012, and is no longer available on many systems.

The build depends on the package `hscurses`, which in turn requires
curses dev files.  In Ubuntu/Debian, for example, these can be obtained
by installing `libncurses-dev`.


##  Use

The `hmp3` executable is invoked with a list of mp3 files or
directories of mp3 files.

```
$ hmp3 ~/Music ~/Downloads/La-La.mp3
```

Once running, `hmp3` is controlled by fairly intuitive key commands.
`h` shows a help menu, and `q` quits.  `hmp3 -h` prints a simple help
message with command line options.

A color scheme can be specified by writing out a `Config { .. }`
value in `~/.config/hmp3/style.conf` (or wherever your XDG config is).
See `Style.hs` for the definition.  The `l` command hot-reloads this
configuration.


##  Original authorship

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

