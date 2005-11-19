
# sanity
AC_INIT(Setup.lhs)

# Prepare to generate the config.h.in file
AC_CONFIG_HEADERS([cbits/config.h])

# this is arbitrary
AC_PREREQ([2.52])

AC_PATH_PROG(MPG321,mpg321)
if test -z "$MPG321" ; then
    AC_PATH_PROG(MPG321,mpg123)
    if test -z "$MPG321" ; then
	    AC_MSG_ERROR([You need mpg321 or mpg123 installed to run hmp3])
    fi
fi
AC_DEFINE_UNQUOTED(MPG321, "$MPG321", [Path to mpg321])

#
# The shell variables should be added to, e.g., $LIBS
#
AC_CHECK_LIB(ncurses, addnstr)
if echo "$LIBS" | grep ncurses > /dev/null 2>&1 ; then
        :
else
        AC_CHECK_LIB(curses,  addnstr)
fi

REPO_PATH_=`sed -n 1p _darcs/prefs/repos`
AC_DEFINE_UNQUOTED(REPO_PATH, "$REPO_PATH_", [Path to hmp3 repo])

PATCH_COUNT_=`darcs changes --xml-output | sed -n '/^<patch/p;/TAG/q' | wc -l | sed 's/ *//g'`
AC_DEFINE_UNQUOTED(PATCH_COUNT, $PATCH_COUNT_, [Current patch count])

AC_CHECK_HEADERS([regex.h sys/types.h])

AC_SUBST(SYMS)

AC_OUTPUT
