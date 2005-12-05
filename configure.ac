
# sanity
AC_INIT(Setup.lhs)

# Prepare to generate the config.h.in file
AC_CONFIG_HEADERS([cbits/config.h])

# this is arbitrary
AC_PREREQ([2.52])

AC_CHECK_PROG(MPG321,mpg321,mpg321)
if test -z "$MPG321" ; then
    AC_CHECK_PROG(MPG321,mpg123,mpg123)
    if test -z "$MPG321" ; then
	    AC_MSG_ERROR([You need mpg321 or mpg123 installed to run hmp3])
    fi
fi
AC_DEFINE_UNQUOTED(MPG321, "$MPG321", [Which mp3 decoder to use])

#
# The shell variables should be added to, e.g., $LIBS
#
AC_CHECK_LIB(ncurses, addnstr)
if echo "$LIBS" | grep ncurses > /dev/null 2>&1 ; then
        :
else
        AC_CHECK_LIB(curses,  addnstr)
fi

AC_CHECK_FUNCS(use_default_colors)

# only scan _darcs if we're in a devel branch
if test -d "_darcs" ; then
    PATCH_COUNT_=`darcs changes --xml-output | sed -n '/^<patch/p;/TAG/q' | wc -l | sed 's/ *//g'`
else
    PATCH_COUNT_=""
fi
AC_DEFINE_UNQUOTED(PATCH_COUNT, "$PATCH_COUNT_", [Current patch count])

AC_CHECK_HEADERS([regex.h sys/types.h])

AC_SUBST(SYMS)

AC_OUTPUT
