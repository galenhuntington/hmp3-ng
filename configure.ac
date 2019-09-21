
AC_INIT(Setup.hs)
AC_CONFIG_HEADERS([cbits/config.h])
AC_PREREQ([2.52])

# Look for mpg321
AC_CHECK_PROGS(MPG321, [mpg321 mpg123], [false])
if test "$MPG321" = "false" ; then
    AC_MSG_ERROR([You need mpg321 or mpg123 installed to run hmp3])
fi
AC_DEFINE_UNQUOTED(MPG321, "$MPG321", [Which mp3 decoder to use])

# Some libs we need
AC_CHECK_LIB(curses,  addnstr)
AC_CHECK_FUNCS(use_default_colors)
AC_CHECK_HEADERS([regex.h sys/types.h])

AC_OUTPUT
