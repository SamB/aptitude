dnl Process this file with autoconf to produce a configure script.
AC_INIT(aptitude, 0.4.3)
AC_CONFIG_SRCDIR([src/main.cc])
AM_INIT_AUTOMAKE
AM_CONFIG_HEADER(config.h)

dnl Use C++
AC_LANG([C++])

dnl Checks for programs.
AC_PROG_CXX
AC_PROG_RANLIB
dnl Checks for programs.
AC_PATH_PROG(PO4A, po4a)
if test "x$PO4A" = "x" ; then
  AM_CONDITIONAL(USE_PO4A, false)
else
  AM_CONDITIONAL(USE_PO4A, true)
fi

dnl C++ has const and inline!!!
ac_cv_c_const=yes
ac_cv_c_inline=yes

dnl Checks for libraries.
AC_CHECK_LIB(ncursesw, initscr, ,
	[AC_MSG_ERROR([Can't find libncursesw -- please install libncursesw5-dev])])
AC_CHECK_LIB(apt-pkg, main, , [AC_MSG_ERROR([Can't find the APT libraries -- please install libapt-pkg-dev])])
AC_CHECK_LIB(pthread, main,
	HAVE_LIBPTHREAD=1
	, [AC_MSG_ERROR([Can't find the POSIX thread libraries])])

ALL_LINGUAS="ar bs ca cs da de el es eu fi fr gl hu it ja km ku lt nb nl nn pl pt pt_BR ro ru sk sv tl tr vi zh_CN zh_TW"
AM_GNU_GETTEXT([external])

dnl Old sigc:
dnl AM_PATH_SIGC(1.0.1, , AC_MSG_ERROR([Can't find libsigc++ 1.0.1 -- please install it.]))

dnl Less old sogc:
dnl PKG_CHECK_MODULES(SIGC, sigc++-1.2)

PKG_CHECK_MODULES(SIGC, sigc++-2.0)

CXXFLAGS="$CXXFLAGS $SIGC_CFLAGS"
LIBS="$LIBS $SIGC_LIBS"
AC_DEFINE_UNQUOTED(SIGC_VERSION, ["$(pkg-config --modversion sigc++-2.0)"], [The version of libsigc++ with which the program was compiled])

dnl Checks for header files.
AC_HEADER_STDC

AC_CHECK_HEADERS(fcntl.h sys/ioctl.h unistd.h, , [AC_MSG_ERROR([Can't find needed header file.])])

AC_CHECK_HEADER(apt-pkg/init.h, , [AC_MSG_ERROR([Can't find the APT header files -- please install libapt-pkg-dev])])

AC_CHECK_HEADER(execinfo.h, [AC_DEFINE([HAVE_EXECINFO_H], [], [Define if the execinfo file (for self-backtracing) is available])])

AC_CHECK_HEADER(locale.h, [AC_DEFINE([HAVE_LOCALE_H], [], [Define if locale.h is available])])

AC_CHECK_DECL(setlocale, [AC_DEFINE([HAVE_SETLOCALE], [], [Define if setlocale is available in locale.h])], , [#include <locale.h>])

HASH_INC=""
HASH_CLS=""

AC_CHECK_HEADER(hash_map,
		[HASH_INC="hash_map"
		 HASH_CLS="hash_map"
		 AC_DEFINE(HAVE_HASH_MAP, ,
			  [Define if the g++ extension hash_map is available])],
	  	[AC_CHECK_HEADER(ext/hash_map,
				 [HASH_INC="ext/hash_map"
				  HASH_CLS="hash_map"
				  AC_DEFINE(HAVE_EXT_HASH_MAP, , [Define if the g++ extension ext/hash_map is available])])])

AC_CHECK_HEADER(hash_set,
		[HASH_INC="hash_set"
		 HASH_CLS="hash_set"
		 AC_DEFINE(HAVE_HASH_SET,  , [Define if the g++ extension hash_set is available])],
		[AC_CHECK_HEADER(ext/hash_set,
				[HASH_INC="ext/hash_set"
				 HASH_CLS="hash_set"
				 AC_DEFINE(HAVE_EXT_HASH_SET,  , [Define if the g++ extension ext/hash_set is available])])])

AC_MSG_CHECKING([whether apt supports ddtp])

AC_COMPILE_IFELSE(AC_LANG_PROGRAM([[#include <apt-pkg/pkgcache.h>]],
				  [[pkgCache::DescIterator d;]]),
		  AC_MSG_RESULT([yes])
		  AC_DEFINE([HAVE_DDTP], [], [Set to true if apt is DDTP-enabled]),
		  AC_MSG_RESULT([no]))

# Try to guess what namespace the hash stuff is in (maybe I should just
# use rbltrees instead in most places?)
if test x$HASH_INC != x
then
    AC_MSG_CHECKING([which namespace contains the hash extensions])

    HASH_NAMESPACE=""

    for NS in std __gnu_cxx
    do
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <$HASH_INC>],
	 			         [${NS}::${HASH_CLS} < int, int > foo;])],
			[HASH_NAMESPACE=$NS])
    done

    if test x$HASH_NAMESPACE = x
    then
       AC_MSG_ERROR([found hash classes, but they aren't in the std or GNU namespace.])
    fi

    AC_MSG_RESULT($HASH_NAMESPACE)
else
    # Use the std namespace if all else fails. (the code will fall back
    # to using map instead of hash_map, set instead of hash_set)
    HASH_NAMESPACE=std
fi

AC_DEFINE_UNQUOTED(HASH_NAMESPACE, $HASH_NAMESPACE, [If a hash header is available, define to the namespace in which the hash class resides])




# Try to guess how to extend string traits
TRAITS_CLASS=""

AC_MSG_CHECKING([for the name of the character traits template])

for T in char_traits string_char_traits
do
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <string>],
				     [std::basic_string<unsigned char,
						        std::$T < unsigned char > > s;])],
		    TRAITS_CLASS=$T)
done

if test x$TRAITS_CLASS = xstring_char_traits
then
    AC_MSG_WARN([Your compiler uses string_char_traits for its character traits.  Some compilers (eg, g++ 2.95) which use this name for the character traits template are known to have hideously broken implementations of the standard string class, which cause aptitude to fail to compile.  If you have a compiler with this problem, please upgrade it to a version that has a more compliant version of the STL (g++ >=3.0 is known to work).  You can specify which compiler this script should use via the CXX environment variable.])
fi

if test x$TRAITS_CLASS = x
then
  AC_MSG_ERROR([can't find the name of the character traits template])
else
  AC_DEFINE_UNQUOTED(TRAITS_CLASS, $TRAITS_CLASS, [The name of the class used by the STL to define character traits])

  AC_MSG_RESULT([$TRAITS_CLASS])
fi




if test x$HAVE_LIBPTHREAD = x1
then
  AC_CHECK_HEADER(pthread.h,
	CXXFLAGS="$CXXFLAGS -D_REENTRANT"
	LIBS="$LIBS -lpthread"
	[AC_DEFINE([HAVE_LIBPTHREAD], [] , [Define if pthread is available])]
	,
	[AC_MSG_ERROR([POSIX thread header not installed])])
fi

WERROR="-Werror"

AC_ARG_ENABLE(dynamic-backtrace,
	AS_HELP_STRING([--enable-dynamic-backtrace], [Modify the executable so that it can generate a backtrace for uncaught exceptions.  Will double the size of the stripped binary.]),
	[if test x$enableval = xyes
	then
	  AC_DEFINE([ENABLE_DYNAMIC_BACKTRACE], [], [Define to enable dynamic generation of backtraces if HAVE_EXECINFO_H is defined])
	  LDFLAGS="$LDFLAGS -Wl,--export-dynamic"
	fi]
	)
AC_ARG_ENABLE(werror,
	AS_HELP_STRING([--disable-werror], [do not compile with -Werror]),
	if test x$enableval = xno
	then
	  WERROR=""
	fi
	)
AC_ARG_ENABLE(period-beep,
	AS_HELP_STRING([--enable-period-beep], [periodically (once a second) produce an annoying beep]),
	if test x$enableval = xyes
	then
	  [AC_DEFINE(WITH_PERIOD_BEEP,  , [Define to produce an annoying beep periodically, for debugging])]
	fi
	)
AC_ARG_ENABLE(reload-cache,
	AS_HELP_STRING([--enable-reload-cache], [allow the user to reload the cache on demand. (debugging feature)]),
	if test x$enableval = xyes
	then
	  [AC_DEFINE(WITH_RELOAD_CACHE,  , [Allow the cache to be reloaded on demand])]
	fi
	)

AC_ARG_ENABLE(package-state-loc,
	AS_HELP_STRING([--with-package-state-loc], [use the given location for storing state (default /var/lib/aptitude)]),
	if test x$enableval = xyes
	then
	  [AC_MSG_ERROR([--with-package-state-loc needs an argument])]
	else
	  STATEDIR="$enableval"
	fi,
	STATEDIR="/var/lib/aptitude")

AC_ARG_ENABLE(lock-loc,
	AS_HELP_STRING([--with-lock-loc], [use the given location for storing state (default /var/lock/aptitude)]),
	if test x$enableval = xyes
	then
	  [AC_MSG_ERROR([--with-lock-loc needs an argument])]
	else
	  LOCKFILE="$enableval"
	fi,
	LOCKFILE="/var/lock/aptitude")

AC_ARG_ENABLE(aptitude,
	AS_HELP_STRING([--disable-aptitude], [don't compile the program itself]),
	if test x$enableval=xno
	then
	  SRCDIRS=
	else
	  SRCDIRS=src
	fi,
	SRCDIRS=src)

AC_ARG_ENABLE(docs,
	AS_HELP_STRING([--disable-docs], [don't generate the documentation]),
	if test x$enableval=xno
	then
	  DOCDIRS=
	else
	  DOCDIRS=doc
	fi,
	DOCDIRS=doc)

AC_SUBST(SRCDIRS)
AC_SUBST(DOCDIRS)

AC_DEFINE_UNQUOTED(STATEDIR, "$STATEDIR", [The location of aptitude's state files])
AC_DEFINE_UNQUOTED(LOCKFILE, "$LOCKFILE", [The location of the lock to prevent simultaneous aptitude instances])
AC_SUBST(STATEDIR)
AC_SUBST(WERROR)

AC_TYPE_SIZE_T

dnl Checks for library functions.
AC_CHECK_FUNCS(strdup)

CPPFLAGS="$CPPFLAGS -DHELPDIR=\\\"\$(datadir)/aptitude\\\""
CPPFLAGS="$CPPFLAGS -DPKGDATADIR=\\\"\$(datadir)/aptitude\\\""
dnl FC_EXPAND_DIR(HELPDIR, [$datadir/aptitude])
dnl AC_DEFINE_UNQUOTED(HELPDIR, "$HELPDIR", [The location in which the online help data is placed])

dnl FC_EXPAND_DIR(PKGDATADIR, [$datadir/aptitude])
dnl AC_DEFINE_UNQUOTED(PKGDATADIR, "$PKGDATADIR", [The location in which the program data is placed])

AC_CONFIG_FILES([
	Doxyfile
	Makefile
	doc/Makefile
	doc/cs/Makefile
	doc/cs/images/Makefile
	doc/de/Makefile
	doc/en/Makefile
	doc/en/images/Makefile
	doc/fi/Makefile
	doc/fi/images/Makefile
	doc/fr/Makefile
	doc/fr/images/Makefile
	doc/ja/Makefile
	doc/ja/images/Makefile
	doc/po4a/Makefile
	doc/po4a/po/Makefile
	doc/po4a/add_fr/Makefile
	m4/Makefile
	po/Makefile.in
	src/Makefile
	src/cmdline/Makefile
	src/generic/Makefile
	src/generic/apt/Makefile
	src/generic/problemresolver/Makefile
	src/generic/util/Makefile
	src/mine/Makefile
	src/vscreen/Makefile
	src/vscreen/config/Makefile
	tests/Makefile
])

AC_OUTPUT
