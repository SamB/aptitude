dnl Process this file with autoconf to produce a configure script.
AC_INIT(aptitude, 0.5.9rc2)
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

AC_MSG_CHECKING([whether apt includes the automatic dependency removal patch (required)])

AC_COMPILE_IFELSE(AC_LANG_PROGRAM([[#include <apt-pkg/depcache.h>]],
				  [[extern pkgDepCache::ActionGroup d;]]),
		  AC_MSG_RESULT([yes]),
		  AC_MSG_FAILURE([need libapt-pkg 0.7 or later]))

AC_CHECK_LIB(pthread, main,
	HAVE_LIBPTHREAD=1
	, [AC_MSG_ERROR([Can't find the POSIX thread libraries])])

ALL_LINGUAS="ar ast bs ca cs da de dz el es eu fi fr gl hu it ja km ku lt mr nb ne nl nn pl pt pt_BR ro ru sk sv th tl tr uk vi zh_CN zh_TW"
AM_GNU_GETTEXT([external])

PKG_CHECK_MODULES(ept, libept, [], [AC_MSG_ERROR([Can't find the ept library -- please install libept-dev])])
AC_DEFINE([HAVE_EPT], [], [Backwards compatibility symbol; must always be defined.])

PKG_CHECK_MODULES(SIGC, sigc++-2.0)

PKG_CHECK_MODULES(CWIDGET, cwidget)

PKG_CHECK_MODULES(LOG4CXX, liblog4cxx)

PKG_CHECK_MODULES(VTE, vte)

dnl Check for Boost headers.  Place each one on a line by itself and
dnl write "dnl" at the end: the "dnl" deletes the newline, and without
dnl it you'll get an error when you run the configure script.
AC_CHECK_HEADERS( dnl
boost/flyweight/hashed_factory.hpp dnl
boost/flyweight.hpp dnl
boost/format.hpp dnl
boost/functional/hash.hpp dnl
boost/function.hpp dnl
boost/iostreams/copy.hpp dnl
boost/iostreams/device/file.hpp dnl
boost/iostreams/filter/gzip.hpp dnl
boost/iostreams/filtering_stream.hpp dnl
boost/iostreams/invert.hpp dnl
boost/iostreams/operations.hpp dnl
boost/iostreams/read.hpp dnl
boost/iostreams/seek.hpp dnl
boost/iostreams/stream.hpp dnl
boost/iostreams/write.hpp dnl
boost/lambda/bind.hpp dnl
boost/make_shared.hpp dnl
boost/multi_index_container.hpp dnl
boost/multi_index/hashed_index.hpp dnl
boost/multi_index/member.hpp dnl
boost/multi_index/sequenced_index.hpp dnl
boost/optional.hpp dnl
boost/scoped_ptr.hpp dnl
boost/shared_ptr.hpp dnl
boost/test/unit_test.hpp dnl
boost/unordered_map.hpp dnl
boost/unordered_set.hpp dnl
                  ,
		  ,
		  [AC_MSG_FAILURE([Boost install not found, too old, or incomplete; install libboost-dev.])])

AX_BOOST_BASE()
AX_BOOST_IOSTREAMS

PKG_CHECK_MODULES(SQLITE3, sqlite3)

HAVE_GTK=1
AC_ARG_ENABLE(gtk,
	AS_HELP_STRING(--disable-gtk, [don't compile the GTK+/gtkmm frontend even if the appropriate libraries are available]),
	[if test x$withval = xyes
	 then
	     HAVE_GTK=1
	 else
	     HAVE_GTK=
	 fi])

WANT_HAVE_GTK=$HAVE_GTK

dnl We always test all the libraries, even if some of them fail, so
dnl that the user gets to see which libraries are needed for GTK+.
if test x$HAVE_GTK = x1
then
  AC_MSG_NOTICE([Testing for the libraries needed to build the GTK+ frontend.])

  PKG_CHECK_MODULES(GLIBMM, glibmm-2.4,,[HAVE_GTK=])
  PKG_CHECK_MODULES(GTHREAD, gthread-2.0,,[HAVE_GTK=])
  PKG_CHECK_MODULES(GTKMM, gtkmm-2.4,,[HAVE_GTK=])
  PKG_CHECK_MODULES(LIBGLADEMM, libglademm-2.4,,[HAVE_GTK=])
else
  AC_MSG_NOTICE([Disabling the GTK+ frontend at your request (--disable-gtk).])
fi

if test x$WANT_HAVE_GTK = x1 && test x$HAVE_GTK != x1
then
  AC_MSG_WARN([Unable to find the necessary GTK+ libraries; disabling GTK+ support.])
fi

if test x$HAVE_GTK = x1
then
  AC_MSG_NOTICE([The GTK+ frontend will be built.])
fi

AM_CONDITIONAL([BUILD_GTK], [test x$HAVE_GTK = x1])
if test x$HAVE_GTK = x1
then
  AC_DEFINE([HAVE_GTK], [], [Define if the GTK+ frontend is included in the build.])
fi

CXXFLAGS="$CXXFLAGS $LOG4CXX_CFLAGS $SIGC_CFLAGS $CWIDGET_CFLAGS $ept_CFLAGS $SQLITE3_CFLAGS"
LIBS="$LIBS $LOG4CXX_LIBS $SIGC_LIBS $CWIDGET_LIBS $ept_LIBS $SQLITE3_LIBS $BOOST_IOSTREAMS_LIB"
CPPFLAGS="$CPPFLAGS $BOOST_CPPFLAGS"
LDFLAGS="$LDFLAGS $BOOST_LDFLAGS"

if test x$HAVE_GTK = x1
then
  CXXFLAGS="$CXXFLAGS $GLIBMM_CFLAGS $GTHREAD_CFLAGS $GTKMM_CFLAGS $LIBGLADEMM_CFLAGS $VTE_CFLAGS"
  LIBS="$LIBS $GLIBMM_LIBS $GTHREAD_LIBS $GTKMM_LIBS $LIBGLADEMM_LIBS $VTE_LIBS"
fi


AC_DEFINE_UNQUOTED(SIGC_VERSION, ["$(pkg-config --modversion sigc++-2.0)"], [The version of libsigc++ with which the program was compiled])

dnl Checks for header files.
AC_HEADER_STDC

AC_CHECK_HEADERS(fcntl.h sys/ioctl.h unistd.h, , [AC_MSG_ERROR([Can't find needed header file.])])

AC_CHECK_HEADER(apt-pkg/init.h, , [AC_MSG_ERROR([Can't find the APT header files -- please install libapt-pkg-dev])])

AC_CHECK_HEADER(execinfo.h, [AC_DEFINE([HAVE_EXECINFO_H], [], [Define if the execinfo file (for self-backtracing) is available])])

AC_CHECK_HEADER(locale.h, [AC_DEFINE([HAVE_LOCALE_H], [], [Define if locale.h is available])])

AC_CHECK_DECL(setlocale, [AC_DEFINE([HAVE_SETLOCALE], [], [Define if setlocale is available in locale.h])], , [#include <locale.h>])

dnl All the hash-related stuff is commented out because g++ 4.3 will
dnl blow us up if we ever include hash_map -- yet it's present and I
dnl can't make the autoconf test fail.  The replacement is
dnl unordered_map, which is not just a nonstandard header: it also
dnl requires language extensions in order to work.  So, since aptitude
dnl has no real need for hashes anyway, I'm just removing all the
dnl hashes that it uses.

dnl HASH_INC=""
dnl HASH_CLS=""

dnl AC_CHECK_HEADER(hash_map,
dnl 		[HASH_INC="hash_map"
dnl 		 HASH_CLS="hash_map"
dnl 		 AC_DEFINE(HAVE_HASH_MAP, ,
dnl 			  [Define if the g++ extension hash_map is available])],
dnl 	  	[AC_CHECK_HEADER(ext/hash_map,
dnl 				 [HASH_INC="ext/hash_map"
dnl 				  HASH_CLS="hash_map"
dnl 				  AC_DEFINE(HAVE_EXT_HASH_MAP, , [Define if the g++ extension ext/hash_map is available])])])

dnl AC_CHECK_HEADER(hash_set,
dnl 		[HASH_INC="hash_set"
dnl 		 HASH_CLS="hash_set"
dnl 		 AC_DEFINE(HAVE_HASH_SET,  , [Define if the g++ extension hash_set is available])],
dnl 		[AC_CHECK_HEADER(ext/hash_set,
dnl 				[HASH_INC="ext/hash_set"
dnl 				 HASH_CLS="hash_set"
dnl 				 AC_DEFINE(HAVE_EXT_HASH_SET,  , [Define if the g++ extension ext/hash_set is available])])])

AC_MSG_CHECKING([whether apt supports ddtp])

AC_COMPILE_IFELSE(AC_LANG_PROGRAM([[#include <apt-pkg/pkgcache.h>]],
				  [[pkgCache::DescIterator d;]]),
		  AC_MSG_RESULT([yes])
		  AC_DEFINE([HAVE_DDTP], [], [Set to true if apt is DDTP-enabled]),
		  AC_MSG_RESULT([no]))

AC_MSG_CHECKING([whether apt supports the Homepage: field])

AC_COMPILE_IFELSE(AC_LANG_PROGRAM([[#include <apt-pkg/pkgrecords.h>]],
				  [[  pkgRecords::Parser *p = NULL;]]
				  [[  if(0) p->Homepage();]]),
		   AC_MSG_RESULT([yes])
		   AC_DEFINE([APT_HAS_HOMEPAGE], [], [Set to true if apt supports the Homepage: field]),
		   AC_MSG_RESULT([no]))

AC_MSG_CHECKING([whether apt supports dpkg triggers])

AC_COMPILE_IFELSE(AC_LANG_PROGRAM([[#include <apt-pkg/pkgcache.h>]],
			          [[pkgCache::State::TriggersAwaited;]]
				  [[pkgCache::State::TriggersPending;]]),
		  AC_MSG_RESULT([yes])
		  AC_DEFINE([APT_HAS_TRIGGERS], [], [Set to true if apt supports dpkg trigger states]),
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

AC_ARG_ENABLE(private-glade-file,
	AS_HELP_STRING([--disable-private-glade-file], [Always read the .glade file from \$datadir instead of reading one in ".".  This can be useful when installing aptitude globally, to avoid accidentally picking up the wrong version of the .glade file.]),
	[if test x$enableval = xno
	 then
	   AC_DEFINE([DISABLE_PRIVATE_GLADE_FILE], [], [Define to disable the use of a .glade file found relative to the current working directory.])
	 fi]
	)

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

AC_ARG_ENABLE(resolver-sanity-checks,
	AS_HELP_STRING([--enable-resolver-sanity-checks], [enable expensive internal sanity-checks in the dependency resolver]),
	if test x$enableval=xyes
	then
	  [AC_DEFINE(ENABLE_RESOLVER_SANITY_CHECKS,  ,  [Enable expensive internal sanity-checks in the dependency resolver.])]
	fi
	)

CXXFLAGS="$CXXFLAGS -Wall $WERROR"

AC_SUBST(SRCDIRS)
AC_SUBST(DOCDIRS)

AC_DEFINE_UNQUOTED(STATEDIR, "$STATEDIR", [The location of aptitude's state files])
AC_DEFINE_UNQUOTED(LOCKFILE, "$LOCKFILE", [The location of the lock to prevent simultaneous aptitude instances])
AC_SUBST(STATEDIR)

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
	doc/es/Makefile
	doc/fi/Makefile
	doc/fi/images/Makefile
	doc/fr/Makefile
	doc/fr/images/Makefile
	doc/ja/Makefile
	doc/ja/images/Makefile
	doc/po4a/Makefile
	doc/po4a/po/Makefile
	doc/po4a/add_de/Makefile
	doc/po4a/add_es/Makefile
	doc/po4a/add_fr/Makefile
	m4/Makefile
	po/Makefile.in
	src/Makefile
	src/gtk/Makefile
	src/gtk/util/Makefile
	src/cmdline/Makefile
	src/generic/Makefile
	src/generic/apt/Makefile
	src/generic/apt/history/Makefile
	src/generic/apt/matching/Makefile
	src/generic/problemresolver/Makefile
	src/generic/util/Makefile
	src/mine/Makefile
	tests/Makefile
])

AC_OUTPUT
