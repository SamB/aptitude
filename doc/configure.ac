AC_INIT([aptitude-doc],[0.6.5])
AC_CONFIG_SRCDIR([en/aptitude.xml])
AM_INIT_AUTOMAKE([1.11 -Wall foreign])

AC_PATH_PROG(PO4A, po4a)
AM_CONDITIONAL(USE_PO4A, [test "x$PO4A" != "x"])

AC_CONFIG_FILES([
	Makefile
	cs/Makefile
	cs/images/Makefile
	de/Makefile
	en/Makefile
	en/images/Makefile
	es/Makefile
	es/images/Makefile
	fi/Makefile
	fi/images/Makefile
	fr/Makefile
	fr/images/Makefile
	ja/Makefile
	ja/images/Makefile
	pl/Makefile
	po4a/Makefile
	po4a/po/Makefile
	po4a/add_de/Makefile
	po4a/add_es/Makefile
	po4a/add_fr/Makefile
	po4a/add_pl/Makefile
])
AC_OUTPUT
