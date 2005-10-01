MAINTAINERCLEANFILES=Makefile.in

localedir = $(datadir)/locale
INCLUDES = -Wall @WERROR@ -I$(top_builddir) -I$(srcdir) -I$(top_srcdir) -I$(top_srcdir)/src
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@
LDADD = @LIBINTL@

noinst_LIBRARIES=libconf.a

libconf_a_SOURCES=\
	colors.h	\
	colors.cc	\
	column_definition.h\
	column_definition.cc\
	keybindings.h	\
	keybindings.cc	\
	style.h	\
	style.cc
