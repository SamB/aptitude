MAINTAINERCLEANFILES = Makefile.in

localedir = $(datadir)/locale
INCLUDES = -I$(top_builddir) -I$(srcdir) -I$(top_srcdir) -I$(top_srcdir)/src
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@
LDADD = @LIBINTL@

noinst_LIBRARIES = libgeneric-matching.a

libgeneric_matching_a_SOURCES = \
	parse.cc	\
	parse.h		\
	pattern.cc	\
	pattern.h