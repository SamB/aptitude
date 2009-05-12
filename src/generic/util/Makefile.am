MAINTAINERCLEANFILES = Makefile.in

localedir = $(datadir)/locale
INCLUDES = -I$(top_builddir) -I$(srcdir) -I$(top_srcdir) -I$(top_srcdir)/src
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

noinst_LIBRARIES = libgeneric-util.a
libgeneric_util_a_SOURCES = \
	dense_setset.h \
	dirent_safe.h \
	immlist.h \
	immset.h \
	maybe.h \
	mut_fun.h \
	refcounted_base.cc \
	refcounted_base.h \
	refcounted_wrapper.h \
	safe_slot.h \
	setset.h \
	temp.cc \
	temp.h \
	undo.cc \
	undo.h \
	util.cc \
	util.h