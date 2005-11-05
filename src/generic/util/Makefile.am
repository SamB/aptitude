MAINTAINERCLEANFILES = Makefile.in

localedir = $(datadir)/locale
INCLUDES = -Wall @WERROR@ -I$(top_builddir) -I$(srcdir) -I$(top_srcdir) -I$(top_srcdir)/src
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

noinst_LIBRARIES = libgeneric-util.a
libgeneric_util_a_SOURCES = \
	bool_accumulate.h \
	dense_setset.h \
	eassert.cc \
	eassert.h \
	event_queue.h \
	exception.cc \
	exception.h \
	immset.h \
	mut_fun.h \
	setset.h \
	slotarg.h \
	strhash.h \
	temp.cc \
	temp.h \
	threads.cc \
	threads.h \
	undo.cc \
	undo.h \
	util.cc \
	util.h