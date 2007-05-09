INCLUDES = -Wall @WERROR@ -I$(top_builddir) -I$(srcdir) -I$(top_srcdir) -I$(top_srcdir)/src

EXTRA_DIST=model.tex test1.txt test3.txt test4.txt resolver_undo.h

noinst_PROGRAMS=test

test_LDADD = $(top_builddir)/src/generic/util/libgeneric-util.a

test_SOURCES=test.cc dump_universe.h \
	dummy_universe.cc dummy_universe.h \
	exceptions.h problemresolver.h \
	sanity_check_universe.h solution.h
