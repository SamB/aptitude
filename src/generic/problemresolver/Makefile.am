INCLUDES = -I$(top_builddir) -I$(srcdir) -I$(top_srcdir) -I$(top_srcdir)/src

EXTRA_DIST=model.tex test1.txt test3.txt test4.txt resolver_undo.h

noinst_PROGRAMS=test

test_LDADD = $(top_builddir)/src/generic/util/libgeneric-util.a

test_SOURCES=test.cc \
	choice.h choice_set.h dump_universe.h \
	dummy_universe.cc dummy_universe.h \
	exceptions.h \
	incremental_expression.cc incremental_expression.h \
	problemresolver.h \
	promotion_set.h sanity_check_universe.h \
	search_graph.h solution.h tier_limits.h
