MAINTAINERCLEANFILES = Makefile.in

INCLUDES = -I$(top_builddir) -I$(top_srcdir) -I$(top_srcdir)/src -I$(srcdir)
LDADD = $(top_builddir)/src/loggers.o			       \
$(top_builddir)/src/generic/apt/matching/libgeneric-matching.a \
$(top_builddir)/src/generic/apt/libgeneric-apt.a	       \
$(top_builddir)/src/generic/apt/matching/libgeneric-matching.a \
$(top_builddir)/src/generic/problemresolver/libgeneric-problemresolver.o   \
$(top_builddir)/src/generic/util/libgeneric-util.a -lcppunit

check_PROGRAMS = test

noinst_PROGRAMS = interactive_set_test

TESTS = test

EXTRADIST = resolver_inputs

interactive_set_test_SOURCES = interactive_set_test.cc

# Note: test_apt_universe is not built by default because it takes way
# too long.  Of course, ideally this would be done in a less ad-hoc
# way...
test_SOURCES = \
	main.cc \
	test_choice.cc \
	test_choice_set.cc \
	test_config_pusher.cc \
	test_dense_setset.cc \
	test_matching.cc \
	test_misc.cc \
	test_promotion_set.cc \
	test_resolver.cc \
	test_resolver_hints.cc \
	test_setset.cc \
	test_tags.cc \
	test_temp.cc \
	test_wtree.cc
