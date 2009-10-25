MAINTAINERCLEANFILES = Makefile.in

INCLUDES = -I$(top_builddir) -I$(top_srcdir) -I$(top_srcdir)/src -I$(srcdir)
AM_CPPFLAGS = -DBOOST_TEST_DYN_LINK
LDADD = $(top_builddir)/src/loggers.o			       \
$(top_builddir)/src/generic/apt/matching/libgeneric-matching.a \
$(top_builddir)/src/generic/apt/libgeneric-apt.a	       \
$(top_builddir)/src/generic/apt/matching/libgeneric-matching.a \
$(top_builddir)/src/generic/problemresolver/libgeneric-problemresolver.a   \
$(top_builddir)/src/generic/util/libgeneric-util.a -lcppunit -lboost_unit_test_framework

check_PROGRAMS = cppunit_test boost_test

noinst_PROGRAMS = interactive_set_test

TESTS = cppunit_test boost_test

EXTRA_DIST = file_caches

interactive_set_test_SOURCES = interactive_set_test.cc

test_choice.o test_choice_set.o test_resolver.o: $(top_srcdir)/src/generic/problemresolver/*.h
test_promotion_set.o test_resolver_hints.o: $(top_srcdir)/src/generic/problemresolver/*.h

# Note: test_apt_universe is not built by default because it takes way
# too long.  Of course, ideally this would be done in a less ad-hoc
# way...
cppunit_test_SOURCES = \
	cppunit_test_main.cc \
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

boost_test_SOURCES = \
	boost_test_main.cc \
	test_file_cache.cc \
	test_sqlite.cc
