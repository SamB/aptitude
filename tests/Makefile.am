MAINTAINERCLEANFILES = Makefile.in

INCLUDES = -Wall @WERROR@ -I$(top_builddir) -I$(top_srcdir) -I$(top_srcdir)/src -I$(srcdir)
LDADD = $(top_builddir)/src/generic/apt/libgeneric-apt.a $(top_builddir)/src/generic/problemresolver/dummy_universe.o $(top_builddir)/src/generic/util/libgeneric-util.a $(top_builddir)/src/vscreen/libvscreen.a -lcppunit

check_PROGRAMS = test

noinst_PROGRAMS = interactive_set_test

TESTS = test

EXTRADIST = data

interactive_set_test_SOURCES = interactive_set_test.cc

# Note: test_apt_universe is not built by default because it takes way
# too long.  Of course, ideally this would be done in a less ad-hoc
# way...
test_SOURCES = \
	main.cc \
	test_dense_setset.cc \
	test_eassert.cc \
	test_misc.cc \
	test_resolver.cc \
	test_setset.cc \
	test_tags.cc \
	test_temp.cc \
	test_threads.cc \
	test_wtree.cc
