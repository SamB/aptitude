MAINTAINERCLEANFILES=Makefile.in aclocal.m4 config.h.in

# Do this the simple way -- it means "make dist" will fail if you
# disable stuff, and has other ugliness, but it is much easier and
# more maintainable than the "correct" alternatives.
SRCDIRS=@SRCDIRS@
DOCDIRS=@DOCDIRS@

SUBDIRS=$(SRCDIRS) $(DOCDIRS) m4 po tests

MANPAGE_LOCALES=fr gl it pl

TLMANPAGES = $(wildcard aptitude.??.8)
TLHELPTXTS = help.txt $(wildcard help-??.txt) $(wildcard help-??_??.txt)
EXTRA_DIST = config.rpath  ChangeLog.SVN Doxyfile.in FAQ README.i18n README.SMART-POINTERS README.THREADS README.VSCREEN aptitude-hackers-guide.txt aclocal.m4 \
	function_groups function_pkgs aptitude-defaults \
	$(TLMANPAGES) $(TLHELPTXTS)

pkgdatadir=$(datadir)/$(PACKAGE)

pkgdata_DATA = $(TLHELPTXTS) COPYING NEWS \
	function_groups function_pkgs aptitude-defaults

install-data-local:
	[ -d $(DESTDIR)$(STATEDIR) ] || $(mkinstalldirs) $(DESTDIR)$(STATEDIR)
# FIXME: this really ought to use the autoconf directory variables

	for l in $(MANPAGE_LOCALES); do \
	  $(mkinstalldirs) $(DESTDIR)$(mandir)/$$l/man8; \
	  $(INSTALL_DATA) $(srcdir)/aptitude.$$l.8 $(DESTDIR)$(mandir)/$$l/man8/aptitude.8; \
	done

uninstall-local:
	rm -fr $(DESTDIR)$(STATEDIR)
# Is -r a good idea here?


ACLOCAL_AMFLAGS = -I m4
