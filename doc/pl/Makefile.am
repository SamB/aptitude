# automake doesn't play nicely with DocBook :(

# no full manual yet
SUBDIRS=
#SUBDIRS=images
EXTRA_DIST=fixman
LANGCODE=pl
MANPAGE=aptitude.$(LANGCODE).8
# How much of the manual should actually at least be translated.
PERCENT_TRANSLATED=0
MANPAGE_PERCENT_TRANSLATED=0
README=README.$(LANGCODE)
XMLSOURCES=aptitude.xml manpage.xml $(srcdir)/../aptitude-common.xsl

# Put documentation in /usr/share/doc/aptitude (by default)
docdir=$(datadir)/doc/$(PACKAGE)
localemandir=$(mandir)/$(LANGCODE)
htmldir=$(docdir)/html/$(LANGCODE)

HTML2TEXT=$(srcdir)/../html-to-text

IMAGES = $(wildcard $(srcdir)/images/*.png)

all-local: doc-stamp

clean-local:
	-rm -fr output-html/ output-txt/ output-man/
	-rm -f doc-stamp doc-css-stamp doc-html-stamp doc-txt-stamp doc-man-stamp
	-rm -fr $(MANPAGE) aptitude.8 $(README) *.tmp
	-rm -fr aptitude.xml manpage.xml

install-data-hook:
	$(mkinstalldirs) $(DESTDIR)$(localemandir)/man8
	$(INSTALL_DATA) $(MANPAGE) $(DESTDIR)$(localemandir)/man8/aptitude.8

# $(mkinstalldirs) $(htmldir)/images
# $(INSTALL_DATA) output-html/*.html output-html/*.css $(htmldir)
# $(INSTALL_DATA) output-html/images/* $(htmldir)/images
# $(INSTALL_DATA) $(srcdir)/$(README) $(DESTDIR)$(pkgdatadir)/$(README)

#doc-stamp: doc-html-stamp doc-css-stamp $(README) $(MANPAGE)
doc-stamp: $(MANPAGE)
	touch doc-stamp

aptitude.xml: $(srcdir)/../en/aptitude.xml $(srcdir)/../po4a/po/$(LANGCODE).po $(srcdir)/../po4a/add_$(LANGCODE)/addendum.1.$(LANGCODE)
	po4a-translate -k $(PERCENT_TRANSLATED) -M utf-8 -f docbook -m $(srcdir)/../en/aptitude.xml -p $(srcdir)/../po4a/po/$(LANGCODE).po -l aptitude.xml

manpage.xml: $(srcdir)/../en/manpage.xml $(srcdir)/../po4a/po/$(LANGCODE).po $(srcdir)/../po4a/add_$(LANGCODE)/addendum.1.$(LANGCODE)
	po4a-translate -k $(MANPAGE_PERCENT_TRANSLATED) -M utf-8 -f docbook -m $(srcdir)/../en/manpage.xml -p $(srcdir)/../po4a/po/$(LANGCODE).po -l manpage.xml

$(MANPAGE): $(XMLSOURCES) $(srcdir)/../aptitude-man.xsl
	-rm -fr output-man $(MANPAGE)
	xsltproc -o output-man/aptitude.8 $(srcdir)/../aptitude-man.xsl aptitude.xml
	mv output-man/aptitude.8 $(MANPAGE)
	/bin/bash $(srcdir)/fixman

$(README): $(XMLSOURCES)  $(srcdir)/../aptitude-txt.xsl
	-rm -fr output-txt
	xsltproc -o output-txt/index.html $(srcdir)/../aptitude-txt.xsl aptitude.xml
	$(HTML2TEXT) output-txt/index.html > $(README)

doc-css-stamp: doc-html-stamp $(srcdir)/../aptitude.css
	rm -f output-html/aptitude.css
	cp $(srcdir)/../aptitude.css output-html/
	touch doc-css-stamp

doc-html-stamp: $(XMLSOURCES) $(srcdir)/../aptitude-html.xsl $(IMAGES)
	-rm -fr output-html/

	xsltproc -o output-html/ $(srcdir)/../aptitude-html.xsl aptitude.xml

	mkdir output-html/images/
	ln -f $(srcdir)/images/*.png output-html/images/
	for x in caution important note tip warning; do ln -s /usr/share/xml/docbook/stylesheet/nwalsh/images/$$x.png output-html/images/; done
	for x in home next prev up; do ln -s /usr/share/xml/docbook/stylesheet/nwalsh/images/$$x.gif output-html/images/; done

	touch doc-html-stamp
