# automake doesn't play nicely with DocBook :(

SUBDIRS=images

LANGCODE=es
MANPAGE=aptitude.$(LANGCODE).8
# How much of the manual should actually at least be translated.
PERCENT_TRANSLATED=0 # status: 0.75%
MANPAGE_PERCENT_TRANSLATED=0 # status: 31%
README=README.$(LANGCODE)
XMLSOURCES=aptitude.xml manpage.xml $(srcdir)/../aptitude-common.xsl

# Put documentation in /usr/share/doc/aptitude (by default)
docdir=$(datadir)/doc/$(PACKAGE)
localemandir=$(mandir)/$(LANGCODE)
htmldir=$(docdir)/html/$(LANGCODE)

IMAGES = $(wildcard $(srcdir)/images/*.png)

all-local: doc-stamp

clean-local:
	-rm -fr output-html/ output-txt/ output-man/
	-rm -f doc-stamp doc-css-stamp doc-html-stamp doc-txt-stamp doc-man-stamp
	-rm -fr $(MANPAGE) $(README) *.tmp
	-rm -fr aptitude.xml manpage.xml

install-data-hook:
	$(mkinstalldirs) $(DESTDIR)$(localemandir)/man8
	$(INSTALL_DATA) $(MANPAGE) $(DESTDIR)$(localemandir)/man8/aptitude.8
	$(mkinstalldirs) $(DESTDIR)$(htmldir)/images
	$(INSTALL_DATA) output-html/*.html output-html/*.css $(DESTDIR)$(htmldir)
	$(INSTALL_DATA) output-html/images/* $(htmldir)/images

	$(INSTALL_DATA) README.$(LANGCODE) $(DESTDIR)$(pkgdatadir)/README.$(LANGCODE)

doc-stamp: doc-html-stamp doc-css-stamp $(README) $(MANPAGE)
	touch doc-stamp

aptitude.xml: $(srcdir)/../en/aptitude.xml $(srcdir)/../po4a/po/$(LANGCODE).po $(srcdir)/../po4a/add_$(LANGCODE)/addendum.1.$(LANGCODE)
	po4a-translate -k $(PERCENT_TRANSLATED) -M utf-8 -f docbook -m $(srcdir)/../en/aptitude.xml -p $(srcdir)/../po4a/po/$(LANGCODE).po -l aptitude.xml

manpage.xml: $(srcdir)/../en/manpage.xml $(srcdir)/../po4a/po/$(LANGCODE).po $(srcdir)/../po4a/add_$(LANGCODE)/addendum.1.$(LANGCODE)
	po4a-translate -k $(MANPAGE_PERCENT_TRANSLATED) -M utf-8 -f docbook -m $(srcdir)/../en/manpage.xml -p $(srcdir)/../po4a/po/$(LANGCODE).po -l manpage.xml

$(MANPAGE): $(XMLSOURCES) $(srcdir)/../aptitude-man.xsl
	-rm -fr output-man $(MANPAGE)
	xsltproc -o output-man/aptitude.8 $(srcdir)/../aptitude-man.xsl aptitude.xml
	mv output-man/aptitude.8 $(MANPAGE)

$(README): $(XMLSOURCES)  $(srcdir)/../aptitude-txt.xsl $(srcdir)/../aptitude-txt.style
	-rm -fr output-txt
	xsltproc -o output-txt/index.html $(srcdir)/../aptitude-txt.xsl aptitude.xml
	html2text -width 80 -ascii -nobs -rcfile $(srcdir)/../aptitude-txt.style output-txt/index.html | $(srcdir)/../fixup-text > $(README)

doc-css-stamp: doc-html-stamp $(srcdir)/../aptitude.css
	rm -f output-html/aptitude.css
	cp $(srcdir)/../aptitude.css output-html/
	touch doc-css-stamp

doc-html-stamp: $(XMLSOURCES) $(srcdir)/../aptitude-html.xsl $(IMAGES)
	-rm -fr output-html/

	xsltproc -o output-html/ $(srcdir)/../aptitude-html.xsl aptitude.xml

	mkdir output-html/images/
	ln -f $(srcdir)/images/*.png output-html/images/
	set -e; for x in $(srcdir)/images/*.svg; do rsvg-convert -f png -o output-html/images/$$(basename $${x%.svg}.png) $$x; done
	for x in caution important note tip warning; do ln -s /usr/share/xml/docbook/stylesheet/nwalsh/images/$$x.png output-html/images/; done
	for x in home next prev up; do ln -s /usr/share/xml/docbook/stylesheet/nwalsh/images/$$x.gif output-html/images/; done

	touch doc-html-stamp
