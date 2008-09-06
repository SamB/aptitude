# # automake doesn't play nicely with DocBook :(
# yup. :-)

# no full manual yet
SUBDIRS=
#SUBDIRS=images
EXTRA_DIST=aptitude.xml manpage.xml fixquotes

LANGCODE=de
MANPAGE=aptitude.$(LANGCODE).8
README=README.$(LANGCODE)
XMLSOURCES=aptitude.xml manpage.xml ../aptitude-common.xsl

# Put documentation in /usr/share/doc/aptitude (by default)
docdir=$(datadir)/doc/$(PACKAGE)
localemandir=$(mandir)/$(LANGCODE)
htmldir=$(docdir)/html/$(LANGCODE)

IMAGES = $(wildcard images/*.png)

all-local: doc-stamp

clean-local:
	-rm -fr output-html/ output-txt/ output-man/
	-rm -f doc-stamp doc-css-stamp doc-html-stamp doc-txt-stamp doc-man-stamp
	-rm -fr $(MANPAGE) aptitude.8 $(README) *.tmp

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

$(MANPAGE): $(XMLSOURCES) ../aptitude-man.xsl
	-rm -fr output-man $(MANPAGE)
	xsltproc -o output-man/aptitude.8 $(srcdir)/../aptitude-man.xsl $(srcdir)/aptitude.xml
	mv output-man/aptitude.8 $(MANPAGE)
	/bin/bash $(srcdir)/fixquotes

$(README): $(XMLSOURCES)  $(srcdir)/../aptitude-txt.xsl $(srcdir)/../aptitude-txt.style
	-rm -fr output-txt
	xsltproc -o output-txt/index.html $(srcdir)/../aptitude-txt.xsl $(srcdir)/aptitude.xml
	html2text -width 80 -ascii -nobs -rcfile $(srcdir)/../aptitude-txt.style output-txt/index.html | $(srcdir)/../doc/fixup-text > $(README)

doc-css-stamp: doc-html-stamp $(srcdir)/../aptitude.css
	rm -f output-html/aptitude.css
	cp $(srcdir)/../aptitude.css output-html/
	touch doc-css-stamp

doc-html-stamp: $(XMLSOURCES) $(srcdir)/../aptitude-html.xsl $(IMAGES)
	-rm -fr output-html/

	xsltproc -o output-html/ $(srcdir)/../aptitude-html.xsl $(srcdir)/aptitude.xml

	mkdir output-html/images/
	ln -f $(srcdir)/images/*.png output-html/images/
	for x in caution important note tip warning; do ln -s /usr/share/xml/docbook/stylesheet/nwalsh/images/$$x.png output-html/images/; done
	for x in home next prev up; do ln -s /usr/share/xml/docbook/stylesheet/nwalsh/images/$$x.gif output-html/images/; done

	touch doc-html-stamp

