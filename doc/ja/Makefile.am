# automake doesn't play nicely with DocBook :(

SUBDIRS=images
EXTRA_DIST=aptitude.xml manpage.xml

LANGCODE=ja
MANPAGE=aptitude.$(LANGCODE).8
# Automake automagically distributes README without asking..
README=README.$(LANGCODE)
XMLSOURCES=aptitude.xml manpage.xml

# Put documentation in /usr/share/doc/aptitude (by default)
docdir=$(datadir)/doc/$(PACKAGE)
localemandir=$(mandir)/$(LANGCODE)
htmldir=$(docdir)/html/$(LANGCODE)

IMAGES = $(wildcard images/*.png)

all-local: doc-stamp

clean-local:
	-rm -fr output-html/ output-txt/ output-man/
	-rm -f doc-stamp doc-css-stamp doc-html-stamp doc-txt-stamp doc-man-stamp
	-rm -fr $(MANPAGE) $(README)

install-data-hook:
	$(mkinstalldirs) $(DESTDIR)$(localemandir)/man8
	$(INSTALL_DATA) $(MANPAGE) $(DESTDIR)$(localemandir)/man8/aptitude.8

#	$(mkinstalldirs) $(htmldir)/images
#	$(INSTALL_DATA) output-html/*.html output-html/*.css $(htmldir)
#	$(INSTALL_DATA) output-html/images/* $(htmldir)/images

#	$(INSTALL_DATA) $(README) $(DESTDIR)$(pkgdatadir)/$(README)

doc-stamp: doc-html-stamp doc-css-stamp $(README) $(MANPAGE)
	touch doc-stamp

db2latex: doc-db2latex-stamp

$(MANPAGE): $(XMLSOURCES) aptitude-man-$(LANGCODE).xsl $(srcdir)/../aptitude-man.xsl
	-rm -fr output-man
	xsltproc -o output-man/aptitude.8 aptitude-man-$(LANGCODE).xsl $(srcdir)/aptitude.xml
	mv output-man/aptitude.8 $(MANPAGE)

$(README): $(XMLSOURCES) $(srcdir)/../aptitude-txt.xsl $(srcdir)/../aptitude-common.xsl $(srcdir)/../aptitude-txt.style
	-rm -fr output-txt/
	xsltproc -o output-txt/index.html $(srcdir)/../aptitude-txt.xsl $(srcdir)/aptitude.xml
	html2text -width 80 -ascii -nobs -rcfile $(srcdir)/../aptitude-txt.style output-txt/index.html | sed 's/&#8212/--/g' > $(README)

doc-db2latex-stamp: $(XMLSOURCES) $(srcdir)/../aptitude-db2latex.xsl $(srcdir)/../aptitude-common.xsl $(IMAGES)
	-rm -fr output-db2latex
	mkdir output-db2latex
	ln -s ../images output-db2latex/images
	xsltproc -o output-db2latex/aptitude.tex $(srcdir)/../aptitude-db2latex.xsl $(srcdir)/aptitude.xml
# Run pdflatex 3 times just to be extra sure that it converges.
	cd output-db2latex && pdflatex -interaction nonstopmode aptitude.tex && pdflatex -interaction nonstopmode aptitude.tex && pdflatex -interaction nonstopmode aptitude.tex
	touch doc-db2latex-stamp

doc-css-stamp: doc-html-stamp $(srcdir)/../aptitude.css
	rm -f output-html/aptitude.css
	cp $(srcdir)/../aptitude.css output-html/
	touch doc-css-stamp

doc-html-stamp: $(XMLSOURCES) aptitude-html-$(LANGCODE).xsl $(srcdir)/../aptitude-common.xsl $(IMAGES)
	-rm -fr output-html/

	xsltproc -o output-html/ aptitude-html-$(LANGCODE).xsl $(srcdir)/aptitude.xml

	mkdir output-html/images/
	ln -f $(srcdir)/images/*.png output-html/images/
	for x in caution important note tip warning; do ln -s /usr/share/xml/docbook/stylesheet/nwalsh/images/$$x.png output-html/images/; done
	for x in home next prev up; do ln -s /usr/share/xml/docbook/stylesheet/nwalsh/images/$$x.gif output-html/images/; done

	touch doc-html-stamp
