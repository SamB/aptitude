SUBDIRS=cs de en fi fr ja po4a

EXTRA_DIST=aptitude-common.xsl aptitude-html.xsl aptitude-man.xsl aptitude-txt.style aptitude-txt.xsl aptitude.css

if USE_PO4A

dist distdir: po4a-dist

update-po: po4a-dist

po4a-dist:
	-@po4a -M utf-8 -k 0 po4a/po4a.cfg

endif
