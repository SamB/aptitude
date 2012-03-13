<?xml version="1.0" encoding="utf-8"?>

<!-- Magic: -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                version="1.0">

<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/html/docbook.xsl"/>

<xsl:import href="aptitude-common.xsl"/>

<xsl:param name="preferred.mediaobject.role">text</xsl:param>

<!-- Force DocBook to use ASCII durnit -->
<xsl:param name='local.l10n.xml' select='document("")'/>
<l:i18n xmlns:l="http://docbook.sourceforge.net/xmlns/l10n/1.0"> 
  <l:l10n language="en">
    <l:context name='xref-number-and-title'>
      <l:template name="section" text="the section called ``%t''"/>
      <l:template name="sect1" text="the section called ``%t''"/>
      <l:template name="sect2" text="the section called ``%t''"/>
      <l:template name="sect3" text="the section called ``%t''"/>
      <l:template name="sect4" text="the section called ``%t''"/>
      <l:template name="sect5" text="the section called ``%t''"/>
      <l:template name="figure" text="Figure&#160;%n, ``%t''"/>
    </l:context>

    <l:context name='xref'>
      <l:template name="section" text="the section called ``%t''"/>
      <l:template name="sect1" text="the section called ``%t''"/>
      <l:template name="sect2" text="the section called ``%t''"/>
      <l:template name="sect3" text="the section called ``%t''"/>
      <l:template name="sect4" text="the section called ``%t''"/>
      <l:template name="sect5" text="the section called ``%t''"/>
    </l:context>


    <!-- Apparently I can't do anything about an em-dash appearing
	 in the refentry. :(
    -->

    <l:dingbat key="startquote" text="``"/>
    <l:dingbat key="endquote" text="''"/>

    <l:dingbat key="nestedstartquote" text="`"/>
    <l:dingbat key="nestedendquote" text="'"/>

    <l:dingbat key="singlestartquote" text="`"/>
    <l:dingbat key="singleendquote" text="'"/>

    <l:dingbat key="bullet" text="*"/>
  </l:l10n>
</l:i18n>

</xsl:stylesheet>
