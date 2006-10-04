<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                version="1.0">
<xsl:import href="../aptitude-man.xsl"/>

<!-- override settings in
     /usr/share/xml/docbook/stylesheet/nwalsh/manpages/docbook.xsl. -->
<xsl:param name="man.output.encoding" select="'euc-jp'"/>
<xsl:output method="text"
            encoding="euc-jp"
	    indent="no"/>

<!-- override templates in
     /usr/share/xml/docbook/stylesheet/nwalsh/manpages/docbook.xsl. -->
<xsl:template match="refnamediv">
  <xsl:text>.SH 名前&#10;</xsl:text>
  <xsl:for-each select="refname">
    <xsl:if test="position()>1">
      <xsl:text>, </xsl:text>
    </xsl:if>
    <xsl:value-of select="."/>
  </xsl:for-each>
  <xsl:text> \- </xsl:text>
  <xsl:value-of select="normalize-space (refpurpose)"/>
</xsl:template>

<xsl:template match="refsynopsisdiv">
  <xsl:text>&#10;.SH "書式"&#10;</xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="note">
  <xsl:text>&#10;.RS&#10;.Sh "注意</xsl:text>
  <xsl:if test="title">
    <xsl:text>: </xsl:text>
    <xsl:value-of select="title[1]"/>
  </xsl:if>
  <xsl:text>"&#10;</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&#10;.RE&#10;</xsl:text>
</xsl:template>

<xsl:template match="articleinfo|bookinfo|refentryinfo" mode="authorsect">
  <xsl:text>.SH 著者</xsl:text>
  <xsl:if test="count(.//author)>1">
    <xsl:text>S</xsl:text>
  </xsl:if>
  <xsl:text>&#10;</xsl:text>

  <xsl:for-each select=".//author">
    <xsl:if test="position() > 1">
      <xsl:text>, </xsl:text>
    </xsl:if>
    <xsl:variable name="author">
      <xsl:apply-templates select="."/>
    </xsl:variable>
    <xsl:value-of select="normalize-space($author)"/>
  </xsl:for-each>
  <xsl:text>.&#10;</xsl:text>
  <xsl:if test=".//editor">
    <xsl:text>.br&#10;Man page edited by </xsl:text>
    <xsl:apply-templates select=".//editor"/>
    <xsl:text>.&#10;</xsl:text>
  </xsl:if>
</xsl:template>

</xsl:stylesheet>
