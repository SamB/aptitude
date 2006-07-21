<?xml version="1.0" encoding="utf-8"?>

<!-- Magic: -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                version="1.0">

<xsl:import href="/usr/share/xml/docbook/stylesheet/db2latex/latex/docbook.xsl"/>

<xsl:import href="aptitude-common.xsl"/>

<xsl:variable name="insert.xref.page.number">1</xsl:variable>

<xsl:variable name='latex.use.ltxtable'>1</xsl:variable>

<xsl:variable name="admon.graphics.path">/usr/share/xml/docbook/stylesheet/db2latex/latex/figures</xsl:variable>

<xsl:variable name="latex.book.preamble.post">\usepackage[dvips]{geometry}\geometry{papersize={6in,9in}}</xsl:variable>

<!-- <xsl:variable name="latex.book.preamble.post">\setlength{\pdfpagewidth}{6in}\setlength{\pdfpageheight}{9in}</xsl:variable> -->
	<xsl:template match="imagedata" name="imagedata">
		<xsl:param name="filename">
			<xsl:choose>
				<xsl:when test="@entityref">
					<xsl:value-of select="unparsed-entity-uri(@entityref)"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="@fileref"/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:param>
		<xsl:param name="is.imageobjectco" select="false()"/>
		<xsl:variable name="width">
			<xsl:choose>
				<xsl:when test="contains(@width, '%') and substring-after(@width, '%')=''">
					<xsl:value-of select="number(substring-before(@width, '%')) div 100"/>
					<xsl:text>\textwidth</xsl:text>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="@width"/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		<xsl:if test="$width!='' and (@scalefit='0' or count(@scale)&gt;0)">
			<xsl:text>\makebox[</xsl:text><xsl:value-of select='$width' /><xsl:text>]</xsl:text>
		</xsl:if>
		<!-- TODO this logic actually needs to make decisions based on the ALLOWED imagedata,
		not all the imagedata present in the source file. -->
		<xsl:choose>
			<xsl:when test="$is.imageobjectco=1">
				<xsl:text>{\begin{overpic}[</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>{\noindent\includegraphics[</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:choose>
			<xsl:when test="@scale"> 
			<xsl:text>scale=</xsl:text>
			<xsl:value-of select="number(@scale) div 100"/>
			</xsl:when>
			<xsl:when test="$width!='' and @scalefit='1'">
			<xsl:text>width=</xsl:text><xsl:value-of select="normalize-space($width)"/>
			</xsl:when>
			<xsl:when test="@depth!='' and @scalefit='1'">
			<xsl:text>height=</xsl:text><xsl:value-of select="normalize-space(@depth)"/>
			</xsl:when>
		</xsl:choose>
		<xsl:choose>
			<xsl:when test="@format = 'PRN'"><xsl:text>,angle=270</xsl:text></xsl:when>
		</xsl:choose>
		<xsl:text>]{</xsl:text>
		<xsl:value-of select="$filename"/>
		<xsl:choose>
			<xsl:when test="$is.imageobjectco=1">
				<xsl:text>}&#10;\calsscale&#10;</xsl:text>
				<xsl:apply-templates select="ancestor::imageobjectco/areaspec//area"/>
				<xsl:text>\end{overpic}}</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>}}</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

</xsl:stylesheet>
