<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns:d="http://nlp.stanford.edu/CoreNLP/v1">

<xsl:output method="text"/>

<xsl:template match="/">
@prefix id: &lt;http://aristo.allenai.org/id#<xsl:value-of select="$filename"/>&gt; .
<xsl:text>
@prefix token: &lt;http://nlp.stanford.edu/token/&gt; .
@prefix ne: &lt;http://nlp.stanford.edu/ne/&gt; .
@prefix basic: &lt;http://nlp.stanford.edu/basic/&gt; .
@prefix dep: &lt;http://nlp.stanford.edu/dep/&gt; .
@prefix coref: &lt;http://nlp.stanford.edu/coref/&gt; .
</xsl:text>
  <xsl:for-each select="root/document/sentences/sentence">
    <xsl:apply-templates select=".">
      <xsl:with-param name="position" select="@id"/>
    </xsl:apply-templates>
  </xsl:for-each>
  
  <xsl:apply-templates select="root/document/coreference"/>
<xsl:text>
</xsl:text>
</xsl:template>

<xsl:template match="root/document/sentences/sentence">
  <xsl:param name="position" select="'0'"/>
<!--
/*
Sentence #<xsl:value-of select="$position"/><xsl:text>
</xsl:text>
  <xsl:value-of select="parse"/>
*/
-->

  <xsl:apply-templates select="tokens">
    <xsl:with-param name="position" select="$position"/>
  </xsl:apply-templates>

  <xsl:for-each select="dependencies[@type='basic-dependencies']">
    <xsl:text>
</xsl:text>
    <xsl:apply-templates select="dep">
      <xsl:with-param name="position" select="$position"/>
      <xsl:with-param name="prefix" select="'basic'"/>
    </xsl:apply-templates>
  </xsl:for-each>

  <xsl:for-each select="dependencies[@type='collapsed-ccprocessed-dependencies']">
    <xsl:text>
</xsl:text>
    <xsl:apply-templates select="dep">
      <xsl:with-param name="position" select="$position"/>
      <xsl:with-param name="prefix" select="'dep'"/>
    </xsl:apply-templates>
  </xsl:for-each>
</xsl:template>

<xsl:template match="tokens">
  <xsl:param name="position" select="'0'"/>
  <xsl:for-each select="token">
:<xsl:value-of select="$position"/>.<xsl:value-of select="@id"/> token:text "<xsl:value-of select="translate(word, '\\', '')"/>" .
:<xsl:value-of select="$position"/>.<xsl:value-of select="@id"/> token:lemma "<xsl:value-of select="translate(lemma, '\\', '')"/>" .
:<xsl:value-of select="$position"/>.<xsl:value-of select="@id"/> token:pos "<xsl:value-of select="POS"/>" .
:<xsl:value-of select="$position"/>.<xsl:value-of select="@id"/> token:begin <xsl:value-of select="CharacterOffsetBegin"/> .
:<xsl:value-of select="$position"/>.<xsl:value-of select="@id"/> token:end <xsl:value-of select="CharacterOffsetEnd"/> .
<xsl:if test="not(NER='O')">:<xsl:value-of select="$position"/>.<xsl:value-of select="@id"/> ne:type "<xsl:value-of select="NER"/>" .
<xsl:if test="NormalizedNER">:<xsl:value-of select="$position"/>.<xsl:value-of select="@id"/> ne:norm "<xsl:value-of select="NormalizedNER"/>" .
</xsl:if>
</xsl:if>
</xsl:for-each>
</xsl:template>

<xsl:template match="dep">
  <xsl:param name="position" select="'0'"/><xsl:param name="prefix" select="'1'"/>:<xsl:value-of select="$position"/>.<xsl:value-of select="governor/@idx"/><xsl:text> </xsl:text><xsl:value-of select="$prefix"/>:<xsl:value-of select="translate(@type, '\\/', '_')"/> :<xsl:value-of select="$position"/>.<xsl:value-of select="dependent/@idx"/> .
</xsl:template>

<xsl:template match="coreference">
  <xsl:for-each select="coreference">
    <xsl:variable name="gov">
      <xsl:value-of select='mention[@representative="true"]/sentence'/>.<xsl:value-of select='mention[@representative="true"]/head'/>
    </xsl:variable>
    <xsl:for-each select="mention">
:<xsl:value-of select="$gov"/> coref:ref :<xsl:value-of select="sentence"/>.<xsl:value-of select="head"/> .</xsl:for-each>
  </xsl:for-each>
</xsl:template>

</xsl:stylesheet>
