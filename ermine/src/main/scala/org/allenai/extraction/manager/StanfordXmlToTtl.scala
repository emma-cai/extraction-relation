package org.allenai.extraction.manager

import scala.xml.Node
import scala.xml.XML

import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream

/** Filter to convert stanford XML to TTL format. */
object StanfordXmlToTtl {
  def apply(input: InputStream, output: OutputStream): Unit = {
    val xml = XML.load(input)
    val outputPrinter = new PrintStream(output)
    printHeaders(outputPrinter)

    for (sentence <- (xml \\ "sentences" \ "sentence")) {
      processSentence(sentence, outputPrinter)
      outputPrinter.println
    }

    // TODO(jkinkead): Process coreferences.
  }

  /** A sentence in the stanford output consists of tokens, "basic" dependencies, and "collapsed"
    * dependencies.
    */
  def processSentence(sentence: Node, output: PrintStream): Unit = {
    // We need a sentence ID to proceed.
    for (idAttr <- sentence \ "@id") {
      val sentenceId = idAttr.text

      for (token <- sentence \ "tokens" \ "token") {
        for (line <- processToken(sentenceId, token)) {
          output.println(line)
        }
        output.println
      }

      for {
        dependencyList <- sentence \ "dependencies"
        depType <- dependencyList \ "@type"
        label <- depType.text match {
          case "basic-dependencies" => Some("basic")
          case "collapsed-ccprocessed-dependencies" => Some("dep")
          // We ignore other dependencies (i.e. "collapsed-dependencies")
          case _ => None
        }
      } {
        for {
          dependency <- dependencyList \ "dep"
        } {
          for (line <- processDependency(sentenceId, dependency, label)) {
            output.println(line)
          }
        }

        output.println
      }
    }
  }

  /** Processes a token element, with a format like:
    *
    * <token id="3"> 
    *   <word>grow</word> 
    *   <lemma>grow</lemma> 
    *   <CharacterOffsetBegin>13</CharacterOffsetBegin> 
    *   <CharacterOffsetEnd>17</CharacterOffsetEnd> 
    *   <POS>VB</POS> 
    *   <NER>O</NER> 
    * </token> 
    *
    * and returns output like:
    *
    * :1.3 token:text "grow" .
    * :1.3 token:lemma "grow" .
    * :1.3 token:pos "VB" .
    * :1.3 token:begin 13 .
    * :1.3 token:end 17 . 
    *
    * with the first number being the provided sentence ID.
    */
  def processToken(sentenceId: String, token: Node): Seq[String] = {
    (for (idAttr <- token \ "@id") yield {
      val tokenId = s":${sentenceId}.${idAttr.text}"
      // Start of each token declaration.
      val tokenPrefix = s"${tokenId} token"
      val words = childrenToTtl(token, "word", tokenPrefix, "text", quoteValue = true)
      val lemmas = childrenToTtl(token, "lemma", tokenPrefix, "lemma", quoteValue = true)
      val posTags = childrenToTtl(token, "POS", tokenPrefix, "pos", quoteValue = true)
      val begins = childrenToTtl(token, "CharacterOffsetBegin", tokenPrefix, "begin",
        quoteValue = false)
      val ends = childrenToTtl(token, "CharacterOffsetEnd", tokenPrefix, "end", quoteValue = false)

      // Ignore NER=O, otherwise print NE stuff.
      val nerText = ((token \ "NER").headOption map { _.text }).getOrElse("O")
      val nes: Seq[String] = nerText match {
        case "O" => Seq.empty
        case _: String => {
          val nePrefix = s"${tokenId} ne"
          // Process type and normalization.
          Seq(childrenToTtl(token, "NER", nePrefix, "type", quoteValue = true),
            childrenToTtl(token, "NormalizedNER", nePrefix, "norm", quoteValue = true)).flatten
        }
        case _ => Seq.empty
      }

      Seq.concat(words, lemmas, posTags, begins, ends, nes)
    }).flatten
  }

  /** Processes a dependency element, with a format like:
    *
    * <dep type="det"> 
    *   <governor idx="2">animals</governor>
    *   <dependent idx="1">Some</dependent>
    * </dep>
    *
    * and returns output like:
    *
    * :1.2 basic:det :1.1 .
    *
    * with the first number in both labels being the provided sentence ID, and the first part of
    * the middle label being the given dependencyLabel.
    */
  def processDependency(sentenceId: String, dependency: Node, dependencyLabel: String):
    Seq[String] = {
    for (typeAttr <- dependency \ "@type") yield {
      // The format is "governorId labelType dependentId".
      // We take the first value found for each item.
      val governorId = (dependency \ "governor" \ "@idx").head
      val dependentId = (dependency \ "dependent" \ "@idx").head

      s":${sentenceId}.${governorId} ${dependencyLabel}:${typeAttr} :${sentenceId}.${dependentId} ."
    }
  }

  /** Prints declaration headers. */
  def printHeaders(outputPrinter: PrintStream): Unit = {
    outputPrinter.print("""
@prefix : <http://halo.vulcan.com/id#> .

@prefix token: <http://nlp.stanford.edu/token/> .
@prefix ne: <http://nlp.stanford.edu/ne/> .
@prefix basic: <http://nlp.stanford.edu/basic/> .
@prefix dep: <http://nlp.stanford.edu/dep/> .
@prefix coref: <http://nlp.stanford.edu/coref/> .

""")
  }

  /** Parses all child nodes with a given name out of a node, and returns them as ttl statements.
    *
    * @param node the parent node to look in
    * @param childName the name of the child node to look for
    * @param prefix the declaration prefix
    * @param outputName the output name
    * @param quoteValue if true, quote the child node's value
    */
  def childrenToTtl(node: Node, childName: String, prefix: String, outputName: String,
    quoteValue: Boolean): Seq[String] = {
    for {
      child <- node \ childName
      value = child.text
    } yield if (quoteValue) {
      s"""${prefix}:${outputName} "${value}" ."""
    } else {
      s"${prefix}:${outputName} ${value} ."
    }
  }
}
