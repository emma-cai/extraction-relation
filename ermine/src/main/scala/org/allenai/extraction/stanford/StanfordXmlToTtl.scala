package org.allenai.extraction.stanford

import scala.xml.Node
import scala.xml.XML

import java.io.Reader
import java.io.OutputStream
import java.io.PrintStream

/** Filter to convert stanford XML to TTL format. */
object StanfordXmlToTtl {
  def apply(input: Reader, output: OutputStream): Unit = {
    val xml = XML.load(input)
    val outputPrinter = new PrintStream(output)
    printHeaders(outputPrinter)

    for (sentence <- (xml \\ "sentences" \ "sentence")) {
      processSentence(sentence, outputPrinter)
      outputPrinter.println
    }

    // Coreferences are stored in a 'coreference' element; this is not a typo.
    for (coreference <- (xml \\ "coreference" \ "coreference")) {
      processCoreference(coreference, outputPrinter)
      outputPrinter.println
    }
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


  /** Process a coreference element, with a format like:
    *
    * <coreference>
    *   <mention representative="true">
    *     <sentence>2</sentence>
    *     <head>4</head>
    *   </mention>
    *   <mention>
    *     <sentence>5</sentence>
    *     <head>10</head>
    *   </mention>
    * </coreference>
    *
    * and prints output like:
    *
    * id:2.4 coref:ref id:2.4 .
    * id:2.4 coref:ref id:5.10 .
    */
  def processCoreference(coreference: Node, output: PrintStream): Unit = {
    // Find the representative mention's ID.
    val rootIdOption: Option[String] = {
      val representativeMention = (coreference \ "mention") find { mention =>
        ((mention \ "@representative").headOption map { _.text }) == Some("true")
      }

      representativeMention match {
        case Some(node) => getMentionId(node)
        case None => None
      }
    }

    rootIdOption map { rootId =>
      for {
        mention <- coreference \ "mention"
        mentionId <- getMentionId(mention)
      } {
        output.println(s"${rootId} coref:ref ${mentionId} .")
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
    * id:1.3 token:text "grow" .
    * id:1.3 token:lemma "grow" .
    * id:1.3 token:pos "VB" .
    * id:1.3 token:begin 13 .
    * id:1.3 token:end 17 .
    *
    * with the first number being the provided sentence ID.
    */
  def processToken(sentenceId: String, token: Node): Seq[String] = {
    (for (idAttr <- token \ "@id") yield {
      val tokenId = s"id:${sentenceId}.${idAttr.text}"
      // Start of each token declaration.
      val words = childrenToTtl(token, "word", tokenId, "token:text", quoteValue = true)
      val lemmas = childrenToTtl(token, "lemma", tokenId, "token:lemma", quoteValue = true)
      val posTags = childrenToTtl(token, "POS", tokenId, "token:pos", quoteValue = true)
      val begins = childrenToTtl(token, "CharacterOffsetBegin", tokenId, "token:begin",
        quoteValue = false)
      val ends = childrenToTtl(token, "CharacterOffsetEnd", tokenId, "token:end",
        quoteValue = false)

      // Ignore NER=O, otherwise print NE stuff.
      val nerText = ((token \ "NER").headOption map { _.text }).getOrElse("O")
      val nes: Seq[String] = nerText match {
        case "O" => Seq.empty
        case _: String => {
          // Process type and normalization.
          Seq(childrenToTtl(token, "NER", tokenId, "ne:type", quoteValue = true),
            childrenToTtl(token, "NormalizedNER", tokenId, "ne:norm", quoteValue = true)).flatten
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
    * id:1.2 basic:det id:1.1 .
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

      s"id:${sentenceId}.${governorId} ${dependencyLabel}:${typeAttr} " +
        s"id:${sentenceId}.${dependentId} ."
    }
  }

  /** Prints declaration headers. */
  def printHeaders(outputPrinter: PrintStream): Unit = {
    outputPrinter.print("""
@prefix id: <http://aristo.allenai.org/id#> .

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
    * @param tokenId the ID of the token
    * @param outputName the output name
    * @param quoteValue if true, quote the child node's value
    */
  def childrenToTtl(node: Node, childName: String, tokenId: String, outputName: String,
    quoteValue: Boolean): Seq[String] = {
    for {
      child <- node \ childName
      value = child.text
    } yield if (quoteValue) {
      s"""${tokenId} ${outputName} "${value}" ."""
    } else {
      s"${tokenId} ${outputName} ${value} ."
    }
  }

  /** Returns the ID for a given mention element. This pulls the values of the 'sentence' and 'head'
    * child nodes for the ID.
    */
  def getMentionId(mention: Node): Option[String] = {
    val sentenceIdOption = (mention \ "sentence").headOption map { _.text }
    val headIdOption = (mention \ "head").headOption map { _.text }
    for {
      sentenceId <- sentenceIdOption
      headId <- headIdOption
    } yield s"id:${sentenceId}.${headId}"
  }
}
