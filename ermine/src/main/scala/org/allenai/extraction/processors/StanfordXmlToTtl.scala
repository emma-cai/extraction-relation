package org.allenai.extraction.processors

import org.allenai.extraction.FlatProcessor

import scala.io.Source
import scala.xml.Node
import scala.xml.XML

import java.io.PrintWriter
import java.io.Writer

/** Filter to convert stanford XML to TTL format. */
object StanfordXmlToTtl extends FlatProcessor {
  /** Converts a stanford XML parse tree into TTL.  Reads a single XML file, and writes output to a
    * TTL file.
    */
  override protected def processInternal(stanfordXml: Source, ttlOut: Writer): Unit = {
    val xml = XML.loadString(stanfordXml.getLines.mkString)

    val outputPrinter = new PrintWriter(ttlOut)
    outputPrinter.print(Ttl.NamespaceHeaders)

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
  def processSentence(sentence: Node, output: PrintWriter): Unit = {
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
    * <mention representative="true">
    * <sentence>2</sentence>
    * <head>4</head>
    * </mention>
    * <mention>
    * <sentence>5</sentence>
    * <head>10</head>
    * </mention>
    * </coreference>
    *
    * and prints output like:
    *
    * id:2.4 coref:ref id:2.4 .
    * id:2.4 coref:ref id:5.10 .
    */
  def processCoreference(coreference: Node, output: PrintWriter): Unit = {
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
    * <token id="3">
    * <word>grow</word>
    * <lemma>grow</lemma>
    * <CharacterOffsetBegin>13</CharacterOffsetBegin>
    * <CharacterOffsetEnd>17</CharacterOffsetEnd>
    * <POS>VB</POS>
    * <NER>O</NER>
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
      val tokenLines = (for {
        string <- getChildText(token, "word")
        lemma <- getChildText(token, "lemma")
        posTag <- getChildText(token, "POS")
        offsetBegin <- getChildText(token, "CharacterOffsetBegin")
        offsetEnd <- getChildText(token, "CharacterOffsetEnd")
      } yield Seq(
        s"""${tokenId} token:text  "${string}"     .""",
        s"""${tokenId} token:lemma "${lemma}"      .""",
        s"""${tokenId} token:pos   "${posTag}"     .""",
        s"""${tokenId} token:begin  ${offsetBegin} .""",
        s"""${tokenId} token:end    ${offsetEnd}   .""")).getOrElse(Seq.empty)

      // Ignore NER=O, otherwise print NE stuff.
      val nerText = getChildText(token, "NER").getOrElse("O")
      val neLines: Seq[String] = nerText match {
        case "O" => Seq.empty
        case _: String => {
          // Process type and normalization.
          val typeLine = Some(s"""${tokenId} ne:type "${nerText}" .""")
          val normLine = for {
            norm <- getChildText(token, "NormalizedNER")
          } yield {
            s"""${tokenId} ne:norm "${norm}" ."""
          }

          Seq(typeLine, normLine).flatten
        }
        case _ => Seq.empty
      }

      Seq.concat(tokenLines, neLines)
    }).flatten
  }

  /** Processes a dependency element, with a format like:
    *
    * <dep type="det">
    * <governor idx="2">animals</governor>
    * <dependent idx="1">Some</dependent>
    * </dep>
    *
    * and returns output like:
    *
    * id:1.2 basic:det id:1.1 .
    *
    * with the first number in both labels being the provided sentence ID, and the first part of
    * the middle label being the given dependencyLabel.
    */
  def processDependency(sentenceId: String, dependency: Node,
    dependencyLabel: String): Seq[String] = {
    for (typeAttr <- dependency \ "@type") yield {
      // The format is "governorId labelType dependentId".
      // We take the first value found for each item.
      val governorId = (dependency \ "governor" \ "@idx").head
      val dependentId = (dependency \ "dependent" \ "@idx").head

      s"id:${sentenceId}.${governorId} ${dependencyLabel}:${typeAttr} " +
        s"id:${sentenceId}.${dependentId} ."
    }
  }

  /** Gets the text contents of the first child node with the given name. */
  def getChildText(node: Node, childName: String): Option[String] = {
    (node \ childName).headOption map { _.text }
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
