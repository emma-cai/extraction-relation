package org.allenai.extraction.processors

import org.allenai.extraction.MultiTextProcessor

import edu.stanford.nlp.dcoref.{ CorefChain, CorefCoreAnnotations }
import edu.stanford.nlp.ling.{ CoreAnnotations, CoreLabel }
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.semgraph.{ SemanticGraph, SemanticGraphCoreAnnotations }
import edu.stanford.nlp.trees.GrammaticalRelation

import scala.collection.JavaConverters._
import scala.io.Source

import java.io.Writer
import java.util.Properties

/** Wrapper around stanford parser, with static configs. Note that this takes a substantial (more
  * than 10 seconds) time to construct, and uses about 600MB of memory.
  *
  * This takes a text file as input and runs the parser on the entire document. Output is Ari TTL.
  */
// TODO(jkinkead): Port this to use tinkerpop to build an RDF graph, instead of writing out TTL by
// hand.
object StanfordTtl extends MultiTextProcessor {
  /** Escapes all quotes and backslashes in a string. */
  def escapeString(string: String): String = {
    string.replace("\\", "\\\\").replace("\"", "\\\"")
  }

  /** Lazily instantiated stanford pipeline. */
  lazy val pipeline = {
    // Construct a StanfordCoreNLP instance.
    val props: Properties = new Properties()
    props.setProperty("encoding", "utf8")
    props.setProperty("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
    props.setProperty("parse.model", "edu/stanford/nlp/models/lexparser/englishRNN.ser.gz")
    new StanfordCoreNLP(props)
  }

  /** Outputs the Stanford parse as TTL from the given source. */
  override def processText(source: Source, destination: Writer): Unit = {
    // Use the filename, sans extension, for the corpus.
    val corpus = source.descr.substring(0, source.descr.lastIndexOf("."))

    // Print the TTL namespace headers.
    destination.write(Ttl.NamespaceHeaders)

    // Run the text through Stanford parser.
    val annotation = pipeline.process(source.getLines().mkString("\n"))

    // Iterate over sentences parsed from the chunk.
    val sentences = annotation.get(classOf[CoreAnnotations.SentencesAnnotation]).asScala
    for ((sentence, sentenceIndex) <- sentences.zipWithIndex) {
      val sentenceId = sentenceIndex + 1
      // The character offset into the chunk of text. Used to build token offests.
      var sentenceOffset = sentence.get(classOf[CoreAnnotations.CharacterOffsetBeginAnnotation])

      // Print out all token info for the sentence.
      val tokens = sentence.get(classOf[CoreAnnotations.TokensAnnotation]).asScala
      for ((token, tokenIndex) <- tokens.zipWithIndex) {
        // Use one-based IDs.
        val tokenId = tokenIndex + 1

        // Convert to TTL.
        val ttlToken = Token.fromStanfordToken(corpus, sentenceId, tokenId, sentenceOffset, token)
        destination.write(ttlToken.ttl)
        destination.write('\n')
      }

      // Print out basic dependency info.
      val basicDependencyGraph =
        sentence.get(classOf[SemanticGraphCoreAnnotations.BasicDependenciesAnnotation])
      for (
        ttlDep <- Dependency.fromStanfordGraph(corpus, sentenceId, "basic", basicDependencyGraph)
      ) {
        destination.write(ttlDep.ttl)
      }
      destination.write('\n')

      // Print out collapsed dependency info.
      val collapsedDependencyGraph = sentence.get(
        classOf[SemanticGraphCoreAnnotations.CollapsedCCProcessedDependenciesAnnotation])
      for (
        ttlDep <- Dependency.fromStanfordGraph(corpus, sentenceId, "dep", collapsedDependencyGraph)
      ) {
        destination.write(ttlDep.ttl)
      }
      destination.write('\n')
    }

    // Build up coreferences.
    val allCorefChains: Iterable[Iterable[Coreference]] = for {
      chain <- annotation.get(classOf[CorefCoreAnnotations.CorefChainAnnotation]).asScala.values
      mentions = chain.getMentionsInTextualOrder.asScala
      // Skip empty or self-referential corefs.
      if mentions.size > 1
    } yield {
      val source = chain.getRepresentativeMention
      val sourceCoref = Coreference.fromStanfordSourceMention(corpus, source)
      val childCorefs = for {
        mention <- mentions if mention != source
      } yield Coreference.fromStanfordMention(corpus, sourceCoref.rootId, mention)

      sourceCoref +: childCorefs
    }
    // Print all corefs.
    for (corefChain <- allCorefChains) {
      for (coref <- corefChain) {
        destination.write(coref.ttl)
      }
      destination.write('\n')
    }
  }

  /** A token parsed from Stanford. */
  case class Token(id: String, text: String, lemma: String, begin: Int, end: Int,
      partOfSpeech: String, namedEntityType: Option[String], namedEntityTag: Option[String]) {

    val ttl: String = {
      val ttlBuilder = new StringBuilder()
      val escapedText = escapeString(text)
      val escapedLemma = escapeString(lemma)
      val escapedPartOfSpeech = escapeString(partOfSpeech)
      ttlBuilder.append(s"""${id} token:text "${escapedText}" .""").append('\n')
      ttlBuilder.append(s"""${id} token:lemma "${escapedLemma}" .""").append('\n')
      ttlBuilder.append(s"""${id} token:pos "${escapedPartOfSpeech}" .""").append('\n')
      ttlBuilder.append(s"""${id} token:begin ${begin} .""").append('\n')
      ttlBuilder.append(s"""${id} token:end ${end} .""").append('\n')
      namedEntityType match {
        // Skip output of default entity type O ("Other").
        case Some("O") =>
        case Some(neType) => {
          val escapedType = escapeString(neType)
          ttlBuilder.append(s"""${id} ne:type "${escapedType}" .""").append('\n')
          namedEntityTag map { neTag =>
            val escapedTag = escapeString(neTag)
            ttlBuilder.append(s"""${id} ne:norm "${escapedTag}" .""").append('\n')
          }
        }
        case None =>
      }
      ttlBuilder.toString
    }
  }
  object Token {
    /** @return the full ID string for a token with the given numerical ids */
    def buildId(corpus: String, sentenceId: Int, tokenId: Int): String =
      s"<http://aristo.allenai.org/id#${corpus}/${sentenceId}_${tokenId}>"

    /** Create a Token from a Stanford annotation.
      * @param sentenceId ID of the sentence in the source document.
      * @param tokenId ID of the token in the source sentence
      * @param sentenceOffset the offset of the sentence into the document. Used to adjust begin and
      * end values for tokens.
      */
    def fromStanfordToken(corpus: String, sentenceId: Int, tokenId: Int, sentenceOffset: Int,
      token: CoreLabel): Token = {

      val id = buildId(corpus, sentenceId, tokenId)
      val text = token.get(classOf[CoreAnnotations.TextAnnotation])
      val lemma = token.get(classOf[CoreAnnotations.LemmaAnnotation])
      val begin = token.get(classOf[CoreAnnotations.CharacterOffsetBeginAnnotation]) - sentenceOffset
      val end = token.get(classOf[CoreAnnotations.CharacterOffsetEndAnnotation]) - sentenceOffset
      val pos = token.get(classOf[CoreAnnotations.PartOfSpeechAnnotation])
      // Optional named entity types.
      val namedEntityType = Option(token.get(classOf[CoreAnnotations.NamedEntityTagAnnotation]))
      val namedEntityTag =
        Option(token.get(classOf[CoreAnnotations.NormalizedNamedEntityTagAnnotation]))

      Token(id, text, lemma, begin, end, pos, namedEntityType, namedEntityTag)
    }
  }

  /** @param name the dependency type
    * @param label the label for the dependency
    */
  case class Dependency(sourceId: String, targetId: String, name: String, label: String) {
    val ttl: String = s"${sourceId} ${name}:${label} ${targetId} .\n"
  }
  object Dependency {
    def fromStanfordGraph(corpus: String, sentenceId: Int, name: String,
      graph: SemanticGraph): Iterable[Dependency] = {

      // Root dependenc(y|ies).
      val roots = for (root <- graph.getRoots.asScala) yield {
        val sourceId = Token.buildId(corpus, sentenceId, 0)
        val targetId = Token.buildId(corpus, sentenceId, root.index)
        val label = GrammaticalRelation.ROOT.getLongName.replaceAll("\\s+", "")
        Dependency(sourceId, targetId, name, label)
      }
      // Graph edges.
      val edges = for (edge <- graph.edgeListSorted.asScala) yield {
        val sourceId = Token.buildId(corpus, sentenceId, edge.getSource.index)
        val targetId = Token.buildId(corpus, sentenceId, edge.getTarget.index)
        val label = edge.getRelation.toString.replaceAll("\\s+", "")
        Dependency(sourceId, targetId, name, label)
      }

      roots ++ edges
    }
  }

  /** A coreference from token `referenceId` to `rootId`. */
  case class Coreference(rootId: String, referenceId: String) {
    val ttl: String = s"${rootId} coref:ref ${referenceId} .\n"
  }
  object Coreference {
    /** Builds a source reference ("representative mention") from a mention instance. */
    def fromStanfordSourceMention(corpus: String, mention: CorefChain.CorefMention): Coreference = {
      val rootId = Token.buildId(corpus, mention.sentNum, mention.headIndex)
      // Root is self-referential.
      Coreference(rootId, rootId)
    }
    /** Builds a coreference from a mention using a given root. */
    def fromStanfordMention(corpus: String, rootId: String,
      mention: CorefChain.CorefMention): Coreference = {

      Coreference(rootId, Token.buildId(corpus, mention.sentNum, mention.headIndex))
    }
  }
}
