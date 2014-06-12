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
object StanfordTtl extends MultiTextProcessor {
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
        val ttlToken = Token.fromStanfordToken(sentenceId, tokenId, sentenceOffset, token)
        destination.write(ttlToken.ttl)
        destination.write('\n')
      }

      // Print out basic dependency info.
      val basicDependencyGraph =
        sentence.get(classOf[SemanticGraphCoreAnnotations.BasicDependenciesAnnotation])
      for (ttlDep <- Dependency.fromStanfordGraph(sentenceId, "basic", basicDependencyGraph)) {
        destination.write(ttlDep.ttl)
      }
      destination.write('\n')

      // Print out collapsed dependency info.
      val collapsedDependencyGraph = sentence.get(
        classOf[SemanticGraphCoreAnnotations.CollapsedCCProcessedDependenciesAnnotation])
      for (ttlDep <- Dependency.fromStanfordGraph(sentenceId, "dep", collapsedDependencyGraph)) {
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
      val sourceCoref = Coreference.fromStanfordSourceMention(source)
      val childCorefs = for {
        mention <- mentions if mention != source
      } yield Coreference.fromStanfordMention(sourceCoref.rootId, mention)

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
      ttlBuilder.append(s"""id:${id} token:text "${text}" .""").append('\n')
      ttlBuilder.append(s"""id:${id} token:lemma "${lemma}" .""").append('\n')
      ttlBuilder.append(s"""id:${id} token:pos "${partOfSpeech}" .""").append('\n')
      ttlBuilder.append(s"""id:${id} token:begin ${begin} .""").append('\n')
      ttlBuilder.append(s"""id:${id} token:end ${end} .""").append('\n')
      namedEntityType match {
        // Skip output of default entity type O ("Other").
        case Some("O") =>
        case Some(neType) => {
          ttlBuilder.append(s"""id:${id} ne:type "${neType}" .""").append('\n')
          namedEntityTag map { neTag =>
            ttlBuilder.append(s"""id:${id} ne:norm "${neTag}" .""").append('\n')
          }
        }
        case None =>
      }
      ttlBuilder.toString
    }
  }
  object Token {
    /** @return the full ID string for a token with the given numerical ids */
    def buildId(sentenceId: Int, tokenId: Int): String = s"${sentenceId}_${tokenId}"

    /** Create a Token from a Stanford annotation.
      * @param sentenceId ID of the sentence in the source document.
      * @param tokenId ID of the token in the source sentence
      * @param sentenceOffset the offset of the sentence into the document. Used to adjust begin and
      * end values for tokens.
      */
    def fromStanfordToken(sentenceId: Int, tokenId: Int, sentenceOffset: Int,
      token: CoreLabel): Token = {

      val id = buildId(sentenceId, tokenId)
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
    val ttl: String = s"id:${sourceId} ${name}:${label} id:${targetId} .\n"
  }
  object Dependency {
    def fromStanfordGraph(sentenceId: Int, name: String,
      graph: SemanticGraph): Iterable[Dependency] = {

      // Root dependenc(y|ies).
      val roots = for (root <- graph.getRoots.asScala) yield {
        val sourceId = Token.buildId(sentenceId, 0)
        val targetId = Token.buildId(sentenceId, root.index)
        val label = GrammaticalRelation.ROOT.getLongName.replaceAll("\\s+", "")
        Dependency(sourceId, targetId, name, label)
      }
      // Graph edges.
      val edges = for (edge <- graph.edgeListSorted.asScala) yield {
        val sourceId = Token.buildId(sentenceId, edge.getSource.index)
        val targetId = Token.buildId(sentenceId, edge.getTarget.index)
        val label = edge.getRelation.toString.replaceAll("\\s+", "")
        Dependency(sourceId, targetId, name, label)
      }

      roots ++ edges
    }
  }

  /** A coreference from token `referenceId` to `rootId`. */
  case class Coreference(rootId: String, referenceId: String) {
    val ttl: String = s"id:${rootId} coref:ref id:${referenceId} .\n"
  }
  object Coreference {
    /** Builds a source reference ("representative mention") from a mention instance. */
    def fromStanfordSourceMention(mention: CorefChain.CorefMention): Coreference = {
      val rootId = Token.buildId(mention.sentNum, mention.headIndex)
      // Root is self-referential.
      Coreference(rootId, rootId)
    }
    /** Builds a coreference from a mention using a given root. */
    def fromStanfordMention(rootId: String, mention: CorefChain.CorefMention): Coreference = {
      Coreference(rootId, Token.buildId(mention.sentNum, mention.headIndex))
    }
  }
}
