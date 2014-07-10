package org.allenai.extraction.confidence

import org.allenai.extraction.confidence.ExtractionInstance.TokenMap
import org.allenai.extraction.processors.definition.OtterToken
import org.allenai.extraction.processors.definition.Argument
import org.allenai.extraction.processors.dependencies.ExtractionLabels
import org.allenai.extraction.FlatProcessor
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.DependencyGraph.tokenInfo
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import scala.collection.mutable
import scala.io.Source

import com.tinkerpop.blueprints.Vertex
import java.io.Writer

/** This processor takes at input the *.ttl.out file (as processed by TurtleProcessor)
 *  which has the fully processed RDF graph. The output is a dump of extractions in
 *  a pretty print format suitable for tagging along with features to be used for
 *  classifier training. A sample entry looks like
 *  
 *  $$SentenceNum$$ = 63
 *  Some offspring are born looking like their parents .
 *  ;;; "Some offspring are born" --EFFECT-> [ - "looking" - LIKE:"their parents" ]<TAB>0.0<TAB>[1|Some|some|DT|] [2|offspring|offspring|NN|]...<TAB>0.0<TAB>0.0<TAB>0.0<TAB>1.0<TAB>...
 *   
 *  where the sentence numbers are used to order the extractions for tagging, and the tab-separated
 *  fields are:
 *  extraction<TAB>score<TAB>tokens<TAB>tab-separated features  
 *    
 **/
object DumpFerretFeatures extends FlatProcessor {

  val inputGraph = new MemoryStoreSailGraph()
  /* for each sentence number, store the sentence string and the full sequence of tokens */
  val sentenceInfoCache = mutable.Map[Int, (String, Seq[OtterToken])]()
  DependencyGraph.setNamespaces(inputGraph)

  // SPARQL query for nodes with added rel: relation
  val relQuery =
    """SELECT ?x ?r ?y WHERE {
      ?x ?rel ?y .
      FILTER(STRSTARTS(str(?rel), "http://aristo.allenai.org/rel/")) .
      BIND(STRAFTER(str(?rel), "http://aristo.allenai.org/rel/") AS ?r) .
    }"""

  val separator = ", "

  /* This code traverse the RDF graph picking up the salient information, many of the
   * sparql queries have been copied from the main pipeline for producing inference rules */
  override def processText(source: Source, sink: Writer): Unit = {

    DependencyGraph.fromTurtle(inputGraph, source)
    
    sink.write("*** Ferret Dump for " + source.descr + " ***\n")
    sink.write("\n*** FEATURES ***\n")
    FerretFeatures.featureMap.keys.foreach { name => sink.write("::: " + name + "\n") }
    
    sink.write("\n*** EXTRACTIONS ***\n")
    // match patterns
    for (map <- DependencyGraph.executeSparql(inputGraph, relQuery)) {
      val x: Vertex = map("x")
      val y: Vertex = map("y")
      val xElements: Vector[(String, Vertex)] = nodeElements(x)
      val yElements: Vector[(String, Vertex)] = nodeElements(y)
      val nodes = (for ((id, node) <- xElements ++ yElements) yield node).toList.distinct
      val sentenceNumbers = (nodes map nodeSentenceNumber).distinct
      val sentences: List[String] = for (num <- sentenceNumbers) yield sentenceInfoCache.getOrElseUpdate(num, getSentenceInfo(num))._1

      val tokenMap: TokenMap = (sentenceNumbers flatMap (num =>
        for (token <- sentenceInfoCache.getOrElseUpdate(num, getSentenceInfo(num))._2) yield (num, token.id) -> token)).toMap

      val rel = map("r")

      val extractionInstance = ExtractionInstance(
        sourceText = sentences.mkString("\n"),
        tokenMap = tokenMap,
        extraction = getExtractionNodes(xElements, rel, yElements, tokenMap))

      sink.write("$$SentenceNum$$ = " + sentenceNumbers.max + "\n")
      sink.write(sentences.mkString("\n"))
      sink.write("\n")
      sink.write(";;; " + extractionInstance.prettyPrint + "\t" + 0.0 + "\t" +
          tokenMap.values.toSeq.sortBy(_.id).map(_.toFullString).mkString(" ") + "\t" +
          (FerretFeatures.featureMap.map { case (name, feature) => feature(extractionInstance) }).mkString("\t")
      )
      sink.write("\n\n")
    }

    inputGraph.shutdown()
  }

  def VertexToToken(v: Vertex, tokenMap: TokenMap): OtterToken =
    tokenMap.get(v.ids) match {
      case Some(token) => token
      case None => throw new IllegalArgumentException(s"TokenMap lookup failed for token ${v.ids}")
    }

  def getExtractionNodeForVertex(v: Vertex, isRelation: Boolean, tokenMap: TokenMap, semanticLabel: String = "") = {
    // The list of exclusion labels gotten from ExtractionLabels processor, to ensure consistency
    val exclude = if (isRelation) ExtractionLabels.VerbExcludeString else ExtractionLabels.ArgExcludeString
    val nodes = (v +: DependencyGraph.nodeConstits(inputGraph, v, exclude)).sortBy(_.tokenId)
    val tokens = nodes map (node => VertexToToken(node, tokenMap))
    ExtractionNode(nodeId = v.toIdString, string = nodeString(v), tokens = tokens, semanticLabel = semanticLabel)
  }

  def getExtractionNodesForArgument(elements: Vector[(String, Vertex)], tokenMap: TokenMap): ExtractionNodeOrTuple = {
    val stdArgs = List("self", "agent", "object")
    if (elements.length == 1 && elements.head._1 == "self")
      getExtractionNodeForVertex(elements.head._2, false, tokenMap)
    else {
      val map = elements.toMap
      val agent = map.get("agent") map (x => getExtractionNodeForVertex(x, false, tokenMap))
      val rel = map.get("self") map (x => getExtractionNodeForVertex(x, true, tokenMap))
      val dObject = map.get("object") map (x => getExtractionNodeForVertex(x, false, tokenMap))
      val args = for {
        (arg, vertex) <- elements
        if !stdArgs.contains(arg)
      } yield getExtractionNodeForVertex(vertex, false, tokenMap, semanticLabel = arg)
      ExtractionTuple(agent = agent, relation = rel.get, dObject = dObject,
        args = args, semanticLabel = rel.get.semanticLabel)
    }
  }

  def getExtractionNodes(xElements: Vector[(String, Vertex)], rel: Vertex, yElements: Vector[(String, Vertex)],
    tokenMap: TokenMap): ExtractionTuple = {
    ExtractionTuple(
      agent = Some(getExtractionNodesForArgument(xElements, tokenMap)),
      relation = ExtractionNode(nodeId = rel.toIdString, string = "UNKNOWN", tokens = Seq(), semanticLabel = rel.toStringLiteral),
      dObject = Some(getExtractionNodesForArgument(yElements, tokenMap)))
  }

  /* get token information associated with a sentence ID */
  def getSentenceInfo(sentenceNum: Int) = {
    val query: String = s"""
      SELECT ?uri ?text ?begin ?pos ?lemma WHERE {
        ?uri <http://nlp.stanford.edu/token/text> ?text .
        FILTER(STRSTARTS(str(?uri), "http://aristo.allenai.org/id#${sentenceNum}_"))
        ?uri <http://nlp.stanford.edu/token/begin> ?begin ;
             <http://nlp.stanford.edu/token/pos> ?pos ;
             <http://nlp.stanford.edu/token/lemma> ?lemma .
      }"""
    val res = for {
      map <- DependencyGraph.executeSparql(inputGraph, query)
      text = map("text").toStringLiteral
    } yield (map("begin").toIntLiteral,
      text,
      OtterToken(id = map("uri").tokenId, string = text, posTag = map("pos").toStringLiteral,
        chunk = "", lemma = map("lemma").toStringLiteral))
    val sortedRes = res.sortBy(_._1)
    (sortedRes.map(_._2).mkString(" "), sortedRes.map(_._3))
  }

  def nodeSentenceNumber(node: Vertex): Int =
    """http://aristo.allenai.org/id#(\d+)""".r.findFirstMatchIn(node.toUri).map(_.group(1)).getOrElse("0").toInt

  /* Gives Vector("isa" -> vertex1, "agent" -> vertex2, ...) */
  def nodeElements(node: Vertex): Vector[(String, Vertex)] = {
    val uri: String = node.toUri
    val query: String = s"""
      SELECT ?pred ?arg WHERE {
        <$uri> ?rel ?arg .
        FILTER(STRSTARTS(str(?rel), "http://aristo.allenai.org/pred/")) .
        BIND(STRAFTER(str(?rel), "http://aristo.allenai.org/pred/") AS ?pred) .
      }"""

    val res = for {
      map <- DependencyGraph.executeSparql(inputGraph, query)
      pred = map("pred").toStringLiteral
    } yield pred -> map("arg")
    res.toVector :+ ("self" -> node)  // use "self" to disinguish from other "isa" edges
  }

  def nodeArgs(node: Vertex, prefix: String = ""): String = {
    val uri: String = node.toUri
    val label: String = nodeLabel(node)
    val query: String = s"""
      SELECT ?pred ?arg WHERE {
        <$uri> ?rel ?arg .
        FILTER(STRSTARTS(str(?rel), "http://aristo.allenai.org/pred/")) .
        BIND(STRAFTER(str(?rel), "http://aristo.allenai.org/pred/") AS ?pred) .
      }"""
    val args = new StringBuilder()
    for (map <- DependencyGraph.executeSparql(inputGraph, query)) {
      val pred = map("pred").toStringLiteral
      val argString = nodeString(map("arg"))
      if (args.isEmpty)
        args ++= prefix
      else
        args ++= separator
      args.append(s"""$pred($label, "$argString")""")
    }
    args.toString
  }

  def nodeIsa(node: Vertex, prefix: String = ""): String = {
    val label: String = nodeLabel(node)
    val string: String = nodeString(node)
    s"""${prefix}isa($label, "$string")"""
  }

  def nodeLabel(node: Vertex): String = {
    val uri: String = node.toUri
    val id: String = uri.split("http://aristo.allenai.org/id#").last
    val query: String = s"""
      SELECT ?label WHERE {
        <$uri> rdfs:label ?label .
      }"""
    val result: Map[String, Vertex] = DependencyGraph.executeSparql(inputGraph, query).head
    val label = result.get("label").map(_.toStringLiteral).getOrElse("")
    s"E$id-$label"
  }

  def nodeString(node: Vertex): String = {
    val uri: String = node.toUri
    val query: String = s"""
      SELECT ?string WHERE {
        <$uri> rdfs:comment ?string .
      }"""
    val result: Seq[Map[String, Vertex]] = DependencyGraph.executeSparql(inputGraph, query)
    if (result.isEmpty)
      s"MISSING_COMMENT($uri)"
    else
      result.head.get("string").map(_.toStringLiteral).getOrElse("")
  }

}
