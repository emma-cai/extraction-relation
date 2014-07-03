package org.allenai.extraction.processors.definition

object OtterExtractionsPostprocessingUtility {

  /** List of words that if present as the object of an IsA relation by themselves,
    * with no other information present, do not make useful extractions. Such
    * extractions should be removed from the results.
    */
  val relObjBlacklist = Set("part", "piece", "thing", "something", "kind", "type", "sort",
    "variety", "way", "manner", "style", "form", "group", "set", "class", "category", "means",
    "mode", "format", "word", "term")
  val relObjPersonlist = Set("someone", "anyone", "somebody", "anybody", "one")

  /** This is a semantic postprocessing method to
    * remove low-quality extraction tuples from a given
    * sequence of extractions output by the definition
    * extractor.
    * @param rawExtractions extractions from Otter definition
    * extractor to be cleaned up.
    * The method returns a clean set of extraction tuples.
    */
  def postProcess(rawExtractions: Seq[OtterExtractionTuple]): Seq[OtterExtractionTuple] = {
    for {
      rawExtraction <- rawExtractions
      postProcessedExtraction <- processExtraction(rawExtraction)
    } yield postProcessedExtraction
  }

  /** Identifies tuples that are completely invalid or need to be modified somewhat.
    * For tuples that are invalid, returns None,
    * for tuples that need to be modified, returns the modified tuple
    * for the rest, returns the original tuple.
    */
  def processExtraction(extraction: OtterExtractionTuple): Option[OtterExtractionTuple] = {
    extraction match {
      // Invalid tuples: (x, isa, x), (x, isa, part), (x, isa, something), (x, isa, thing)
      case SimpleOtterExtractionTuple(_, _, Some(agent), relation, Some(relObj), advps, pps) if (relation.relationType.isDefined && (relation.relationType.get == RelationTypeEnum.IsA)
        && (advps.size == 0) && (pps.size == 0) &&
        (agent.string.trim.equalsIgnoreCase(relObj.string) || (relObjBlacklist.contains(relObj.string.toLowerCase)))) =>
        None
      // Tuples to be modified: (x, isa, somebody), etc: to be modified to (x, isa, person)
      case SimpleOtterExtractionTuple(tokens, interval, agent, relation, Some(relObj), advps, pps) if (relation.relationType.isDefined && (relation.relationType.get == RelationTypeEnum.IsA)
        && (advps.size == 0) && (pps.size == 0) && relObjPersonlist.contains(relObj.string.toLowerCase)) =>
        Option(SimpleOtterExtractionTuple(tokens,
          interval,
          agent,
          relation,
          Option(Argument("person", relObj.tokens, relObj.tokenInterval)),
          advps,
          pps))
      // Tuples that are "fine"
      case _ => Option(extraction)
    }
  }
}