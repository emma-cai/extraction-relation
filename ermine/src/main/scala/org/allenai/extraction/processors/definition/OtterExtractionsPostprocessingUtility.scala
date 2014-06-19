package org.allenai.extraction.processors.definition

object OtterExtractionsPostprocessingUtility {
  /** This is a semantic postprocessing method to
    * remove low-quality extraction tuples from a given
    * sequence of extractions output by the definition
    * extractor.
    * @param rawExtractions extractions from Otter definition
    * extractor to be cleaned up.
    * The method returns a clean set of extraction tuples.
    */
  def postProcess(rawExtractions: Seq[OtterExtractionTuple]): Seq[OtterExtractionTuple] = {
    var postProcessedExtractions = Seq.empty[OtterExtractionTuple]

    for (rawExtraction <- rawExtractions) {
      val postProcessedExtractionOption = processExtraction(rawExtraction)
      if (postProcessedExtractionOption.isDefined) {
        postProcessedExtractions :+= postProcessedExtractionOption.get
      }
    }
    postProcessedExtractions
  }

  /** Identifies tuples that are completely invalid or need to be modified somewhat.
    * For tuples that are invalid, returns None,
    * for tuples that need to be modified, returns the modified tuple
    * for the rest, returns the original tuple.
    */
  def processExtraction(extraction: OtterExtractionTuple): Option[OtterExtractionTuple] = {
    extraction match {
      // Invalid tuples: (x, isa, part), (x, isa, something), (x, isa, thing)
      case SimpleOtterExtractionTuple(_, _, _, relation, Some(relObj), advps, pps) if (relation.relationType.isDefined && (relation.relationType.get == RelationTypeEnum.IsA)
        && (advps.size == 0) && (pps.size == 0) && (relObj.string.equalsIgnoreCase("part")
          || relObj.string.equalsIgnoreCase("piece")
          || relObj.string.equalsIgnoreCase("thing")
          || relObj.string.equalsIgnoreCase("something"))) =>
        None
      // Tuples to be modified: (x, isa, somebody), etc: to be modified to (x, isa, person)
      case SimpleOtterExtractionTuple(tokens, interval, agent, relation, Some(relObj), advps, pps) if (relation.relationType.isDefined && (relation.relationType.get == RelationTypeEnum.IsA)
        && (advps.size == 0) && (pps.size == 0) && (relObj.string.equalsIgnoreCase("someone")
          || relObj.string.equalsIgnoreCase("anyone")
          || relObj.string.equalsIgnoreCase("somebody")
          || relObj.string.equalsIgnoreCase("anybody")
          || relObj.string.equalsIgnoreCase("one"))) =>
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