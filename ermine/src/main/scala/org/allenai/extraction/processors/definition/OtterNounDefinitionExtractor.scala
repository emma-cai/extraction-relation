package org.allenai.extraction.processors.definition

import org.allenai.taggers.{ Extractor, NamedGroupType }

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.typer.Type

import scala.Option.option2Iterable

/** A Definition Extractor to process Noun definitions.
  * All processing that happens here is intimately tied to the specific rules defined in the
  * Cascade file for Definition Extraction on Nouns.
  * @param dataPath Path to the noun definition data- mainly the required OpenRegex rule files.
  */
class OtterNounDefinitionExtractor(dataPath: String) extends OtterDefinitionExtractor(dataPath, "noun") {

  /** The input definition will match one of these high level definition types, or none at all.
    * This is the list of top level Types we will consider to start mining for the constituent
    * extraction parts.
    */
  val definitionTypeNames = Set[String]("RelClauseDefinition", "WhenWhereDefinition", "WhichWhomDefinition",
    "IsaFactsDefinition", "IsaToFactsDefinition", "IsaDefinition", "IsWhereDefinition")

  /** This (extract) method has been implemented in the DefinitionOpenRegexExtractor base class,
    * but we are overriding it  here because we have some special handling going on here- if the initial
    * definition text does not give back any extraction results, there is retry logic here to prepend
    * the definition term to the definition text and retry. This is required because of the way the
    * preprocessor splits definition lines with multiple definitions separated by semicolons.
    * Due to this preprocessing, sometimes a definition does not contain the subject (defined term).
    * This retry we know makes sense for noun definition terms- the patterns written handle both
    * a fully-formed sentence with the defined term as subject as well as "<term> : <definition>".
    * Also, we are not using the JSON format for the extraction results here right now.
    */
  override def extract(definedTerm: String, preprocessedDefinition: String): (Seq[OtterExtractionTuple], Seq[Lemmatized[ChunkedToken]]) = {
    var result = super.extractText(preprocessedDefinition)
    // Some retries if no results were found with just the definition text.
    if (definedTerm.length > 0) {
      // E.g.: The input, "Academia	Noun	'Academia' is a word for the group of people who are 
      // a part of the scientific and cultural community; this group of people have attended a 
      // university and/or do research." is converted by the preprocessor into two separate definition
      // lines:
      //   "Academia	Noun	Academia is a word for the group of people who are 
      //                      a part of the scientific and cultural community"
      //   and
      //   "Academia	Noun	this group of people have attended a university and/or do research."
      // The second definition does not have the term but a coreference. So if we pass
      // "Academia: this group of people have attended a university and/or do research." to the 
      // definition extractor, it works since it has rules to handle this pattern, but not without
      // the "Academia: ".
      if (result._1.isEmpty) {
        result = super.extractText(definedTerm + " : " + preprocessedDefinition)
      }
      // E.g.: The input, "brain cancer	Noun	# is a type of cancer that arises in the brain."
      // does not have the subject as part of the definition. In this case passing it to the 
      // definition extractor with the term prepended, i.e., as "brain cancer	is a type of cancer 
      // that arises in the brain." works.
      if (result._1.isEmpty) {
        result = super.extractText(definedTerm + " " + preprocessedDefinition)
      }
    }
    result
  }

  /** A client-visible method that takes the output of the 'extract' method, which contains
    * the output Types, and returns a list of extractions formatted as tuples, as Strings.
    * Sample output for the definition, "Chlorophyll is a green pigment found in almost all plants,
    * algae, and cyanobacteria." :
    * Each of these extractions is a Tuple representing the following information:
    * Defined Term: Chlorophyll
    * Isa: (Chlorophyll, isa, pigment)
    * Fact: (Chlorophyll, is, green pigment)
    * Quality: (Chlorophyll, is, green)
    * Fact: (Chlorophyll, found in, almost all plants, algae, and cyanobacteria, )
    */
  override def process(allTypes: Seq[Type], lastLevelTypes: Seq[Type], defnTokens: Seq[Lemmatized[ChunkedToken]]): Seq[OtterExtractionTuple] = {
    (lastLevelTypes find { t => definitionTypeNames.contains(t.name) } map { processNounDefinition(_, allTypes, defnTokens) }).getOrElse(Seq.empty[OtterExtractionTuple])
  }

  /** Given a Type, the entire list of output Types, and the seq of input definition tokens,
    * generate extractions from the Isa part and the Facts part (Look at the rules in sent.taggers for details).
    */
  private def processNounDefinition(typ: Type, types: Seq[Type], defnTokens: Seq[Lemmatized[ChunkedToken]]): Seq[OtterExtractionTuple] = {
    // Check the different possible parts (Isa, Facts) of a noun definition and get extractions for each.    

    // Isa
    val (definedTermOption: Option[Argument], isaExtractions: Seq[OtterExtractionTuple]) = processNounDefinitionIsa(typ, types, defnTokens)
    // Facts
    val factExtractions = definedTermOption map { processNounDefinitionFacts(_, typ, types, defnTokens) } getOrElse Seq.empty[OtterExtractionTuple]

    isaExtractions ++ factExtractions
  }

  /** The Isa part (potentially) consists of the following relations: Isa, Qualities (based on adjectives
    * preceding the Isa-noun), Context, and Properties (identified by the keyword "with"). Mine for each
    * of these parts and generate tuples.
    * Also return the Argument representing the DefinedTerm as this will need to be passed to processNounDefinitionFacts
    * as the implied subject where a subject is missing from a verb phrase.
    */
  private def processNounDefinitionIsa(typ: Type, types: Seq[Type], defnChunkedTokens: Seq[Lemmatized[ChunkedToken]]): (Option[Argument], Seq[OtterExtractionTuple]) = {
    var results = Seq.empty[OtterExtractionTuple]
    var definedTermArgOption: Option[Argument] = None
    val isaOption = Extractor.findSubtypesWithName(types)(typ, "Isa").headOption
    val (definedTermOption, isaRelOption, defnIsaOption) = nounDefinitionGetDefinedTermAndIsaRel(isaOption, types)
    val isaRelArgOption = isaRelOption map { x => Argument(x.text, OtterToken.makeTokenSeq(defnChunkedTokens, x.tokenInterval), Option(x.tokenInterval)) }
    (definedTermOption, defnIsaOption) match {
      case (Some(definedTerm), Some(defnIsa)) => {
        val (definedTermArg, definedTermTuple) = processNounDefinitionDefinedTerm(definedTerm, types, defnChunkedTokens)
        definedTermArgOption = Option(definedTermArg)
        results :+= definedTermTuple
        val subtypeTuples = (Extractor.findSubtypes(types)(defnIsa)).map(typ => (typ.name, typ))
        for ((typName, typContent) <- subtypeTuples) {
          val result = typName match {
            case "DefnIsa.Context" => Seq[OtterExtractionTuple](processNounDefinitionContext(definedTermArg, typContent, types, defnChunkedTokens))
            case "DefnIsa.IsaWhat" => processNounDefinitionIsaWhat(definedTermArg, isaRelArgOption, typContent, types, defnChunkedTokens)
            case "DefnIsa.Qualities" => processNounDefinitionQualities(definedTermArg, typContent, types, defnChunkedTokens)
            case "DefnIsa.Properties" => processNounDefinitionProperties(definedTermArg, typContent, types, defnChunkedTokens)
            case _ => Seq.empty[OtterExtractionTuple]
          }
          results ++= result
        }
      }
      case _ => None
    }
    (definedTermArgOption, results)
  }

  /** Get the Defined Term. This is somewhat redundant if the input definition file already contains the defined term.
    * The Defined Term ties together the different extractions for a certain definition since it is the subject for
    * every extraction.
    */
  private def nounDefinitionGetDefinedTermAndIsaRel(isaOption: Option[NamedGroupType], types: Seq[Type]): (Option[Type], Option[Type], Option[Type]) = {
    var defnIsaOption: Option[Type] = None
    var isaRelOption: Option[Type] = None
    val definedTermOption: Option[Type] = isaOption match {
      case Some(isa) =>
        (Extractor.findAlignedTypes(types)(isa).headOption match {
          case Some(defnPattern) =>
            var defnIsaPart = Option[Type](defnPattern)
            if (!defnPattern.name.equals("DefnIsa")) {
              defnIsaPart = Extractor.findSubtypesWithName(types)(defnPattern, "DefnIsaPart").headOption
            }
            defnIsaPart match {
              case Some(defnIsaPart) =>
                (defnIsaOption = Extractor.findAlignedTypesWithName(types)(defnIsaPart, "DefnIsa").headOption)
              case _ =>
                defnIsaOption = Option[Type](defnPattern)
            }
            defnIsaOption
          case _ => None
        }) match {
          case Some(defnIsa: Type) =>
            isaRelOption = Extractor.findSubtypesWithName(types)(defnIsa, "IsaRel").headOption
            Extractor.findSubtypesWithName(types)(defnIsa, "DefinedTerm").headOption match {
              case Some(definedTrm) => Option[Type](definedTrm)
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }
    (definedTermOption, isaRelOption, defnIsaOption)
  }

  /** Generate a Tuple containing an (Otter) Argument for the DefinedTerm and an OtterExtractionTuple to represent
    * the DefinedTerm relation
    */
  private def processNounDefinitionDefinedTerm(typ: Type, types: Seq[Type], defnChunkedTokens: Seq[Lemmatized[ChunkedToken]]): (Argument, OtterExtractionTuple) = {
    val definedTermTokens = OtterToken.makeTokenSeq(defnChunkedTokens, typ.tokenInterval)
    val definedTermArg = Argument(typ.text, definedTermTokens, Some(typ.tokenInterval))
    val definedTermRelObj = Option(definedTermArg)
    val definedTermTuple = SimpleOtterExtractionTuple(
      definedTermTokens,
      typ.tokenInterval,
      Some(Argument("DefinedTerm", Seq.empty[OtterToken], None)),
      Relation(Some(RelationTypeEnum.DefinedTerm), Argument("is", Seq.empty[OtterToken], None)),
      definedTermRelObj,
      Seq.empty[Argument],
      Seq.empty[Argument])
    (definedTermArg, definedTermTuple)
  }

  /** Generate an OtterExtractionTuple that has Context info for the definition
    */
  private def processNounDefinitionContext(definedTermArg: Argument, typ: Type, types: Seq[Type], defnChunkedTokens: Seq[Lemmatized[ChunkedToken]]): OtterExtractionTuple = {
    val defnTokens = OtterToken.makeTokenSeq(defnChunkedTokens, typ.tokenInterval)
    val typeArg = Argument(typ.text, defnTokens, Some(typ.tokenInterval))
    val relObj = Option(typeArg)
    SimpleOtterExtractionTuple(
      defnTokens,
      typ.tokenInterval,
      Some(Argument("Context", Seq.empty[OtterToken], None)),
      Relation(Some(RelationTypeEnum.Context), Argument("is", Seq.empty[OtterToken], None)),
      relObj,
      Seq.empty[Argument],
      Seq.empty[Argument])
  }

  /** Processes the IsaWhat group captured in the DefnIsa rule
    */
  private def processNounDefinitionIsaWhat(definedTermArg: Argument, isaRelArgOption: Option[Argument], typ: Type, types: Seq[Type], defnChunkedTokens: Seq[Lemmatized[ChunkedToken]]): Seq[OtterExtractionTuple] = {
    var results = Seq.empty[OtterExtractionTuple]
    val alignedTypes = Extractor.findAlignedTypes(types)(typ)
    for (alignedType <- alignedTypes) {
      if (results.length == 0) {
        alignedType.name match {
          case "NGMultiple" => results ++= processNounDefinitionNGMultiple(definedTermArg, isaRelArgOption, alignedType, types, defnChunkedTokens)
          case "NGNoDetSingle" => results ++= processNounDefinitionNGSingle(definedTermArg, isaRelArgOption, alignedType, types, defnChunkedTokens)
          case _ => None
        }
      }
    }
    results
  }

  /** Split composite NPs matching the NGMultiple rules up into a sequence of extractions representing
    * each constituent simple NP (NPSingle)
    */
  private def processNounDefinitionNGMultiple(definedTermArg: Argument, isaRelArgOption: Option[Argument], typ: Type, types: Seq[Type], defnChunkedTokens: Seq[Lemmatized[ChunkedToken]]): Seq[OtterExtractionTuple] = {
    var results = Seq.empty[OtterExtractionTuple]
    val subtypes = Extractor.findSubtypes(types)(typ)
    for (subtype <- subtypes) {
      val alignedTypes = Extractor.findAlignedTypesWithName(types)(subtype, "NGSingle")
      for (alignedType <- alignedTypes) {
        results ++= processNounDefinitionNGSingle(definedTermArg, isaRelArgOption, alignedType, types, defnChunkedTokens)
      }
    }
    results
  }

  /** Process simple NP
    */
  private def processNounDefinitionNGSingle(definedTermArg: Argument, isaRelArgOption: Option[Argument], typ: Type, types: Seq[Type], defnChunkedTokens: Seq[Lemmatized[ChunkedToken]]): Seq[OtterExtractionTuple] = {
    var results = Seq.empty[OtterExtractionTuple]
    val nounOption = nounDefinitionGetNounPartFromNGSingle(typ, types, "Nouns")
    val npOption = nounDefinitionGetNounPartFromNGSingle(typ, types, "NP")
    val auxOption = nounDefinitionGetAuxPartFromNGSingle(typ, types)
    val prepOption = nounDefinitionGetPrepFromNGSingle(typ, types)
    val adjs = nounDefinitionGetAdjsFromNGSingle(typ, types)

    val isaRelArg: Argument = isaRelArgOption getOrElse (Argument("isa", Seq.empty[OtterToken], None))

    if (nounOption.isDefined) {
      // Process the "nouns"-only part of the noun phrase.
      // The "if" above exists because that is the core part (the "head") of a noun phrase,
      // so if it doesn't exist, i.e., is None, then it doesn't make sense to proceed with
      // processing anything else.
      val noun = nounOption.get

      // Construct the tokens and relation object out of the extracted "nouns" part to make the
      // resultant IsA tuple.
      val nounTokens = OtterToken.makeTokenSeq(defnChunkedTokens, noun.tokenInterval)
      val nounArg = Argument(noun.text, nounTokens, Some(noun.tokenInterval))
      val relObj = Option(nounArg)
      results :+= SimpleOtterExtractionTuple(
        nounTokens,
        noun.tokenInterval,
        Some(definedTermArg),
        Relation(Some(RelationTypeEnum.IsA), isaRelArg),
        relObj,
        Seq.empty[Argument],
        Seq.empty[Argument])

      // Now process the entire noun phrase including the auxiliary and preposition parts since
      // we want to also generated IsA relations out of the bigger noun phrase.
      if (npOption.isDefined) {
        var preps = Seq.empty[Argument]
        var advs = Seq.empty[Argument]

        val np = npOption.get
        val npText = new StringBuilder(np.text)
        var npIntervalOption = Option(np.tokenInterval)

        // The "Aux" part as defined in the rules is a captured group representing possession.
        // In this case we want to merge the NP part with the Aux part to build the composite
        // argument. So we concatenate the Aux text to the NP text and take the total token interval
        // (span).
        if (auxOption.isDefined) {
          val aux = auxOption.get
          npText ++= " " + aux.text
          val auxIntervalOption = Option(aux.tokenInterval)
          if (auxIntervalOption.isDefined) {
            val auxInterval = auxIntervalOption.get
            npIntervalOption = npIntervalOption map { npInterval => Interval.span(Seq(npInterval, auxInterval)) }
          }
        }

        // If prepositions were present, generate the 'pps' Arguments field of the tuple (OtterExtractionTupleSimple)
        if (prepOption.isDefined) {
          val prep = prepOption.get
          val prepIntervalOption = Option(prep.tokenInterval)
          val prepTokens = prepIntervalOption match {
            case Some(prepInterval) => OtterToken.makeTokenSeq(defnChunkedTokens, prepInterval)
            case _ => Seq.empty[OtterToken]
          }
          if (prep.text != "") preps :+= Argument(prep.text, prepTokens, prepIntervalOption)
        }

        if (npIntervalOption.isDefined) {
          val npInterval = npIntervalOption.get
          // We need to produce an Extraction Tuple only if this interval does not completely
          // overlap, i.e., is not the same as the interval covered by just the Nouns (handled earlier).
          if (!npInterval.equals(noun.tokenInterval)) {
            val npTokens = OtterToken.makeTokenSeq(defnChunkedTokens, npInterval)
            val npArg = Argument(npText.toString, npTokens, Option(npInterval))
            val relObj = Option(npArg)
            results :+= SimpleOtterExtractionTuple(
              npTokens,
              npInterval,
              Some(definedTermArg),
              Relation(Some(RelationTypeEnum.IsA), isaRelArg),
              relObj,
              advps = advs,
              pps = preps)
          }
        }
      }
    }

    // Iterate over the adjectives and generate a Quality tuple for each.
    for (adj <- adjs) {
      val adjTokens = OtterToken.makeTokenSeq(defnChunkedTokens, adj.tokenInterval)
      val adjArg = Argument(adj.text, adjTokens, Some(adj.tokenInterval))
      val relObj = Option(adjArg)
      results :+= SimpleOtterExtractionTuple(
        adjTokens,
        adj.tokenInterval,
        Some(definedTermArg),
        Relation(Some(RelationTypeEnum.Quality), Argument("is", Seq.empty[OtterToken], None)),
        relObj,
        Seq.empty[Argument],
        Seq.empty[Argument])
    }

    results
  }

  /** Mine the "NounPart" group captured in the NGSingle/NGNoDetSingle rules
    */
  private def nounDefinitionGetNounPartFromNGSingle(typ: Type, types: Seq[Type], requiredPart: String): Option[Type] = {
    var nounPartSubtype = "NG1"
    var mainSubtype = "NP2"
    var isaTermSubtype = "NP1"

    if (typ.name.equals("NGNoDetSingle")) {
      nounPartSubtype = "NGNoDet1"
      mainSubtype = "NP2NoDet"
      isaTermSubtype = "NP1NoDet"
    }

    val noun = for {
      nounPart <- Extractor.findSubtypesWithName(types)(typ, "NounPart").headOption
      ng <- Extractor.findAlignedTypesWithName(types)(nounPart, nounPartSubtype).headOption
      main <- Extractor.findSubtypesWithName(types)(ng, "Main").headOption
      np2 <- Extractor.findAlignedTypesWithName(types)(main, mainSubtype).headOption
      isaTerm <- Extractor.findSubtypesWithName(types)(np2, "IsaTerm").headOption
      np1 <- Extractor.findAlignedTypesWithName(types)(isaTerm, isaTermSubtype).headOption
      res <- nounDefinitionGetNounPartFromNP1(np1, types, requiredPart)
    } yield {
      res
    }

    noun
  }

  /** Mine the "Aux" group captured in the NGSingle/NGNoDetSingle rules
    */
  private def nounDefinitionGetAuxPartFromNGSingle(typ: Type, types: Seq[Type]): Option[Type] = {
    var nounPartSubtype = "NG1"
    if (typ.name.equals("NGNoDetSingle"))
      nounPartSubtype = "NGNoDet1"
    // If (Main) noun was found, tack on auxiliary part (indicating possession) on to it    
    val auxOption = for {
      nounPart <- Extractor.findSubtypesWithName(types)(typ, "NounPart").headOption
      ng <- Extractor.findAlignedTypesWithName(types)(nounPart, nounPartSubtype).headOption
      aux <- Extractor.findSubtypesWithName(types)(ng, "Aux").headOption
    } yield aux

    auxOption
  }

  /** Mine the "PrepPart" group captured in the NGSingle/NGNoDetSingle rules
    */
  private def nounDefinitionGetPrepFromNGSingle(typ: Type, types: Seq[Type]): Option[Type] = {
    Extractor.findSubtypesWithName(types)(typ, "PrepPart").headOption
  }

  /** Extract adjectives, one per resultant string, from a noun phrase.
    */
  private def nounDefinitionGetAdjsFromNGSingle(typ: Type, types: Seq[Type]): Seq[Type] = {
    var nounPartSubtype = "NG1"
    var mainSubtype = "NP2"
    var isaTermSubtype = "NP1"

    if (typ.name.equals("NGNoDetSingle")) {
      nounPartSubtype = "NGNoDet1"
      mainSubtype = "NP2NoDet"
      isaTermSubtype = "NP1NoDet"
    }

    val adjs = for {
      nounPart <- Extractor.findSubtypesWithName(types)(typ, "NounPart").headOption
      ng <- Extractor.findAlignedTypesWithName(types)(nounPart, nounPartSubtype).headOption
      main <- Extractor.findSubtypesWithName(types)(ng, "Main").headOption
      np2 <- Extractor.findAlignedTypesWithName(types)(main, mainSubtype).headOption
      isaTerm <- Extractor.findSubtypesWithName(types)(np2, "IsaTerm").headOption
      np1 <- Extractor.findAlignedTypesWithName(types)(isaTerm, isaTermSubtype).headOption
    } yield {
      nounDefinitionGetAdjsFromNP1(np1, types)
    }

    adjs getOrElse Seq.empty[Type]
  }

  /** Helper method: Get the types corresponding to different parts of text matching the NP1 pattern
    */
  private def nounDefinitionGetNounPartFromNP1(typ: Type, types: Seq[Type], requiredPart: String): Option[Type] = {
    Extractor.findSubtypesWithName(types)(typ, requiredPart).headOption
  }

  /** Get the output types from the Adjs captured group from a NP1/NP1NoDet pattern
    */
  private def nounDefinitionGetAdjsFromNP1(typ: Type, types: Seq[Type]): Seq[Type] = {
    Extractor.findSubtypesWithName(types)(typ, "Adjs").headOption match {
      case Some(adjs) =>
        Extractor.findAlignedTypesWithName(types)(adjs, "AP1").headOption match {
          case Some(ap1) => getElementsOfCompositeType(ap1, types)
          case _ => Seq.empty[Type]
        }
      case _ => Seq.empty[Type]
    }
  }

  /** Helper method: Extract types corresponding to all constituent captured groups from a given type (pattern)
    */
  private def getElementsOfCompositeType(typ: Type, types: Seq[Type]): Seq[Type] = {
    Extractor.findSubtypes(types)(typ).toSeq
  }

  /** Helper method: Extract the text corresponding to each constituent captured group in a given type (pattern)
    */
  private def getElementsOfCompositeTypeText(typ: Type, types: Seq[Type]): Seq[String] = {
    getElementsOfCompositeType(typ, types).map(t => t.text)
  }

  /** Get the Qualities from a DefnIsa output Type and construct an OtterExtractionTuple for each
    */
  private def processNounDefinitionQualities(definedTermArg: Argument, typ: Type, types: Seq[Type], defnChunkedTokens: Seq[Lemmatized[ChunkedToken]]): Seq[OtterExtractionTuple] = {
    var results = Seq.empty[OtterExtractionTuple]
    val alignedTypes = Extractor.findAlignedTypes(types)(typ)
    for (alignedType <- alignedTypes) {
      if (results.length == 0) {
        alignedType.name match {
          case "AP3" =>
            {
              val ap3Types = getElementsOfCompositeType(alignedType, types)
              // Iterate over all the returned Types and construct Quality tuples out of them.
              for (adj <- ap3Types) {
                val adjTokens = OtterToken.makeTokenSeq(defnChunkedTokens, adj.tokenInterval)
                val adjArg = Argument(adj.text, adjTokens, Some(adj.tokenInterval))
                val relObj = Option(adjArg)
                results :+= SimpleOtterExtractionTuple(
                  adjTokens,
                  adj.tokenInterval,
                  Some(definedTermArg),
                  Relation(Some(RelationTypeEnum.Quality), Argument("is", Seq.empty[OtterToken], None)),
                  relObj,
                  Seq.empty[Argument],
                  Seq.empty[Argument])
              }
            }
          case _ =>
        }
      }
    }
    results
  }

  /** Get the Properties from a DefnIsa output Type and construct an OtterExtractionTuple for each
    */
  private def processNounDefinitionProperties(definedTermArg: Argument, typ: Type, types: Seq[Type], defnChunkedTokens: Seq[Lemmatized[ChunkedToken]]): Seq[OtterExtractionTuple] = {
    var results = Seq.empty[OtterExtractionTuple]
    val alignedTypes = Extractor.findAlignedTypes(types)(typ)
    for (alignedType <- alignedTypes) {
      if (results.length == 0) {
        val result = alignedType.name match {
          case "NGMultiple" =>
            {
              val ngTypes = getElementsOfCompositeType(alignedType, types)
              // Iterate over all the returned Types and construct Property OtterExtractionTuples out of them.
              for (ng <- ngTypes) {
                val ngTokens = OtterToken.makeTokenSeq(defnChunkedTokens, ng.tokenInterval)
                val ngArg = Argument(ng.text, ngTokens, Some(ng.tokenInterval))
                val relObj = Option(ngArg)
                results :+= SimpleOtterExtractionTuple(
                  ngTokens,
                  ng.tokenInterval,
                  Some(definedTermArg),
                  Relation(Some(RelationTypeEnum.Property), Argument("has", Seq.empty[OtterToken], None)),
                  relObj,
                  Seq.empty[Argument],
                  Seq.empty[Argument])
              }
            }
          case _ =>
        }
      }
    }
    results
  }

  /** Process the 'Fact' parts of the top level Definition rules for each high-level pattern (listed in
    * definitionTypeNames)
    */
  private def processNounDefinitionFacts(definedTermArg: Argument, typ: Type, types: Seq[Type], defnChunkedTokens: Seq[Lemmatized[ChunkedToken]]): Seq[OtterExtractionTuple] = {
    var results = Seq.empty[OtterExtractionTuple]
    val pp = Extractor.findSubtypesWithName(types)(typ, "PP").headOption
    val alignedTypes =
      Extractor.findSubtypesWithName(types)(typ, "Facts").headOption match {
        case Some(facts) =>
          Extractor.findAlignedTypes(types)(facts)
        case _ => Seq.empty[Type]
      }
    for (alignedType <- alignedTypes) {
      if (results.length == 0) {
        val currResults = alignedType.name match {
          case "VP3" => getExtractionTuplesForRuleVP3(definedTermArg, alignedType, types, defnChunkedTokens, pp)
          case "VP2" => getExtractionTuplesForRuleVP2(definedTermArg, alignedType, types, defnChunkedTokens, pp) match {
            case Some(x) => Seq[OtterExtractionTuple](x)
            case _ => Seq.empty[OtterExtractionTuple]
          }
          case "VP" => getExtractionTuplesForVPs(definedTermArg, alignedType, types, defnChunkedTokens, pp)
          case "S" => makeDescribesRelationExtractionTuples(definedTermArg, getExtractionTuplesForRuleS(alignedType, types, defnChunkedTokens, pp))
          case "S1" => makeDescribesRelationExtractionTuples(definedTermArg, getExtractionTuplesForRuleS1(alignedType, types, defnChunkedTokens, pp))
          case _ => Seq.empty[OtterExtractionTuple]
        }
        results ++= currResults
      }
    }
    results
  }

  /** Make the 'Desrcibes' Relation Extractions for facts that are complete sentences describing what the defined term is
    */
  private def makeDescribesRelationExtractionTuples(agent: Argument, tuples: Seq[OtterExtractionTuple]): Seq[OtterExtractionTuple] = {
    var results = Seq.empty[OtterExtractionTuple]
    for (tuple <- tuples) {
      tuple match {
        case x: SimpleOtterExtractionTuple =>
          results :+= OtterExtractionTupleWithTupleRelObject(
            x.tupleTokens,
            x.tokenInterval,
            Option(agent),
            Relation(Option(RelationTypeEnum.Describes), Argument("describes", Seq.empty[OtterToken], None)),
            x,
            Seq.empty[Argument],
            Seq.empty[Argument])
        case _ =>
      }
    }
    results
  }

  /** For a Composite VP, return a list of OtterExtractionTuples with one tuple per constituent VP
    */
  private def getExtractionTuplesForVPs(agent: Argument, typ: Type, types: Seq[Type], defnChunkedTokens: Seq[Lemmatized[ChunkedToken]], pp: Option[Type]): Seq[OtterExtractionTuple] = {
    val parts = Extractor.findAlignedTypesWithName(types)(typ, "VP").headOption match {
      case Some(vp) => getElementsOfCompositeType(vp, types)
      case _ => Seq.empty[Type]
    }
    val results: Seq[OtterExtractionTuple] = for {
      part <- parts
      vp1 <- Extractor.findAlignedTypesWithName(types)(part, "VP1").headOption
      result <- getExtractionTuplesForRuleVP1(agent, vp1, types, defnChunkedTokens, pp)
    } yield {
      result
    }

    results
  }

  /** Get a sequence of OtterExtractionTuples representing a match on the 'VP3' rule
    */
  private def getExtractionTuplesForRuleVP3(agent: Argument, typ: Type, types: Seq[Type], defnChunkedTokens: Seq[Lemmatized[ChunkedToken]], pp: Option[Type]): Seq[OtterExtractionTuple] = {
    val antecedentVPOption: Option[SimpleOtterExtractionTuple] =
      Extractor.findSubtypesWithName(types)(typ, "AntecedentVP").headOption match {
        case Some(antecedent) =>
          Extractor.findAlignedTypesWithName(types)(antecedent, "VP1").headOption match {
            case Some(vp1) => getExtractionTuplesForRuleVP1(agent, vp1, types, defnChunkedTokens, pp)
            case _ => None
          }
        case _ => None
      }
    val relOption: Option[Relation] =
      Extractor.findSubtypesWithName(types)(typ, "Rel").headOption match {
        case Some(rl) =>
          {
            val relTokens = OtterToken.makeTokenSeq(defnChunkedTokens, rl.tokenInterval)
            val relArg = Argument(rl.text, relTokens, Some(rl.tokenInterval))
            Option(Relation(None, relArg))
          }
        case _ => None
      }
    val consequentSs: Seq[OtterExtractionTuple] =
      Extractor.findSubtypesWithName(types)(typ, "ConsequentS").headOption match {
        case Some(consequent) =>
          Extractor.findAlignedTypesWithName(types)(consequent, "S1").headOption match {
            case Some(s1) => getExtractionTuplesForRuleS1(s1, types, defnChunkedTokens, pp)
            case _ => Seq.empty[OtterExtractionTuple]
          }
        case _ => Seq.empty[OtterExtractionTuple]
      }

    var results = Seq.empty[OtterExtractionTuple]

    for {
      antecedentVP <- antecedentVPOption
      rel <- relOption
    } yield {
      for (consequentS <- consequentSs) {
        consequentS match {
          case consequentSTuple: SimpleOtterExtractionTuple =>
            results :+= ComplexOtterExtractionTuple(
              OtterToken.makeTokenSeq(defnChunkedTokens, typ.tokenInterval),
              typ.tokenInterval,
              antecedentVP,
              rel,
              consequentSTuple)
          case _ =>
        }
      }
    }

    results
  }

  /** Get an OtterExtractionTuple representing a match on the 'VP2' rule
    */
  private def getExtractionTuplesForRuleVP2(agent: Argument, typ: Type, types: Seq[Type], defnChunkedTokens: Seq[Lemmatized[ChunkedToken]], pp: Option[Type]): Option[OtterExtractionTuple] = {
    val antecedentVPOption: Option[SimpleOtterExtractionTuple] =
      Extractor.findSubtypesWithName(types)(typ, "AntecedentVP").headOption match {
        case Some(antecedent) =>
          Extractor.findAlignedTypesWithName(types)(antecedent, "VP1").headOption match {
            case Some(vp1) => getExtractionTuplesForRuleVP1(agent, vp1, types, defnChunkedTokens, pp)
            case _ => None
          }
        case _ => None
      }

    val relOption: Option[Relation] =
      Extractor.findSubtypesWithName(types)(typ, "Rel").headOption match {
        case Some(rl) =>
          {
            val relTokens = OtterToken.makeTokenSeq(defnChunkedTokens, rl.tokenInterval)
            val relArg = Argument(rl.text, relTokens, Some(rl.tokenInterval))
            Option(Relation(None, relArg))
          }
        case _ => None
      }

    val consequentVPOption: Option[SimpleOtterExtractionTuple] =
      Extractor.findSubtypesWithName(types)(typ, "ConsequentVP").headOption match {
        case Some(consequent) =>
          Extractor.findAlignedTypesWithName(types)(consequent, "VP1").headOption match {
            case Some(vp1) => getExtractionTuplesForRuleVP1(agent, vp1, types, defnChunkedTokens, pp)
            case _ => None
          }
        case _ => None
      }

    val result = for {
      antecedentVP <- antecedentVPOption
      rel <- relOption
      consequentVP <- consequentVPOption
    } yield {
      ComplexOtterExtractionTuple(
        OtterToken.makeTokenSeq(defnChunkedTokens, typ.tokenInterval),
        typ.tokenInterval,
        antecedentVP,
        rel,
        consequentVP)
    }

    result
  }

  /** Get an OtterExtractionTuple representing a match on the 'VP1' rule
    */
  def getExtractionTuplesForRuleVP1(agent: Argument, typ: Type, types: Seq[Type], defnChunkedTokens: Seq[Lemmatized[ChunkedToken]], pp: Option[Type]): Option[SimpleOtterExtractionTuple] = {
    val relOption: Option[Relation] = Extractor.findSubtypesWithName(types)(typ, "Rel").headOption match {
      case Some(rl) =>
        {
          val relTokens = OtterToken.makeTokenSeq(defnChunkedTokens, rl.tokenInterval)
          val relArg = Argument(rl.text, relTokens, Some(rl.tokenInterval))
          Option(Relation(None, relArg))
        }
      case _ => None
    }

    val arg2Option = Extractor.findSubtypesWithName(types)(typ, "Arg2").headOption
    val arg3Option = Extractor.findSubtypesWithName(types)(typ, "Arg3").headOption

    // Get the intervals corresponding to the above args
    val arg2IntervalOption = arg2Option map { arg2 => arg2.tokenInterval }
    val arg3IntervalOption = arg3Option map { arg3 => arg3.tokenInterval }

    // Get the overall interval (span)
    val intervalOption = arg3IntervalOption match {
      case Some(arg3Interval) =>
        {
          arg2IntervalOption match {
            case Some(arg2Interval) => Option(Interval.span(Seq(arg2Interval, arg3Interval)))
            case _ => Option(arg3Interval)
          }
        }
      case _ => arg2IntervalOption
    }
    // Get the complete text for the relation object Argument
    val argText = new StringBuilder((arg2Option map { arg2 => arg2.text }) getOrElse (""))
    argText ++= (arg3Option map { arg3 => arg3.text }) getOrElse ("")
    // Get the complete set of tokens for the relation object Argument
    val argTokens = intervalOption match {
      case Some(interval) => OtterToken.makeTokenSeq(defnChunkedTokens, interval)
      case _ => Seq.empty[OtterToken]
    }
    // Now construct the relation Object Argument (we need an Option type)
    val relObjOption = Some(Argument(argText.toString, argTokens, intervalOption))

    // Get the prepositional phrase Arguments
    val pps = Extractor.findSubtypesWithName(types)(typ, "Arg").headOption match {
      case Some(prep) =>
        {
          val prepTokens = OtterToken.makeTokenSeq(defnChunkedTokens, prep.tokenInterval)
          if (prep.text != "") Seq[Argument](Argument(prep.text, prepTokens, intervalOption)) else Seq()
        }
      case _ => Seq.empty[Argument]
    }

    // Get the adverbial phrase Arguments
    val advps = Extractor.findSubtypesWithName(types)(typ, "Arg4").headOption match {
      case Some(advp) =>
        {
          val advpTokens = OtterToken.makeTokenSeq(defnChunkedTokens, advp.tokenInterval)
          Seq[Argument](Argument(advp.text, advpTokens, Some(advp.tokenInterval)))
        }
      case _ => Seq.empty[Argument]
    }

    // Construct the result Extraction Tuple using all the pieces of info derived above
    val result = for {
      rel <- relOption
    } yield {
      SimpleOtterExtractionTuple(
        OtterToken.makeTokenSeq(defnChunkedTokens, typ.tokenInterval),
        typ.tokenInterval,
        Option(agent),
        rel,
        relObjOption,
        advps = advps,
        pps = pps)
    }

    result
  }

  /** Get a seq of OtterExtractionTuples representing a match on the 'S' rule
    */
  private def getExtractionTuplesForRuleS(typ: Type, types: Seq[Type], defnChunkedTokens: Seq[Lemmatized[ChunkedToken]], pp: Option[Type]): Seq[OtterExtractionTuple] = {
    val antecedentS1Tuples = (for {
      antecedentS <- Extractor.findSubtypesWithName(types)(typ, "AntecedentS").headOption
      S1 <- Extractor.findAlignedTypesWithName(types)(antecedentS, "S1").headOption
    } yield (getExtractionTuplesForRuleS1(S1, types, defnChunkedTokens, pp))) getOrElse Seq.empty[OtterExtractionTuple]

    val relOption: Option[Relation] =
      Extractor.findSubtypesWithName(types)(typ, "Rel").headOption match {
        case Some(rl) =>
          {
            val relTokens = OtterToken.makeTokenSeq(defnChunkedTokens, rl.tokenInterval)
            val relArg = Argument(rl.text, relTokens, Some(rl.tokenInterval))
            Option(Relation(None, relArg))
          }
        case _ => None
      }

    val consequentS1Tuples = (for {
      consequentS <- Extractor.findSubtypesWithName(types)(typ, "ConsequentS").headOption
      S1 <- Extractor.findAlignedTypesWithName(types)(consequentS, "S1").headOption
    } yield (getExtractionTuplesForRuleS1(S1, types, defnChunkedTokens, pp))) getOrElse Seq.empty[OtterExtractionTuple]

    var results = Seq.empty[OtterExtractionTuple]
    // Compose a Complex Extraction Tuple for every possible pair of Simple Antecedent and Consequent Tuples
    if (relOption.isDefined) {
      val rel = relOption.get
      for (antecedent <- antecedentS1Tuples) {
        for (consequent <- consequentS1Tuples) {
          (antecedent, consequent) match {
            case (antecedentTuple: SimpleOtterExtractionTuple, consequentTuple: SimpleOtterExtractionTuple) =>
              {
                results :+= ComplexOtterExtractionTuple(
                  OtterToken.makeTokenSeq(defnChunkedTokens, typ.tokenInterval),
                  typ.tokenInterval,
                  antecedentTuple,
                  rel,
                  consequentTuple)
              }
            case _ =>
          }
        }
      }
    }

    results
  }

  /** Get a seq of OtterExtractionTuples representing a match on the 'S1' rule
    */
  private def getExtractionTuplesForRuleS1(typ: Type, types: Seq[Type], defnChunkedTokens: Seq[Lemmatized[ChunkedToken]], pp: Option[Type]): Seq[OtterExtractionTuple] = {
    val arg1Option = Extractor.findSubtypesWithName(types)(typ, "Arg1").headOption
    val subj: Argument = arg1Option match {
      case Some(arg1) => Argument(arg1.text, OtterToken.makeTokenSeq(defnChunkedTokens, arg1.tokenInterval), Some(arg1.tokenInterval))
      case _ => Argument("", Seq.empty[OtterToken], None)
    }
    val vps =
      Extractor.findSubtypesWithName(types)(typ, "VP").headOption match {
        case Some(vp) =>
          Extractor.findAlignedTypesWithName(types)(vp, "VP").headOption match {
            case Some(vpType) => getExtractionTuplesForVPs(subj, vpType, types, defnChunkedTokens, pp)
            case _ => Seq.empty[SimpleOtterExtractionTuple]
          }
        case _ => Seq.empty[SimpleOtterExtractionTuple]
      }
    vps
  }
}
