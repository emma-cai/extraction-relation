package org.allenai.extraction.processors.definition

import java.io.Writer

import scala.io.Source

import org.allenai.taggers.Extractor
import org.allenai.taggers.NamedGroupType

import edu.knowitall.tool.typer.Type

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

  /** This (processText) method has been implemented in the DefinitionOpenRegexExtractor base class,
    * but we are overriding it  here because we have some special handling going on here- if the initial
    * definition text does not give back any extraction results, there is retry logic here to prepend
    * the definition term to the definition text and retry. This is required because of the way the
    * preprocessor splits definition lines with multiple definitions separated by semicolons.
    * Due to this preprocessing, sometimes a definition does not contain the subject (defined term).
    * This retry we know makes sense for noun definition terms- the patterns written handle both
    * a fully-formed sentence with the defined term as subject as well as "<term> : <definition>".
    * Also, we are not using the JSON format for the extraction results here right now.
    */
  override def processText(defnInputSource: Source, destination: Writer): Unit = {

    // Iterate over input sentences (definitions), preprocess each and send it to the extractText method.
    for (line <- defnInputSource.getLines) {
      val (term, termWordClass, termDefinition) = preprocessLine(line)
      if ((termWordClass.length == 0) || (termWordClass.equalsIgnoreCase(wordClass))) {
        var results = super.extractText(termDefinition)

        // Some retries if no results were found with just the definition text.
        if (term.length > 0) {
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
          if (results.isEmpty) {
            results = super.extractText(term + " : " + termDefinition)
          }

          // E.g.: The input, "brain cancer	Noun	# is a type of cancer that arises in the brain."
          // does not have the subject as part of the definition. In this case passing it to the 
          // definition extractor with the term prepended, i.e., as "brain cancer	is a type of cancer 
          // that arises in the brain." works.
          if (results.isEmpty) {
            results = super.extractText(term + " " + termDefinition)
          }
        }

        // Output: First write out the input line.
        destination.write("DEFINITION:   " + line + "\n")

        // Then write out the extraction results.
        for (result <- results) {
          destination.write(result + "\n")
        }
        destination.write("\n")
      }
    }

  }

  /** A client-visible method that takes the output of the 'extract' method, which contains
    * the output Types, and returns a list of extractions formatted as tuples, as Strings.
    * Sample output for the definition, "Chlorophyll is a green pigment found in almost all plants,
    * algae, and cyanobacteria." :
    * Each of these extractions is a String in the returned Seq of Strings:
    * Defined Term: Chlorophyll
    * Isa: (Chlorophyll, isa, pigment)
    * Fact: (Chlorophyll, is, green pigment)
    * Quality: (Chlorophyll, is, green)
    * Fact: (Chlorophyll, found in, almost all plants, algae, and cyanobacteria, )
    */
  override def process(allTypes: Seq[Type], lastLevelTypes: Seq[Type]): Seq[String] = {
    (lastLevelTypes find { t => definitionTypeNames.contains(t.name) } map { processNounDefinition(_, allTypes) }).getOrElse(Seq.empty[String])
  }

  /** Given a Type and the entire list of output Types, generate extractions from the
    * Isa part and the Facts part (Look at the rules in sent.taggers for details).
    */
  private def processNounDefinition(typ: Type, types: Seq[Type]): Seq[String] = {
    // Check the different possible parts (Isa, Facts) of a noun definition and get extractions for each.    

    // Isa
    val (definedTermOption: Option[String], isaExtractions: Seq[String]) = processNounDefinitionIsa(typ, types)
    // Facts
    val factExtractions = definedTermOption map { processNounDefinitionFacts(_, typ, types) } getOrElse Seq.empty[String]

    isaExtractions ++ factExtractions
  }

  /** The Isa part (potentially) consists of the following relations: Isa, Qualities (based on adjectives
    * preceding the Isa-noun), Context, and Properties (identified by the keyword "with"). Mine for each
    * of these parts and generate tuple-like relation strings.
    */
  private def processNounDefinitionIsa(typ: Type, types: Seq[Type]): (Option[String], Seq[String]) = {
    var results = Seq.empty[String]
    val isaOption = Extractor.findSubtypesWithName(types)(typ, "Isa").headOption
    val (definedTermOption, defnIsaOption) = nounDefinitionGetDefinedTerm(isaOption, types)
    (definedTermOption, defnIsaOption) match {
      case (Some(definedTerm), Some(defnIsa)) => {
        results ++= Seq[String]("Defined Term: " + definedTerm)
        val subtypeTuples = (Extractor.findSubtypes(types)(defnIsa)).map(typ => (typ.name, typ))
        for ((typName, typContent) <- subtypeTuples) {
          val result = typName match {
            case "DefnIsa.Context" => Seq[String]("Context:" + typContent.text)
            case "DefnIsa.IsaWhat" => processNounDefinitionIsaWhat(definedTerm, typContent, types)
            case "DefnIsa.Qualities" => processNounDefinitionQualities(definedTerm, typContent, types)
            case "DefnIsa.Properties" => processNounDefinitionProperties(definedTerm, typContent, types)
            case _ => Seq.empty[String]
          }
          results ++= result
        }
      }
      case _ => None
    }
    (definedTermOption, results)
  }

  /** Get the Defined Term. This is somewhat redundant if the input definition file already contains the defined term.
    * The Defined Term ties together the different extractions for a certain definition since it is the subject for every extraction.
    */
  private def nounDefinitionGetDefinedTerm(isaOption: Option[NamedGroupType], types: Seq[Type]): (Option[String], Option[Type]) = {
    var defnIsaOption: Option[Type] = None
    val definedTermOption: Option[String] = isaOption match {
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
            Extractor.findSubtypesWithName(types)(defnIsa, "DefinedTerm").headOption match {
              case Some(definedTrm) => Option[String](definedTrm.text)
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }
    (definedTermOption, defnIsaOption)
  }

  /** Processes the IsaWhat group captured in the DefnIsa rule */
  private def processNounDefinitionIsaWhat(definedTerm: String, typ: Type, types: Seq[Type]): Seq[String] = {
    var results = Seq.empty[String]
    val alignedTypes = Extractor.findAlignedTypes(types)(typ)
    for (alignedType <- alignedTypes) {
      if (results.length == 0) {
        alignedType.name match {
          case "NGMultiple" => results ++= processNounDefinitionNGMultiple(definedTerm, alignedType, types)
          case "NGNoDetSingle" => results ++= processNounDefinitionNGSingle(definedTerm, alignedType, types)
          case _ => None
        }
      }
    }
    results
  }

  /** Splits composite NPs matching the NGMultiple rules up into a sequence of extractions representing
    * each constituent simple NP (NPSingle)
    */
  private def processNounDefinitionNGMultiple(definedTerm: String, typ: Type, types: Seq[Type]): Seq[String] = {
    var results = Seq.empty[String]
    val subtypes = Extractor.findSubtypes(types)(typ)
    for (subtype <- subtypes) {
      val alignedTypes = Extractor.findAlignedTypesWithName(types)(subtype, "NGSingle")
      for (alignedType <- alignedTypes) {
        results ++= processNounDefinitionNGSingle(definedTerm, alignedType, types)
      }
    }
    results
  }

  /** Process simple NP */
  private def processNounDefinitionNGSingle(definedTerm: String, typ: Type, types: Seq[Type]): Seq[String] = {
    var results = Seq.empty[String]

    val np: StringBuilder = new StringBuilder(nounDefinitionGetNounPartFromNGSingle(typ, types, "NP"))
    var noun: String = nounDefinitionGetNounPartFromNGSingle(typ, types, "Nouns")
    var aux: String = nounDefinitionGetAuxPartFromNGSingle(typ, types)
    val prep: String = nounDefinitionGetPrepFromNGSingle(typ, types)
    val adjs: Seq[String] = nounDefinitionGetAdjsFromNGSingle(typ, types)

    if (noun.length > 0) {
      results ++= Seq[String]("Isa: (" + definedTerm + ", isa, " + noun + ")")
      if (aux.length > 0) {
        np ++= " " + aux
      }
      if (prep.length > 0) {
        np ++= ", " + prep
      }
      if ((np.length > 0) && (!np.toString.equals(noun))) {
        results ++= Seq[String]("Isa: (" + definedTerm + ", isa, " + np + ")")
      }
    }

    results ++= adjs.map(adj => "Quality: (" + definedTerm + ", is, " + adj + ")")

    results
  }

  /** Mine the "NounPart" group captured in the NGSingle/NGNoDetSingle rules */
  private def nounDefinitionGetNounPartFromNGSingle(typ: Type, types: Seq[Type], requiredPart: String): String = {
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
    } yield {
      nounDefinitionGetNounPartFromNP1(np1, types, requiredPart)
    }

    noun.getOrElse("")
  }

  /** Mine the "Aux" group captured in the NGSingle/NGNoDetSingle rules */
  private def nounDefinitionGetAuxPartFromNGSingle(typ: Type, types: Seq[Type]): String = {
    var nounPartSubtype = "NG1"
    if (typ.name.equals("NGNoDetSingle"))
      nounPartSubtype = "NGNoDet1"
    // If (Main) noun was found, tack on auxiliary part (indicating possession) on to it    
    val auxOption = for {
      nounPart <- Extractor.findSubtypesWithName(types)(typ, "NounPart").headOption
      ng <- Extractor.findAlignedTypesWithName(types)(nounPart, nounPartSubtype).headOption
      aux <- Extractor.findSubtypesWithName(types)(ng, "Aux").headOption
    } yield aux.text

    auxOption.getOrElse("")
  }

  /** Mine the "PrepPart" group captured in the NGSingle/NGNoDetSingle rules */
  private def nounDefinitionGetPrepFromNGSingle(typ: Type, types: Seq[Type]): String = {
    Extractor.findSubtypesWithName(types)(typ, "PrepPart").headOption map { _.text } getOrElse ""
  }

  /** Extract adjectives, one per resultant string, from a noun phrase. */
  private def nounDefinitionGetAdjsFromNGSingle(typ: Type, types: Seq[Type]): Seq[String] = {
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

    adjs getOrElse Seq.empty[String]
  }

  /** Helper method to get different parts of text matching the NP1 pattern */
  private def nounDefinitionGetNounPartFromNP1(typ: Type, types: Seq[Type], requiredPart: String): String = {
    Extractor.findSubtypesWithName(types)(typ, requiredPart).headOption match {
      case Some(nouns) => nouns.text
      case _ => ""
    }
  }

  /** Get the result of the Adjs captured group from a NP1/NP1NoDet pattern */
  private def nounDefinitionGetAdjsFromNP1(typ: Type, types: Seq[Type]): Seq[String] = {
    Extractor.findSubtypesWithName(types)(typ, "Adjs").headOption match {
      case Some(adjs) =>
        Extractor.findAlignedTypesWithName(types)(adjs, "AP1").headOption match {
          case Some(ap1) => getElementsOfCompositeTypeText(ap1, types)
          case _ => Seq.empty[String]
        }
      case _ => Seq.empty[String]
    }
  }

  /** Helper method to extract all constituent captured groups from a given type (pattern) */
  private def getElementsOfCompositeType(typ: Type, types: Seq[Type]): Seq[Type] = {
    Extractor.findSubtypes(types)(typ) toSeq
  }

  /** Helper method to extract the text corresponding to each constituent captured group in a given type (pattern) */
  private def getElementsOfCompositeTypeText(typ: Type, types: Seq[Type]): Seq[String] = {
    getElementsOfCompositeType(typ, types).map(t => t.text)
  }

  /** Get the Qualities from a DefnIsa output Type */
  private def processNounDefinitionQualities(definedTerm: String, typ: Type, types: Seq[Type]): Seq[String] = {
    var results = Seq.empty[String]
    val alignedTypes = Extractor.findAlignedTypes(types)(typ)
    for (alignedType <- alignedTypes) {
      if (results.length == 0) {
        val result = alignedType.name match {
          case "AP3" => getElementsOfCompositeTypeText(alignedType, types)
          case _ => Seq.empty[String]
        }
        results ++= result.map(r => "Quality: (" + definedTerm + ", is, " + r + ")")
      }
    }
    results
  }

  /** Get the Properties from a DefnIsa output Type */
  private def processNounDefinitionProperties(definedTerm: String, typ: Type, types: Seq[Type]): Seq[String] = {
    var results = Seq.empty[String]
    val alignedTypes = Extractor.findAlignedTypes(types)(typ)
    for (alignedType <- alignedTypes) {
      if (results.length == 0) {
        val result = alignedType.name match {
          case "NGMultiple" => getElementsOfCompositeTypeText(alignedType, types)
          case _ => Seq.empty[String]
        }
        results ++= result.map(r => "Property: (" + definedTerm + ", has, " + r + ")")
      }
    }
    results
  }

  /** Process the 'Fact' parts of the top level Definition rules for each high-level pattern (listed in
    * definitionTypeNames)
    */
  private def processNounDefinitionFacts(definedTerm: String, typ: Type, types: Seq[Type]): Seq[String] = {
    var results = Seq.empty[String]
    val pp =
      Extractor.findSubtypesWithName(types)(typ, "PP").headOption match {
        case Some(pp) => ", " + pp.text
        case _ => ""
      }

    val alignedTypes =
      Extractor.findSubtypesWithName(types)(typ, "Facts").headOption match {
        case Some(facts) =>
          Extractor.findAlignedTypes(types)(facts)
        case _ => Seq.empty[Type]
      }
    for (alignedType <- alignedTypes) {
      if (results.length == 0) {
        alignedType.name match {
          case "VP3" => results ++= Seq[String]("Fact: (" + getFormattedVP3(alignedType, types, definedTerm) + pp + ")")
          case "VP2" => results ++= Seq[String]("Fact: (" + getFormattedVP2(alignedType, types, definedTerm) + pp + ")")
          case "VP" => results ++= getFormattedVPs(alignedType, types).map(r => "Fact: (" + definedTerm + ", " + r + pp + ")")
          case "S" => results ++= Seq[String]("Fact: (" + definedTerm + ", describes, " + getFormattedS(alignedType, types) + ")")
          case "S1" => results ++= Seq[String]("Fact: (" + definedTerm + ", describes, " + getFormattedS1(alignedType, types) + ")")
          case _ => None
        }
      }
    }
    results
  }

  /** For a Composite VP, this method returns a list of strings with one extraction per constituent VP */
  private def getFormattedVPs(typ: Type, types: Seq[Type]): Seq[String] = {
    var results = Seq.empty[String]
    val parts = Extractor.findAlignedTypesWithName(types)(typ, "VP").headOption match {
      case Some(vp) => getElementsOfCompositeType(vp, types)
      case _ => Seq.empty[Type]
    }
    for (part <- parts) {
      val result =
        Extractor.findAlignedTypesWithName(types)(part, "VP1").headOption match {
          case Some(vp1) => getFormattedVP1(vp1, types)
          case _ => ""
        }
      results ++= Seq[String](result)
    }
    results
  }

  /** Get a string representing a match on a sentence rule defined in the 'VP3' rule */
  private def getFormattedVP3(typ: Type, types: Seq[Type], subj: String): String = {
    val result: StringBuilder = new StringBuilder("(")
    val antecedentVP: String =
      Extractor.findSubtypesWithName(types)(typ, "AntecedentVP").headOption match {
        case Some(antecedent) =>
          Extractor.findAlignedTypesWithName(types)(antecedent, "VP1").headOption match {
            case Some(vp1) => getFormattedVP1(vp1, types)
            case _ => ""
          }
        case _ => ""
      }
    val rel =
      Extractor.findSubtypesWithName(types)(typ, "Rel").headOption match {
        case Some(rl) => rl.text
        case _ => ""
      }
    val consequentS: String =
      Extractor.findSubtypesWithName(types)(typ, "ConsequentS").headOption match {
        case Some(consequent) =>
          Extractor.findAlignedTypesWithName(types)(consequent, "S1").headOption match {
            case Some(s1) => getFormattedS1(s1, types)
            case _ => ""
          }
        case _ => ""
      }
    if ((antecedentVP.length() > 0) && (rel.length() > 0) && (consequentS.length() > 0)) {
      result ++= "(" + subj + " , " + antecedentVP + "), " + rel + ", " + consequentS
    }
    result ++= ")"
    result.toString()
  }

  /** Get a string representing a match on a sentence rule defined in the 'VP2' rule */
  private def getFormattedVP2(typ: Type, types: Seq[Type], subj: String): String = {
    val result: StringBuilder = new StringBuilder("")
    val antecedentVP: String =
      Extractor.findSubtypesWithName(types)(typ, "AntecedentVP").headOption match {
        case Some(antecedent) =>
          Extractor.findAlignedTypesWithName(types)(antecedent, "VP1").headOption match {
            case Some(vp1) => getFormattedVP1(vp1, types)
            case _ => ""
          }
        case _ => ""
      }

    val rel =
      Extractor.findSubtypesWithName(types)(typ, "Rel").headOption match {
        case Some(rl) => rl.text
        case _ => ""
      }

    val consequentVP: String =
      Extractor.findSubtypesWithName(types)(typ, "ConsequentVP").headOption match {
        case Some(consequent) =>
          Extractor.findAlignedTypesWithName(types)(consequent, "VP1").headOption match {
            case Some(vp1) => getFormattedVP1(vp1, types)
            case _ => ""
          }
        case _ => ""
      }

    if ((antecedentVP.length() > 0) && (rel.length() > 0) && (consequentVP.length() > 0)) {
      result ++= "(" + subj + ", " + antecedentVP + "), " + rel + ", " + "(" + " , " + consequentVP + ")"
    }
    result.toString()
  }

  /** Get a string representing a match on a sentence rule defined in the 'VP1' rule */
  def getFormattedVP1(typ: Type, types: Seq[Type]): String = {
    val result: StringBuilder = new StringBuilder("")
    val rel = (Extractor.findSubtypesWithName(types)(typ, "Rel").headOption map (_.text)).getOrElse("")
    val arg2 = (Extractor.findSubtypesWithName(types)(typ, "Arg2").headOption map (_.text)).getOrElse("")
    val arg3 = (Extractor.findSubtypesWithName(types)(typ, "Arg3").headOption map (_.text)).getOrElse("")
    val arg = (Extractor.findSubtypesWithName(types)(typ, "Arg").headOption map (_.text)).getOrElse("")
    if (rel.length() > 0) {
      result ++= rel + ", " + arg2 + ", " + arg3 + ", " + arg
    }
    result.toString()
  }

  /** Get a string representing a match on a sentence rule defined in the 'S' rule */
  private def getFormattedS(typ: Type, types: Seq[Type]): String = {
    val result: StringBuilder = new StringBuilder("")
    val formattedAntecedentS1 = (for {
      antecedentS <- Extractor.findSubtypesWithName(types)(typ, "AntecedentS").headOption
      S1 <- Extractor.findAlignedTypesWithName(types)(antecedentS, "S1").headOption
    } yield (getFormattedS1(S1, types))) getOrElse ""

    val rel =
      Extractor.findSubtypesWithName(types)(typ, "Rel").headOption match {
        case Some(rl) => rl.text
        case _ => ""
      }

    val formattedConsequentS1 = (for {
      consequentS <- Extractor.findSubtypesWithName(types)(typ, "ConsequentS").headOption
      S1 <- Extractor.findAlignedTypesWithName(types)(consequentS, "S1").headOption
    } yield (getFormattedS1(S1, types))) getOrElse ""

    if ((formattedAntecedentS1.length() > 0) && (rel.length() > 0) && (formattedConsequentS1.length() > 0)) {
      result ++= "(" + formattedAntecedentS1 + ", " + rel + ", " + formattedConsequentS1 + ")"
    }
    result.toString()
  }

  /** Get a string representing a match on a sentence rule defined in the 'S1' rule */
  private def getFormattedS1(typ: Type, types: Seq[Type]): String = {
    val result = new StringBuilder("")
    val arg1 =
      Extractor.findSubtypesWithName(types)(typ, "Arg1").headOption match {
        case Some(a) =>
          a.text
        case _ => ""
      }
    val vps =
      Extractor.findSubtypesWithName(types)(typ, "VP").headOption match {
        case Some(vp) =>
          Extractor.findAlignedTypesWithName(types)(vp, "VP").headOption match {
            case Some(vpType) => getFormattedVPs(vpType, types)
            case _ => Seq.empty[String]
          }
        case _ => Seq.empty[String]
      }

    // Need an extra pair of parens around this sentence if it has a composite VP that 
    // is broken into multiple VPs (tuples).
    val numVps = vps.length
    if (numVps > 1)
      result ++= "("
    var ix = 0
    for (vp <- vps) {
      var vpStr = ""
      if ((arg1.length() > 0) && (vp.length() > 0)) {
        if (ix > 0)
          result ++= ", "
        vpStr = "(" + arg1 + ", " + vp + ")"
        ix += 1
      }
      result ++= vpStr
    }
    if (numVps > 1)
      result ++= ")"

    result.toString()
  }
}
