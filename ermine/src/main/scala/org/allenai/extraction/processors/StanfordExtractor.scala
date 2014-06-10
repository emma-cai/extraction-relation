package org.allenai.extraction.processors

import org.allenai.extraction.TextProcessor
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import scala.io.Source

import java.io.Writer


/** processor to match dependency patterns */
object StanfordExtractor extends TextProcessor {
  override val numInputs = 1
  override val numOutputs = 1

  // SPARQL queries
  val queries: Seq[(String,String)] = Seq(
    // (id, query) where id is for logging rule matches 

    //"In the hot weather our bodies sweat perspiration bringing water to our[our] skin."
    //% ("our bodies"/?x "sweat" "perspiration" [ "In the hot weather" ] )	""/EFFECT-55	("our bodies"/?x "bring" "water" [ "to our[our] skin" ] )
    ("55", """CONSTRUCT { ?root rel:effect ?comp . } WHERE {
      ?root dep:xcomp ?comp .
      ?comp token:pos "VBG" .
      FILTER NOT EXISTS {?comp dep:ccomp ?ccomp . }
    }"""),
    ("55", """CONSTRUCT { ?root rel:effect ?comp . } WHERE {
      ?root dep:vmod ?comp ;
            token:pos ?pos .
      FILTER(regex(str(?pos), "^VB")) .
      ?comp token:pos "VBG" .
      FILTER NOT EXISTS {?comp dep:ccomp ?ccomp . }
    }"""),

    //"When some animals prepare for the long winters by storing food and going dormant it is called hibernation."
    //% ("some animals"/?x "store" "food")	"by"/ENABLE-61	("some animals"/?x "prepare" "" [ "for the long winters" ] )
    ("61", """CONSTRUCT { ?root rel:enable ?comp . } WHERE {
      ?comp dep:prepc_by ?root .
      ?root token:pos "VBG" .
    }"""),

    //"When some animals prepare for the long winters by storing food and going dormant it is called hibernation."
    //% ("some animals" "prepare" "" [ "for the long winters" ] )	"is called"/EXAMPLE-7	"hibernation"
    ("7", """CONSTRUCT { ?root rel:example ?comp . } WHERE {
      ?called dep:xcomp ?root ;
              token:text "called" ;
              dep:auxpass ?is ;
              dep:advcl|dep:csubjpass ?comp .
    }"""),

    //"A hand lens is used to view objects in more detail."
    //% "A hand lens"/?x	"is used"/PURPOSE-29	("A hand lens"/?x "view" "objects" [ "in more detail" ] )
    ("29", """CONSTRUCT { ?root rel:purpose ?comp . } WHERE {
      ?rel dep:nsubjpass ?root ;
           dep:auxpass ?aux ;
           token:text "used" ;
           dep:xcomp ?comp .
      FILTER NOT EXISTS { ?parent dep:rcmod ?rel . }
    }"""),

    //"Freezing involves changing water from its[A hand lens] liquid state to its[its liquid state] solid state ice by the removal of heat."
    //% ("Freezing"/?x "remove" "heat")	"by"/ENABLE-24	("Freezing"/?x "change" "water" [ "from its[A hand lens] liquid state", "to its[its liquid state] solid state" ] )
    ("24", """CONSTRUCT { ?root rel:enable ?comp . } WHERE {
      ?comp dep:prep_by ?root ;
            token:pos "VBG" .
      ?root token:pos "NN" .
    }"""),

    //"Animals can not make their[some animals] own food so they[some animals] must eat to get nutrients."
    //% ("Animals" "not make" "their[some animals] own food")	"so"/EFFECT-83	("some animals" "eat" "")
    ("83", """CONSTRUCT { ?root rel:effect ?comp . } WHERE {
      ?root dep:advcl ?comp .
      ?comp dep:mark ?mark .
      ?mark token:lemma "so" .
    }"""),

    //"Animals can not make their[some animals] own food so they[some animals] must eat to get nutrients."
    //% ("some animals"/?x "eat" "")	"to"/REQUIREMENT-52	("some animals"/?x "get" "nutrients")
    ("52", """CONSTRUCT { ?root rel:requirement ?comp . } WHERE {
      ?root dep:xcomp ?comp ;
            dep:aux ?modal .
      ?modal token:lemma "must" .
      FILTER NOT EXISTS { ?parent dep:rcmod ?root . }
      FILTER NOT EXISTS { ?comp dep:cop ?cop . }
    }"""),

    //"Getting enough rest is also an important part of living a healthy life."
    //% ("" "get" "rest")	"part of"/PART-44	("" "live" "a healthy life")
    ("44", """CONSTRUCT { ?root rel:part ?comp . } WHERE {
      ?part dep:csubj ?root ;
            dep:cop ?cop ;
            token:lemma "part" .
      { ?part dep:prep_of ?comp . }
      UNION
      { ?part basic:prep ?prep .
        ?prep basic:dep ?comp . } # failed PP
    }"""),

    //"Decomposers are living things that break down dead organisms and recycle their[dead organisms] nutrients into the soil."
    //% "Decomposers"	"are"/EXAMPLE-8	("living things" "break down" "dead organisms")
    ("8", """CONSTRUCT { ?root rel:example ?comp . } WHERE {
      ?comp dep:nsubj ?root ;
            dep:cop ?cop ;
            token:lemma ?lemma .
      FILTER(!regex(str(?lemma), "^(example|cause|part|way)$$")) .
      { ?comp dep:det ?det . }
      UNION
      { ?comp token:pos "NNS" . }
    }"""),

    //"A human 's body produces sweat is a way that an organism may adjust to hot temperatures."
    //% "sweat"	"is way"/EXAMPLE-3	("an organism" "adjust" "sweat" [ "to hot temperatures" ] )
    ("3", """CONSTRUCT { ?root rel:example ?comp . } WHERE {
      ?way dep:nsubj ?root ;
           dep:cop ?cop ;
           token:lemma "way" ;
           dep:partmod|dep:rcmod ?comp .
    }"""),

    //"A squirrel storing nuts is preparing for a seasonal change in the environment."
    //% ("A squirrel"/?x "store" "nuts")	""/EFFECT-63	("A squirrel"/?x "prepare" "" [ "for a seasonal change in the environment" ] )
    ("63", """CONSTRUCT { ?root rel:effect ?comp . } WHERE {
      ?subj dep:partmod ?root .
      ?comp dep:nsubj ?subj .
      ?root token:pos "VBG" .
    }"""),

    //"A student should use a hand lens to get a better look at the spots."
    //% ("A student"/?x "use" "a hand lens")	"to"/EFFECT-53	("A student"/?x "get" "" [ "a better look at the spots" ] )
    ("53", """CONSTRUCT { ?root rel:effect ?comp . } WHERE {
      ?root dep:xcomp ?comp ;
            dep:aux ?modal .
      FILTER NOT EXISTS { ?modal token:lemma "must" . }
      FILTER NOT EXISTS { ?parent dep:rcmod ?root . }
      FILTER NOT EXISTS { ?comp dep:cop ?cop . }
    }"""),
    ("53", """CONSTRUCT { ?root rel:effect ?comp . } WHERE {
      ?root dep:xcomp ?comp .
      FILTER NOT EXISTS { ?root dep:aux ?modal . }
      FILTER NOT EXISTS { ?parent dep:rcmod ?root . }
      FILTER NOT EXISTS { ?comp dep:cop ?cop . }
    }"""),

    //"Decreasing the temperature is one way to change water from a liquid to a solid."
    //% ("" "decrease" "the temperature")	"is way"/EFFECT-85	("" "change" "water" [ "from a liquid", "to a solid" ] )
    ("85", """CONSTRUCT { ?root rel:effect ?comp . } WHERE {
      ?way dep:csubj ?root ;
           dep:cop ?cop ;
           token:lemma "way" ;
           dep:rcmod|dep:infmod ?comp .
    }"""),

    //"The heat that produces the smoke is caused by friction."
    //% "friction"	"caused"/CAUSE-17	("The heat" "produce" "the smoke")
    ("17", """CONSTRUCT { ?root rel:cause ?comp . } WHERE {
      ?subj dep:rcmod ?comp .
      ?cause dep:nsubjpass ?subj ;
             token:lemma "cause" ;
             dep:agent ?root .
    }"""),

    //"Resting is necessary for humans to maintain good health."
    //% ("humans"/?x "rest" "")	"necessary"/REQUIREMENT-39	("humans"/?x "maintain" "good health")
    ("39", """CONSTRUCT { ?root rel:requirement ?comp . } WHERE {
      ?necessary dep:nsubj ?root ;
                 token:lemma "necessary" ;
                 dep:cop ?cop ;
                 dep:advcl ?comp .
    }"""),

    //"The white fur of an arctic fox blends in with the snow is called camouflage."
    //% "The white fur of an arctic fox blends"	"is called"/EXAMPLE-9	"camouflage"
    ("9", """CONSTRUCT { ?root rel:example ?comp . } WHERE {
      ?called dep:nsubjpass ?comp ;
              token:text "called" ;
              dep:auxpass ?is ;
              dep:xcomp ?root .
      FILTER NOT EXISTS { ?called dep:advcl ?advcl . }
    }"""),

    //"When plants and animals die decomposers help return nutrients to the food chain."
    //% "decomposers"	"help"/PURPOSE-26	("decomposers" "return" "nutrients" [ "to the food chain" ] )
    ("26", """CONSTRUCT { ?root rel:purpose ?comp . } WHERE {
      ?help dep:nsubj ?root ;
            dep:ccomp|dep:xcomp ?comp ;
            token:lemma ?lemma .
      FILTER regex(?lemma, "^(help|aid|allow|assist|enable)$") .
      FILTER NOT EXISTS { ?parent dep:rcmod ?help . }
    }"""),

    //"When plants and animals die decomposers help return nutrients to the food chain."
    //% ("decomposers" "return" "nutrients" [ "to the food chain" ] )	"When"/WHEN-72	("plants" "die" "")
    ("72", """CONSTRUCT { ?root rel:when ?comp . } WHERE {
      ?help dep:xcomp|dep:ccomp ?root ;
            dep:advcl ?comp ;
            token:lemma ?lemma .
      FILTER regex(?lemma, "^(help|aid|allow|assist|enable)$") .
      ?comp dep:advmod|dep:mark ?adv .
      ?adv token:lemma "when" .
    }"""),

    //"Gravity causes the ball to fall to the ground."
    //% "Gravity"	"causes"/CAUSE-16	("the ball" "fall" "" [ "to the ground" ] )
    ("16", """CONSTRUCT { ?root rel:cause ?comp . } WHERE {
      ?cause dep:xcomp ?root ;
             token:lemma "cause" .
      { ?comp dep:rcmod ?cause . }
      UNION
      { ?cause dep:nsubj ?comp . }
    }""")
  )

  override protected def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    val source = sources(0)
    val graph = new DependencyGraph()
    graph.loadTurtle(source)

    // match patterns
    for {
      (id, query) <- queries
      map <- graph.executeQuery(query)
    } {
      // add results, recording id for logging
      graph.addEdge(id, map("subject"), map("object"), map("predicate").toLiteral)
    }

    // convert output
    val sink: Writer = destinations(0)
    graph.saveTurtle(sink)
    graph.shutdown()
  }
}
