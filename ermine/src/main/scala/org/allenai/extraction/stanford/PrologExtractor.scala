package org.allenai.extraction.stanford

import org.allenai.extraction.interface._

import jpl.Query

import spray.json._
import spray.json.DefaultJsonProtocol._

import java.io.File

object PrologExtractor {
  def apply(ttlFile: File, tokenMap: Map[String, Token]): Seq[ExtractionRule] = {
    // TODO(jkinkead): Take from config?
    val prefixPath = "/Users/Kinkead/work/prototyping/prolog/extraction"
    val loadProlog = new Query(
      s"consult(['${prefixPath}/relation.pl', '${prefixPath}/patterns-stanford.pl'])," +
      s"rdf_load('${ttlFile.getAbsolutePath()}'), " +
      // Magic here: Fill in the Json variable with all relations.
      "relation(Json)")
    val rawJson = for (result <- loadProlog.allSolutions) yield {
      // TODO(jkinkead): Doesn't work, since the JSON has embedded \n literals, and is quoted.
      JsonParser(result.get("Json").toString)
    }

    val extractedRules = for {
      rawRule <- rawJson
      ruleObject = {
        rawRule match {
          case JsString(value) => println(value)
          case _ => println("(not string)")
        }
        rawRule.asJsObject
      }
      antecedents <- extractTuples(ruleObject.fields.get("antecedents"), tokenMap)
      consequents <- extractTuples(ruleObject.fields.get("consequents"), tokenMap)
    } yield {
      val relationString = Token.originalText(
        tokensFromIds(ruleObject.fields("relation").asJsObject.fields("string"), tokenMap))
      println("extracted relation: " + relationString)
      None
    }
    // Json is of the format:
    /*
     {
        "class":"ExtractionRule",
        "antecedents": [ {"class":"NounPhrase", "string": ["id:\'22.1\'" ]} ],
        "relation": {
          "class":"Relation",
          "string": ["id:\'22.2\'" ],
          "normalizedRelation":"CAUSE"
        },
        "consequents": [
          {
           "class":"ExtractionTuple",
           "subject": {"class":"NounPhrase", "string": ["id:\'22.3\'", "id:\'22.4\'" ]},
           "verbPhrase": ["id:\'22.6\'" ],
           "directObject": {},
           "extraPhrases": [ ["id:\'22.7\'", "id:\'22.8\'", "id:\'22.9\'" ] ]
          }
        ]
    }
     */

    Seq.empty
  }

  /** Extract a series of ExtractionTuple from a JsArray.
    * @return the tuples, or an empty seq if the value is missing or not an array
    */
  def extractTuples(arrayValue: Option[JsValue], tokenMap: Map[String, Token]):
    Seq[ExtractionTuple] = arrayValue match {
      /*
    case Some(JsArray(tuples)) => for {
      tuple <- tuples
      tupleObject = tuple.asJsObject
    } yield {

    }
    */
    case _ => Seq.empty
  }

  def tokensFromIds(jsValue: JsValue, tokenMap: Map[String, Token]): Seq[Token] = jsValue match {
    case JsArray(ids) => (for {
        id <- ids
        idString <- id match {
          case JsString(value) => Some(value)
          // TODO(jkinkead): Warn?
          case _ => None
        }
      } yield tokenMap.get(idString)).flatten
    case _ => Seq.empty
  }
}
