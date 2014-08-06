package org.allenai.relation.util

import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.index.CorruptIndexException
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.queryParser.QueryParser
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.ScoreDoc
import org.apache.lucene.search.TopScoreDocCollector
import org.apache.lucene.store.LockObtainFailedException
import org.apache.lucene.util.Version
import org.apache.lucene.search.Similarity
import org.apache.lucene.index.FieldInvertState

class Searching {
  def runSearch(index: String, query: String, queryField: String,
    resFields: List[String], maxres: Int): List[List[String]] = {
    if(query.equals(""))
      return null
    
      //maxinum number of results considered
    var resList: List[List[String]] = List() //list of results							
     
    try {
      var collector: TopScoreDocCollector = TopScoreDocCollector.create(maxres, false)
      var searcher: IndexSearcher = new IndexSearcher(index)
      var analyzer: Analyzer = new StandardAnalyzer()
      var parser: QueryParser = new QueryParser(queryField, analyzer)
      var phraseQuery: org.apache.lucene.search.Query = parser.parse(query)
      searcher.search(phraseQuery, collector)
      var hits = collector.topDocs().scoreDocs
      var numTotalHits = collector.getTotalHits()
      if (numTotalHits > 0) {
        for (i <- 0 to Math.min(maxres, numTotalHits) - 1) {
        //   println("i="+i)
          var document = searcher.doc(hits(i).doc)
          var perres: List[String] = List()
        //for one query, it may have multiple fields returned
          resFields.foreach(p => perres = perres ::: List(document.get(p)))
          if (!resList.contains(perres))
            resList = resList :+ perres
      }
    }
    searcher.close()
    analyzer.close()
    }catch {
      case e => null
    }
    return resList
  }

  def runSearch(index: String, query1: String, query2: String, queryField1: String,
    queryField2: String, resFields: List[String], maxres: Int): List[List[String]] = {
    
    if(query1.equals("") || query2.equals(""))
      return null
      
    var resList: List[List[String]] = List() //list of results

    try {
      var collector: TopScoreDocCollector = TopScoreDocCollector.create(maxres, false)
      var searcher: IndexSearcher = new IndexSearcher(index)
      var analyzer: Analyzer = new StandardAnalyzer()
    //    var parser1:QueryParser = new QueryParser(queryField1, analyzer)
    //    var phraseQuery1:org.apache.lucene.search.Query = parser1.parse(query1)
    //    var parser2:QueryParser = new QueryParser(queryField2, analyzer)
    //    var phraseQuery2:org.apache.lucene.search.Query = parser2.parse(query2)
      var parser: QueryParser = new QueryParser(queryField1, analyzer)

      var special = queryField1 + ":" + query1 + " AND " + queryField2 + ":" + query2
      var phraseQuery: org.apache.lucene.search.Query = parser.parse(special)
      searcher.search(phraseQuery, collector)
      var hits = collector.topDocs().scoreDocs
      var numTotalHits = collector.getTotalHits()
    
      for (i <- 0 to Math.min(maxres, numTotalHits) - 1) {
        var document = searcher.doc(hits(i).doc)
        var perres: List[String] = List()
      //for one query, it may have multiple fields returned
        resFields.foreach(p => perres = perres ::: List(document.get(p)))
        if (!resList.contains(perres))
          resList = resList :+ perres
      }

      searcher.close()
      analyzer.close()
      }catch {
        case e => null
      }
    return resList
  }
  

  
//  def TFIDF(indexPath: String, query: String, queryfield: String): Int = {
//    var searcher: IndexSearcher = new IndexSearcher(indexPath)
//    var analyzer: Analyzer = new StandardAnalyzer()
//    var parser: QueryParser = new QueryParser(queryfield, analyzer)
//    var special = queryfield + ":" + query
//    var phraseQuery: org.apache.lucene.search.Query = parser.parse(special)
//    var docs = searcher.search(phraseQuery)
//    var len = docs.length()
//    println(len)
//    searcher.close()
//    analyzer.close()
//
//    return docs.length()
//  }
}


//class SimpleSimilarity extends Similarity {
//  def computeNorm(field:String, state:FieldInvertState) = { }
//}
