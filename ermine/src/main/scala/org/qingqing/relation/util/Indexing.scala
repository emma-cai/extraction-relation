package org.qingqing.relation.util

import scala.io.Source
import java.io.File
import scala.collection.mutable.Map
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.index.CorruptIndexException
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.store.LockObtainFailedException

class Indexing {
  def BuildIndex(dir: String, indexPath: String) = {
    //build index
    var mergeFactor = 1000
    var maxMergeDocs = Integer.MAX_VALUE

    var index = new File(indexPath)
    var analyzer: Analyzer = new StandardAnalyzer()
    var indexWriter: IndexWriter = new IndexWriter(index, analyzer, true)
    indexWriter.setMergeFactor(mergeFactor)
    indexWriter.setMaxMergeDocs(maxMergeDocs)

    //read from directory
    val files = new java.io.File(dir).listFiles.filter(_.getName.endsWith("lzo") == false)
    for (file <- files) { //for each file
      var filename = file.getName()
      println("building index from " + dir + "/" + filename)

      for (line <- scala.io.Source.fromFile(dir + "/" + filename).getLines()) { //for each line
        val arr = line.split("\t")
        val arrlen = arr.length
        var i = 9
        while (i + 9 <= arrlen) { //for each sentence
          val arg1 = arr(i)
          val kp = arr(i + 1)
          val arg2 = arr(i + 2)
          val sen = arr(i + 6)

          var document: Document = new Document()
          var field_arg1: Field = new Field("arg1", arg1, Field.Store.YES, Field.Index.ANALYZED)
          var field_kp: Field = new Field("kp", kp, Field.Store.YES, Field.Index.ANALYZED)
          var field_arg2: Field = new Field("arg2", arg2, Field.Store.YES, Field.Index.ANALYZED)
          var field_sen: Field = new Field("sen", sen, Field.Store.YES, Field.Index.ANALYZED)

          document.add(field_arg1)
          document.add(field_kp)
          document.add(field_arg2)
          document.add(field_sen)
          indexWriter.addDocument(document)

          i = i + 12
        }
      }
    }
    indexWriter.optimize()
    indexWriter.close()
  }
  
  def CK12Index(source: String, indexPath: String) = {
    //build index
    var mergeFactor = 1000
    var maxMergeDocs = Integer.MAX_VALUE

    var index = new File(indexPath)
    var analyzer: Analyzer = new StandardAnalyzer()
    var indexWriter: IndexWriter = new IndexWriter(index, analyzer, true)
    indexWriter.setMergeFactor(mergeFactor)
    indexWriter.setMaxMergeDocs(maxMergeDocs)

    for (line <- scala.io.Source.fromFile(source).getLines) { //for each line
      val arr = line.split("\t")
      val arrlen = arr.length
      if (arrlen >=2 ) { //for each sentence
        val id = arr(0)
        val sen = arr(1)
        var document: Document = new Document()
        var field_id: Field = new Field("id", id, Field.Store.YES, Field.Index.ANALYZED)
        var field_sen: Field = new Field("sen", sen, Field.Store.YES, Field.Index.ANALYZED)

        document.add(field_id)
        document.add(field_sen)
        indexWriter.addDocument(document)
      }
    }
    indexWriter.optimize()
    indexWriter.close()
  }
}