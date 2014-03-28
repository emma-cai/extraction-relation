package org.allenai.extraction.manager

import org.allenai.extraction.stanford.PrologExtractor
import org.allenai.extraction.stanford.StanfordParser
import org.allenai.extraction.stanford.StanfordXmlToTtl

import com.typesafe.config.ConfigFactory

import java.io.File
import java.io.FileOutputStream
import java.io.PrintStream
import java.io.StringReader
import java.io.StringWriter

/** Main app to run extractions. */
object Ermine {
  def main(args: Array[String]): Unit = {
    // Load up a config file.
    val config = ConfigFactory.load()

    // TODO(jkinkead): Validate the input.
    val infile = new File(config.getString("ermine.input.filename"))
    val outfile = new File(config.getString("ermine.output.filename"))

    if (!infile.canRead()) {
      // TODO(jkinkead): Better error handling.
      println("bad infile: " + infile)
    } else {
      println("loading parser")
      val parser = new StanfordParser()
      println("processing file")
      // A streaming API would be more efficient; but the parser already requires that the whole
      // input file reside in memory.
      val stringWriter = new StringWriter(Math.min(1024 * 1024, (infile.length() * 2).toInt))
      parser.processFile(infile, stringWriter)
      val tmpTtlFile = File.createTempFile("output", ".ttl")
      val tmpFileWriter = new FileOutputStream(tmpTtlFile)
      val tokenMap = try {
        StanfordXmlToTtl(
          new StringReader(stringWriter.toString()), new FileOutputStream(tmpTtlFile))
      } finally {
        tmpFileWriter.close()
      }
      println("wrote ttl output to " + tmpTtlFile)

      // Run Prolog on the ttl output file.
      val results = PrologExtractor(tmpTtlFile, tokenMap)
    }
  }
}
