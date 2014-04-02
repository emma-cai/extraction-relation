package org.allenai.extraction.manager

import org.allenai.common.Logging
import org.allenai.extraction.stanford.PrologExtractor
import org.allenai.extraction.stanford.StanfordParser
import org.allenai.extraction.stanford.StanfordXmlToTtl

import com.typesafe.config.ConfigFactory

import scala.io.Source

import java.io.File
import java.io.FileWriter
import java.io.StringWriter

/** Main app to run extractions. */
object Ermine extends Logging {
  def main(args: Array[String]): Unit = {
    // Load up a config file.
    val config = ConfigFactory.load()

    // TODO(jkinkead): Validate the input.
    val infile = new File(config.getString("ermine.input.filename"))
    val outfile = new File(config.getString("ermine.output.filename"))

    if (!infile.canRead()) {
      // TODO(jkinkead): Better error handling.
      logger.error("bad infile: " + infile)
    } else {
      logger.info("loading parser")
      val parser = new StanfordParser()
      logger.info("processing file")
      // A streaming API would be more efficient; but the parser already requires that the whole
      // input file reside in memory.
      val stringWriter = new StringWriter(Math.min(1024 * 1024, (infile.length() * 2).toInt))
      parser.extract(Source.fromFile(infile), stringWriter)

      // Run Prolog on the Stanford parse.
      PrologExtractor.extract(Source.fromString(stringWriter.toString()), new FileWriter(outfile))
    }
  }
}
