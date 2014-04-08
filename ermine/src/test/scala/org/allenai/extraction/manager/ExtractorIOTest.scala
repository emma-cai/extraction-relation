package org.allenai.extraction.manager

import org.allenai.common.testkit.UnitSpec

import com.typesafe.config.ConfigFactory

import java.net.URI

class ExtractorIOTest extends UnitSpec {
  val devNull = "file:///dev/null"
  val devNullUri = new URI(devNull)

  "ExtractorIO.fromConfig" should "handle a full object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {name: "a", uri: "${devNull}"}
      """)
    val io = ExtractorIO.fromConfig(config.getValue("io"), 0)
    io should be (ExtractorIO("a", Some(devNullUri)))
  }
  it should "handle a name-only object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {name: "b"}
      """)
    val io = ExtractorIO.fromConfig(config.getValue("io"), 1)
    io should be (ExtractorIO("b", None))
  }
  it should "handle a uri-only object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {uri: "${devNull}"}
      """)
    val io = ExtractorIO.fromConfig(config.getValue("io"), 2)
    io should be (ExtractorIO.defaultIO(2, Some(devNullUri)))
  }
  it should "handle a bareword name correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = "c"
      """)
    val io = ExtractorIO.fromConfig(config.getValue("io"), 3)
    io should be (ExtractorIO("c", None))
  }
  it should "handle an empty object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {}
      """)
    val io = ExtractorIO.fromConfig(config.getValue("io"), 10)
    io should be (ExtractorIO.defaultIO(10, None))
  }
  it should "fail gracefully with a bad name" in {
    val config = ConfigFactory.parseString(s"""
      io = { name: [] }
      """)
    an[ExtractionException] should be thrownBy {
      ExtractorIO.fromConfig(config.getValue("io"), 0)
    }
  }
  it should "fail gracefully with a bad URI" in {
    val config = ConfigFactory.parseString(s"""
      io = { uri: "not a legal URI" }
      """)
    an[ExtractionException] should be thrownBy {
      ExtractorIO.fromConfig(config.getValue("io"), 0)
    }
  }
  it should "fail gracefully with a bad root object" in {
    val config = ConfigFactory.parseString(s"""
      io = [ { name: "IO is not an array" } ]
      """)
    an[ExtractionException] should be thrownBy {
      ExtractorIO.fromConfig(config.getValue("io"), 0)
    }
  }
}
