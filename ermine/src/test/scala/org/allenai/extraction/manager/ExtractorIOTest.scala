package org.allenai.extraction.manager

import org.allenai.common.testkit.UnitSpec

import com.typesafe.config.ConfigFactory

import java.net.URI

class ExtractorIOTest extends UnitSpec {
  val devNull = "file:///dev/null"
  val devNullUri = new URI(devNull)

  // Tests for the main entry point (fromConfigValue).
  "ExtractorIO.fromConfigValue" should "handle a full object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {name: "a", uri: "${devNull}"}
      """)
    val io = ExtractorIO.fromConfigValue(config.getValue("io"), 0)
    io should be (ExtractorIO("a", devNullUri))
  }
  it should "handle a bareword name correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = "c"
      """)
    val io = ExtractorIO.fromConfigValue(config.getValue("io"), 3)
    io should be (ExtractorIO("c", new URI("name:c")))
  }
  it should "fail gracefully with a bad root object" in {
    val config = ConfigFactory.parseString(s"""
      io = [ { name: "IO is not an array" } ]
      """)
    an[ExtractionException] should be thrownBy {
      ExtractorIO.fromConfigValue(config.getValue("io"), 0)
    }
  }

  // Tests for the helper function parsing objects (fromConfig).
  "ExtractorIO.fromConfig" should "handle a full object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {name: "a", uri: "${devNull}"}
      """)
    val io = ExtractorIO.fromConfig(config.getConfig("io"), "0")
    io should be (ExtractorIO("a", devNullUri))
  }
  it should "handle a name-only object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {name: "b"}
      """)
    val io = ExtractorIO.fromConfig(config.getConfig("io"), "1")
    io should be (ExtractorIO("b", new URI("name:b")))
  }
  it should "handle a uri-only object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {uri: "${devNull}"}
      """)
    val io = ExtractorIO.fromConfig(config.getConfig("io"), "2")
    io should be (ExtractorIO(ExtractorIO.defaultName("2"), devNullUri))
  }
  it should "handle an empty object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {}
      """)
    val io = ExtractorIO.fromConfig(config.getConfig("io"), "10")
    io should be (ExtractorIO.defaultIO("10"))
  }
  it should "fail gracefully with a bad name" in {
    val config = ConfigFactory.parseString(s"""
      io = { name: [] }
      """)
    an[ExtractionException] should be thrownBy {
      ExtractorIO.fromConfig(config.getConfig("io"), "0")
    }
  }
  it should "fail gracefully with a bad URI" in {
    val config = ConfigFactory.parseString(s"""
      io = { uri: "not a legal URI" }
      """)
    an[ExtractionException] should be thrownBy {
      ExtractorIO.fromConfig(config.getConfig("io"), "0")
    }
  }
}
