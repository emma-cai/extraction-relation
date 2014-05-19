package org.allenai.extraction.manager

import org.allenai.common.testkit.UnitSpec

import com.typesafe.config.ConfigFactory

import java.net.URI

class ProcessorIOTest extends UnitSpec {
  val devNull = "file:///dev/null"
  val devNullUri = new URI(devNull)

  // Tests for the main entry point (fromConfigValue).
  "ProcessorIO.fromConfigValue" should "handle a full object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {name: "a", uri: "${devNull}"}
      """)
    val io = ProcessorIO.fromConfigValue(config.getValue("io"), 0)
    io should be (ProcessorIO("a", devNullUri))
  }
  it should "handle a bareword name correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = "c"
      """)
    val io = ProcessorIO.fromConfigValue(config.getValue("io"), 3)
    io should be (ProcessorIO("c", new URI("name:c")))
  }
  it should "fail gracefully with a bad root object" in {
    val config = ConfigFactory.parseString(s"""
      io = [ { name: "IO is not an array" } ]
      """)
    an[ErmineException] should be thrownBy {
      ProcessorIO.fromConfigValue(config.getValue("io"), 0)
    }
  }

  // Tests for the helper function parsing objects (fromConfig).
  "ProcessorIO.fromConfig" should "handle a full object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {name: "a", uri: "${devNull}"}
      """)
    val io = ProcessorIO.fromConfig(config.getConfig("io"), "0")
    io should be (ProcessorIO("a", devNullUri))
  }
  it should "handle a name-only object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {name: "b"}
      """)
    val io = ProcessorIO.fromConfig(config.getConfig("io"), "1")
    io should be (ProcessorIO("b", new URI("name:b")))
  }
  it should "handle a uri-only object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {uri: "${devNull}"}
      """)
    val io = ProcessorIO.fromConfig(config.getConfig("io"), "2")
    io should be (ProcessorIO(ProcessorIO.unnamedName("2"), devNullUri))
  }
  it should "handle an empty object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {}
      """)
    val io = ProcessorIO.fromConfig(config.getConfig("io"), "10")
    io should be (ProcessorIO.unnamedIO("10"))
  }
  it should "fail gracefully with a bad name" in {
    val config = ConfigFactory.parseString(s"""
      io = { name: [] }
      """)
    an[ErmineException] should be thrownBy {
      ProcessorIO.fromConfig(config.getConfig("io"), "0")
    }
  }
  it should "fail gracefully with a bad URI" in {
    val config = ConfigFactory.parseString(s"""
      io = { uri: "not a legal URI" }
      """)
    an[ErmineException] should be thrownBy {
      ProcessorIO.fromConfig(config.getConfig("io"), "0")
    }
  }
}
