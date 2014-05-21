package org.allenai.extraction.manager

import org.allenai.common.testkit.UnitSpec

import com.typesafe.config.ConfigFactory

import java.io.File

class ProcessorIoTest extends UnitSpec {
  val devNull = "file:///dev/null"
  val devNullFile = new File("/dev/null")

  // Tests for the main entry point (fromConfigValue).
  "ProcessorIo.fromConfigValue" should "handle a full object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {name: "a", uri: "${devNull}"}
      """)
    val io = ProcessorIo.fromConfigValue(config.getValue("io"), 0)
    io should be (new FileIo("a", false, devNullFile))
  }
  it should "handle a bareword name correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = "c"
      """)
    val io = ProcessorIo.fromConfigValue(config.getValue("io"), 3)
    io should be (new EphemeralIo("c", false))
  }
  it should "fail gracefully with a bad root object" in {
    val config = ConfigFactory.parseString(s"""
      io = [ { name: "IO is not an array" } ]
      """)
    an[ErmineException] should be thrownBy {
      ProcessorIo.fromConfigValue(config.getValue("io"), 0)
    }
  }

  // Tests for the helper function parsing objects (fromConfig).
  "ProcessorIo.fromConfig" should "handle a full object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {name: "a", uri: "${devNull}"}
      """)
    val io = ProcessorIo.fromConfig(config.getConfig("io"), "0")
    io should be (new FileIo("a", false, devNullFile))
  }
  it should "handle a name-only object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {name: "b"}
      """)
    val io = ProcessorIo.fromConfig(config.getConfig("io"), "1")
    io should be (new EphemeralIo("b", false))
  }
  it should "handle a uri-only object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {uri: "${devNull}"}
      """)
    val io = ProcessorIo.fromConfig(config.getConfig("io"), "2")
    io should be (new FileIo(ProcessorIo.unnamedKey("2"), true, devNullFile))
  }
  it should "handle an empty object correctly" in {
    val config = ConfigFactory.parseString(s"""
      io = {}
      """)
    val io = ProcessorIo.fromConfig(config.getConfig("io"), "10")
    io should be (ProcessorIo.unnamedIO("10"))
  }
  it should "fail gracefully with a bad name" in {
    val config = ConfigFactory.parseString(s"""
      io = { name: [] }
      """)
    an[ErmineException] should be thrownBy {
      ProcessorIo.fromConfig(config.getConfig("io"), "0")
    }
  }
  it should "fail gracefully with a bad URI" in {
    val config = ConfigFactory.parseString(s"""
      io = { uri: "not a legal URI" }
      """)
    an[ErmineException] should be thrownBy {
      ProcessorIo.fromConfig(config.getConfig("io"), "0")
    }
  }
}
