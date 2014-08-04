package org.qingqing.relation.processors


object pipeline {
  def main(args: Array[String]) = {
    RunTrainingExtraction.main(null)
    RunDependencyPath.main(null)
  }
}