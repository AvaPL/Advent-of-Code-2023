package io.github.avapl
package day6.puzzle2

import day6.Race
import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Race](day = 6) {

  override protected def parse(string: String): Race = {
    val lines = string.splitLines
    val duration = parseLine(lines(0))
    val recordDistance = parseLine(lines(1))
    Race(duration, recordDistance)
  }

  private def parseLine(line: String) =
    line
      .splitByRegex("\\s+")
      .drop(1)
      .mkString
      .toLong
}
