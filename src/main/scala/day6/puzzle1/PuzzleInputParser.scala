package io.github.avapl
package day6.puzzle1

import day6.Race
import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[List[Race]](day = 6) {

  override protected def parse(string: String): List[Race] = {
    val lines = string.splitLines
    val durations = parseLine(lines(0))
    val recordDistances = parseLine(lines(1))
    durations
      .zip(recordDistances)
      .map { case (duration, recordDistance) =>
        Race(duration, recordDistance)
      }
      .toList
  }

  private def parseLine(line: String) =
    line
      .splitByRegex("\\s+")
      .drop(1)
      .map(_.toInt)
}
