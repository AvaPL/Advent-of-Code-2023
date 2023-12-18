package io.github.avapl
package day18.puzzle1

import day18.*
import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[DigPlan](day = 18) {

  override protected def parse(string: String): DigPlan =
    for {
      s"$directionString $value (#$_)" <- string.splitLines
    } yield {
      val direction = parseDirection(directionString)
      DigStep(direction, value.toInt)
    }

  private def parseDirection(string: String) =
    string match
      case "U" => Up
      case "D" => Down
      case "L" => Left
      case "R" => Right
}
