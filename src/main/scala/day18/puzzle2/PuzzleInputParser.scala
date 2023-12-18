package io.github.avapl
package day18.puzzle2

import day18.*
import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[DigPlan](day = 18) {

  override protected def parse(string: String): DigPlan =
    for {
      s"$_(#$hexString)" <- string.splitLines
    } yield {
      val hexValue = BigInt(hexString.take(5), 16).toLong
      val direction = parseDirection(hexString(5))
      DigStep(direction, hexValue)
    }

  private def parseDirection(char: Char) =
    char match
      case '0' => Right
      case '1' => Down
      case '2' => Left
      case '3' => Up
}
