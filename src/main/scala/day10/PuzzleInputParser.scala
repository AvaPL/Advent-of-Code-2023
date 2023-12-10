package io.github.avapl
package day10

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Grid](day = 10) {

  override protected def parse(string: String): Grid =
    for {
      line <- string.splitLines.toVector
    } yield for {
      tileChar <- line.toVector
    } yield tileChar match
      case '.' => Ground
      case 'S' => Start
      case '|' => UpDown
      case '-' => LeftRight
      case 'L' => UpRight
      case 'J' => UpLeft
      case '7' => DownLeft
      case 'F' => DownRight
}
