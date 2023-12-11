package io.github.avapl
package day11

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[GalaxyImage](day = 11) {

  override protected def parse(string: String): GalaxyImage =
    for {
      line <- string.splitLines.toVector
    } yield for {
      elementChar <- line.toVector
    } yield elementChar match
      case '.' => EmptySpace
      case '#' => Galaxy
}
