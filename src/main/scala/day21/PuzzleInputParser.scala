package io.github.avapl
package day21

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[(GardenMap, Position)](day = 21) {

  override protected def parse(string: String): (GardenMap, Position) = {
    var startingPosition = Option.empty[Position]
    val gardenMap = for {
      (line, row) <- string.splitLines.toVector.zipWithIndex
    } yield for {
      (gardenElement, column) <- line.toVector.zipWithIndex
    } yield gardenElement match
      case '.' => GardenPlot
      case '#' => Rock
      case 'S' =>
        startingPosition = Some(Position(row, column))
        GardenPlot
    (gardenMap, startingPosition.get)
  }
}
