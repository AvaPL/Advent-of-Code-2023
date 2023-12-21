package io.github.avapl
package day21

import com.softwaremill.quicklens._

@main def puzzle1(): Unit = {
  val (gardenMap, startingPosition) = PuzzleInputParser.parsedInput
  val garden = new Garden(gardenMap)
  val positionsAfter64Steps = garden.doNSteps(startingPosition, steps = 64)
  val result = positionsAfter64Steps.size
  println(result)
}

class Garden(gardenMap: GardenMap) {

  def doNSteps(startingPosition: Position, steps: Int): Set[Position] =
    (1 to steps).foldLeft(Set(startingPosition)) { (positions, _) =>
      doStep(positions)
    }

  private def doStep(positions: Set[Position]) =
    positions.flatMap(determinePossibleStepPositions)

  private def determinePossibleStepPositions(positions: Position) =
    Set(
      positions.modify(_.row).using(_ - 1),
      positions.modify(_.row).using(_ + 1),
      positions.modify(_.column).using(_ - 1),
      positions.modify(_.column).using(_ + 1)
    ).filter { case Position(row, column) =>
      row >= 0 &&
      row < gardenMap.size &&
      column >= 0 &&
      column < gardenMap.head.size &&
      gardenMap(row)(column) != Rock
    }
}
