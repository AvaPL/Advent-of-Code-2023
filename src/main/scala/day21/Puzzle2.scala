package io.github.avapl
package day21

import com.softwaremill.quicklens.*

@main def puzzle2(): Unit = {
  val (gardenMap, startingPosition) = PuzzleInputParser.parsedInput
  val infiniteGarden = new InfiniteGarden(gardenMap)
  // x=65  n=0
  val after65Steps = infiniteGarden.doNSteps(startingPosition, steps = 65).size
  // x=196 n=1
  val after196Steps = infiniteGarden.doNSteps(startingPosition, steps = 196).size
  // x=327 n=2
  val after327Steps = infiniteGarden.doNSteps(startingPosition, steps = 327).size
  // n = (x - 65) / 131, thus
  val nFinal = (26_501_365L - 65) / 131
  // The above values are results of a quadratic formula: an^2 + bn + c
  // Calculating a, b, c from the system of equations:
  // a * 0^2 + b * 0 + c = after65Steps  => c = after65Steps
  // a * 1^2 + b * 1 + c = after196Steps => a + b = after196Steps - c            => b = after196Steps - c - a
  // a * 2^2 + b * 2 + c = after327Steps => 2a + 2 * (a + b) + c = after327Steps => a = (after327Steps - 2 * after196Steps + c) / 2
  // Result:
  val a = (after327Steps - 2 * after196Steps + after65Steps) / 2
  val b = (4 * after196Steps - after327Steps - 3 * after65Steps) / 2
  val c = after65Steps
  val result = a * (nFinal * nFinal) + b * nFinal + c
  println(result)
}

class InfiniteGarden(gardenMap: GardenMap) {

  def doNSteps(startingPosition: Position, steps: Int): Set[Position] =
    (1 to steps).foldLeft(Set(startingPosition)) { (positions, step) =>
      doStep(gardenMap, positions)
    }

  private def doStep(gardenMap: GardenMap, positions: Set[Position]) =
    positions.flatMap(determinePossibleStepPositions)

  private def determinePossibleStepPositions(positions: Position) =
    Set(
      positions.modify(_.row).using(_ - 1),
      positions.modify(_.row).using(_ + 1),
      positions.modify(_.column).using(_ - 1),
      positions.modify(_.column).using(_ + 1)
    ).filter { case Position(row, column) =>
      gardenMap(math.floorMod(row, gardenMap.size))(math.floorMod(column, gardenMap.head.size)) != Rock
    }
}
