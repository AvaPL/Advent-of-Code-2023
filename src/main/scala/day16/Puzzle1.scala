package io.github.avapl
package day16

@main def puzzle1(): Unit = {
  val contraption = PuzzleInputParser.parsedInput
  val (initialBeamPosition, initialBeamDirection) = (Position(0, 0), Right)
  val laserTraces = traceLaser(contraption, initialBeamPosition, initialBeamDirection)
  val result = countEnergizedTiles(laserTraces)
  println(result)
}
