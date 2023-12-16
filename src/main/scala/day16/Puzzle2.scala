package io.github.avapl
package day16

@main def puzzle2(): Unit = {
  val contraption = PuzzleInputParser.parsedInput
  val beamsFromTop = contraption.head.indices.map(column => (Position(0, column), Down))
  val beamsFromBottom = contraption.last.indices.map(column => (Position(0, column), Up))
  val beamsFromLeft = contraption.indices.map(row => (Position(row, 0), Right))
  val beamsFromRight = contraption.indices.map(row => (Position(row, contraption.head.size - 1), Left))
  val initialBeams = beamsFromTop ++ beamsFromBottom ++ beamsFromLeft ++ beamsFromRight
  val laserTraces = initialBeams.map(traceLaser(contraption, _, _))
  val energizedTilesCounts = laserTraces.map(countEnergizedTiles)
  val result = energizedTilesCounts.max
  println(result)
}
