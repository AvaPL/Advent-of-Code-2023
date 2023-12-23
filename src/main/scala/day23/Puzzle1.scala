package io.github.avapl
package day23

import com.softwaremill.quicklens.*

@main def puzzle1(): Unit = {
  val hikingTrailsMap = PuzzleInputParser.parsedInput
  val startPosition = Position(0, 1)
  val endPosition = Position(hikingTrailsMap.indices.last, hikingTrailsMap.head.indices.last - 1)
  val result = new LongestPath(hikingTrailsMap, startPosition, endPosition).length
  println(result)
}

class LongestPath(hikingTrailsMap: HikingTrailsMap, startPosition: Position, endPosition: Position) {

  lazy val length: Int = {
    def loop(currentPosition: Position, visitedPositions: Set[Position]): Int =
      if (currentPosition == endPosition)
        visitedPositions.size - 1 // don't include the start position
      else
        possibleSteps(currentPosition, visitedPositions)
          .map(step => loop(step, visitedPositions + step))
          .maxOption
          .getOrElse(-1)

    loop(currentPosition = startPosition, visitedPositions = Set(startPosition))
  }

  private def possibleSteps(position: Position, visitedPositions: Set[Position]) = {
    val steps = hikingTrailsMap(position.row)(position.column) match
      case SlopeUp    => List(position.modify(_.row).using(_ - 1))
      case SlopeDown  => List(position.modify(_.row).using(_ + 1))
      case SlopeLeft  => List(position.modify(_.column).using(_ - 1))
      case SlopeRight => List(position.modify(_.column).using(_ + 1))
      case Path =>
        List(
          position.modify(_.row).using(_ - 1),
          position.modify(_.row).using(_ + 1),
          position.modify(_.column).using(_ - 1),
          position.modify(_.column).using(_ + 1)
        )
    steps.filter { case position @ Position(row, column) =>
      row >= 0 &&
      row < hikingTrailsMap.size &&
      column >= 0 &&
      column < hikingTrailsMap.head.size &&
      hikingTrailsMap(row)(column) != Forest &&
      !visitedPositions.contains(position)
    }
  }
}
