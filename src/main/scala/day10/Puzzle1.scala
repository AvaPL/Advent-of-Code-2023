package io.github.avapl
package day10

import scala.collection.mutable
import com.softwaremill.quicklens.*

@main def puzzle1(): Unit = {
  val grid = PuzzleInputParser.parsedInput
  val (startTilePosition, startTileType) = determineStartTilePositionAndType(grid)
  val result = calculateLongestPath(grid, startTilePosition, startTileType)
  println(result)
}

private def calculateLongestPath(grid: Grid, startTilePosition: Position, startTileType: Pipe) = {
  val visitedPositions = mutable.Set[Position](startTilePosition)
  var positionsToVisit = determinePositionsToVisit(startTilePosition, startTileType)
  var currentCost = 0
  while (positionsToVisit.nonEmpty) {
    positionsToVisit.foreach(visitedPositions.add)
    positionsToVisit = positionsToVisit
      .flatMap { position =>
        val pipeType = grid(position.row)(position.column).asInstanceOf[Pipe]
        determinePositionsToVisit(position, pipeType)
      }
      .filter(!visitedPositions.contains(_))
    currentCost += 1
  }
  currentCost
}

private def determinePositionsToVisit(position: Position, pipeType: Pipe) =
  pipeType match
    case UpDown =>
      List(
        position.modify(_.row).using(_ + 1),
        position.modify(_.row).using(_ - 1)
      )
    case LeftRight =>
      List(
        position.modify(_.column).using(_ + 1),
        position.modify(_.column).using(_ - 1)
      )
    case UpRight =>
      List(
        position.modify(_.row).using(_ - 1),
        position.modify(_.column).using(_ + 1)
      )
    case UpLeft =>
      List(
        position.modify(_.row).using(_ - 1),
        position.modify(_.column).using(_ - 1)
      )
    case DownLeft =>
      List(
        position.modify(_.row).using(_ + 1),
        position.modify(_.column).using(_ - 1)
      )
    case DownRight =>
      List(
        position.modify(_.row).using(_ + 1),
        position.modify(_.column).using(_ + 1)
      )
