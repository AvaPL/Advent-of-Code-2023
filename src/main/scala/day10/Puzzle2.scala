package io.github.avapl
package day10

import com.softwaremill.quicklens.*

import scala.collection.mutable

@main def puzzle2(): Unit = {
  val grid = PuzzleInputParser.parsedInput
  val (startTilePosition, startTileType) = determineStartTilePositionAndType(grid)
  val gridWithoutStartTile = grid.modify(_.at(startTilePosition.row).at(startTilePosition.column)).setTo(startTileType)
  val enlargedGrid = enlarge(gridWithoutStartTile)
  val enlargedStartTilePosition = startTilePosition.modifyAll(_.row, _.column).using(_ * 3 + 1)
  val loopPositions = getLoopPositions(enlargedGrid, enlargedStartTilePosition)
  val outsidePositions = determineOutsidePositions(enlargedGrid, loopPositions)
  val insidePositions = determineInsidePositions(enlargedGrid, loopPositions, outsidePositions)
  val result = insidePositions.size
  prettyPrintLoop(enlargedGrid, loopPositions, outsidePositions, insidePositions)
  println(result)
}

private def enlarge(grid: Grid) = {
  val enlargedGrid = for {
    row <- grid
  } yield for {
    column <- row
  } yield column match
    case Ground =>
      Vector(
        Vector(Ground, Ground, Ground),
        Vector(Ground, Ground, Ground),
        Vector(Ground, Ground, Ground)
      )
    case UpDown =>
      Vector(
        Vector(Ground, UpDown, Ground),
        Vector(Ground, UpDown, Ground),
        Vector(Ground, UpDown, Ground)
      )
    case LeftRight =>
      Vector(
        Vector(Ground, Ground, Ground),
        Vector(LeftRight, LeftRight, LeftRight),
        Vector(Ground, Ground, Ground)
      )
    case UpRight =>
      Vector(
        Vector(Ground, UpDown, Ground),
        Vector(Ground, UpRight, LeftRight),
        Vector(Ground, Ground, Ground)
      )
    case UpLeft =>
      Vector(
        Vector(Ground, UpDown, Ground),
        Vector(LeftRight, UpLeft, Ground),
        Vector(Ground, Ground, Ground)
      )
    case DownLeft =>
      Vector(
        Vector(Ground, Ground, Ground),
        Vector(LeftRight, DownLeft, Ground),
        Vector(Ground, UpDown, Ground)
      )
    case DownRight =>
      Vector(
        Vector(Ground, Ground, Ground),
        Vector(Ground, DownRight, LeftRight),
        Vector(Ground, UpDown, Ground)
      )
  enlargedGrid.flatMap { row =>
    val row0 = row.flatMap(_(0))
    val row1 = row.flatMap(_(1))
    val row2 = row.flatMap(_(2))
    Vector(row0, row1, row2)
  }
}

private def getLoopPositions(grid: Grid, startTilePosition: Position) = {
  val visitedPositions = mutable.Set[Position](startTilePosition)
  val startTileType = grid(startTilePosition.row)(startTilePosition.column).asInstanceOf[Pipe]
  var positionsToVisit = determinePositionsToVisit(startTilePosition, startTileType)
  while (positionsToVisit.nonEmpty) {
    positionsToVisit.foreach(visitedPositions.add)
    positionsToVisit = positionsToVisit
      .flatMap { position =>
        val pipeType = grid(position.row)(position.column).asInstanceOf[Pipe]
        determinePositionsToVisit(position, pipeType)
      }
      .filter(!visitedPositions.contains(_))
  }
  visitedPositions.toSet
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

private def determineOutsidePositions(enlargedGrid: Grid, loopPositions: Set[Position]) = {
  var positionsToVisit = List[Position](Position(0, 0))
  val visitedPositions = mutable.Set[Position](positionsToVisit*)
  while (positionsToVisit.nonEmpty) {
    positionsToVisit.foreach(visitedPositions.add)
    positionsToVisit = positionsToVisit
      .flatMap(determineOutsidePositionsToVisit(enlargedGrid, loopPositions))
      .distinct
      .filter(!visitedPositions.contains(_))
  }
  visitedPositions.toSet
}

private def determineOutsidePositionsToVisit(enlargedGrid: Grid, loopPositions: Set[Position])(position: Position) =
  List(
    position.modify(_.row).using(_ - 1),
    position.modify(_.row).using(_ + 1),
    position.modify(_.column).using(_ - 1),
    position.modify(_.column).using(_ + 1)
  ).filter { case position @ Position(row, column) =>
    row >= 0 &&
    row < enlargedGrid.size &&
    column >= 0 &&
    column < enlargedGrid.head.size &&
    !loopPositions.contains(position)
  }

private def determineInsidePositions(
    enlargedGrid: Grid,
    loopPositions: Set[Position],
    outsidePositions: Set[Position]
) = {
  val positionsToCheck = for {
    row <- 1 until enlargedGrid.size by 3
    column <- 1 until enlargedGrid.head.size by 3
  } yield Position(row, column)
  positionsToCheck.filter { position =>
    !loopPositions.contains(position) && !outsidePositions.contains(position)
  }.toSet
}

private def prettyPrintLoop(
    grid: Grid,
    loopPositions: Set[Position],
    outsidePositions: Set[Position],
    insidePositions: Set[Position]
): Unit =
  for {
    row <- grid.indices
  } for {
    column <- grid.head.indices
  } {
    if (loopPositions.contains(Position(row, column))) {
      val symbol = grid(row)(column) match
        case UpDown    => '║'
        case LeftRight => '═'
        case UpRight   => '╚'
        case UpLeft    => '╝'
        case DownLeft  => '╗'
        case DownRight => '╔'
      print(symbol)
    } else if (outsidePositions.contains(Position(row, column))) print('O')
    else if (insidePositions.contains(Position(row, column))) print('I')
    else print('.')
    if (column == grid.head.length - 1) println()
  }
