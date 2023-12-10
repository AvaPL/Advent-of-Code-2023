package io.github.avapl
package day10

type Grid = Vector[Vector[Tile]]

sealed trait Tile
case object Start extends Tile
case object Ground extends Tile
sealed trait Pipe extends Tile
case object UpDown extends Pipe
case object LeftRight extends Pipe
case object UpRight extends Pipe
case object UpLeft extends Pipe
case object DownLeft extends Pipe
case object DownRight extends Pipe

case class Position(
    row: Int,
    column: Int
)

def determineStartTilePositionAndType(grid: Grid): (Position, Pipe) = {
  val startTileRow = grid.indexWhere(_.contains(Start))
  val startTileColumn = grid(startTileRow).indexOf(Start)

  val hasConnectingPipeAbove = grid.lift(startTileRow - 1).map(_(startTileColumn)).exists {
    case UpDown | DownLeft | DownRight => true
    case _                             => false
  }
  val hasConnectingPipeBelow = grid.lift(startTileRow + 1).map(_(startTileColumn)).exists {
    case UpDown | UpRight | UpLeft => true
    case _                         => false
  }
  val hasConnectingPipeLeft = grid(startTileRow).lift(startTileColumn - 1).exists {
    case LeftRight | UpRight | DownRight => true
    case _                               => false
  }
  val startTileType =
    if (hasConnectingPipeAbove && hasConnectingPipeBelow) UpDown
    else if (hasConnectingPipeAbove && hasConnectingPipeLeft) UpLeft
    else if (hasConnectingPipeBelow && hasConnectingPipeLeft) DownLeft
    else if (hasConnectingPipeAbove) UpRight
    else if (hasConnectingPipeBelow) DownRight
    else LeftRight

  val startTilePosition = Position(startTileRow, startTileColumn)
  (startTilePosition, startTileType)
}
