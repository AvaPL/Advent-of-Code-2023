package io.github.avapl
package day16

import scala.collection.mutable

type Contraption = Vector[Vector[ContraptionElement]]

sealed trait ContraptionElement {
  def transformDirection(direction: BeamDirection): List[BeamDirection]
}

case object EmptySpace extends ContraptionElement {
  override def transformDirection(direction: BeamDirection): List[BeamDirection] = List(direction)
}

sealed trait Mirror extends ContraptionElement

case object SlashMirror extends Mirror {
  override def transformDirection(direction: BeamDirection): List[BeamDirection] = List(
    direction match
      case Up    => Right
      case Down  => Left
      case Left  => Down
      case Right => Up
  )
}

case object BackslashMirror extends Mirror {
  override def transformDirection(direction: BeamDirection): List[BeamDirection] = List(
    direction match
      case Up    => Left
      case Down  => Right
      case Left  => Up
      case Right => Down
  )
}

sealed trait Splitter extends ContraptionElement

case object VerticalSplitter extends Splitter {
  override def transformDirection(direction: BeamDirection): List[BeamDirection] =
    direction match
      case Up | Down    => List(direction)
      case Left | Right => List(Up, Down)
}

case object HorizontalSplitter extends Splitter {
  override def transformDirection(direction: BeamDirection): List[BeamDirection] =
    direction match
      case Left | Right => List(direction)
      case Up | Down    => List(Left, Right)
}

sealed trait BeamDirection
case object Up extends BeamDirection
case object Down extends BeamDirection
case object Left extends BeamDirection
case object Right extends BeamDirection

case class Position(
    row: Int,
    column: Int
) {

  def shift(direction: BeamDirection): Position =
    direction match
      case Up    => copy(row = row - 1)
      case Down  => copy(row = row + 1)
      case Left  => copy(column = column - 1)
      case Right => copy(column = column + 1)
}

def traceLaser(contraption: Contraption, initialBeamPosition: Position, initialBeamDirection: BeamDirection) = {
  val contraptionBeams = contraption.map(_.map(_ => mutable.Set[BeamDirection]()))
  val beamsToEvaluate = mutable.Queue[(Position, BeamDirection)]((initialBeamPosition, initialBeamDirection))

  def isPositionValid(position: Position) =
    position.row >= 0 &&
      position.row < contraption.size &&
      position.column >= 0 &&
      position.column < contraption.head.size

  while (beamsToEvaluate.nonEmpty) {
    val (position, beamDirection) = beamsToEvaluate.dequeue()
    val wasBeamInserted = contraptionBeams(position.row)(position.column).add(beamDirection)
    if (wasBeamInserted) {
      val newDirections = contraption(position.row)(position.column).transformDirection(beamDirection)
      val newBeamsToEvaluate = newDirections
        .map(direction => (position.shift(direction), direction))
        .filter((position, _) => isPositionValid(position))
      newBeamsToEvaluate.foreach(beamsToEvaluate.enqueue)
    }
  }

  contraptionBeams.map(_.map(_.toSet))
}

def countEnergizedTiles(laserTraces: Vector[Vector[Set[BeamDirection]]]) =
  laserTraces.map(_.count(_.nonEmpty)).sum
