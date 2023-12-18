package io.github.avapl
package day18

import scala.annotation.tailrec

type DigPlan = Seq[DigStep]

case class DigStep(
    direction: Direction,
    value: Long
)

sealed trait Direction
case object Up extends Direction
case object Down extends Direction
case object Left extends Direction
case object Right extends Direction

case class Position(
    row: Long,
    column: Long
) {

  def move(step: DigStep): Position = {
    val DigStep(direction, value) = step
    direction match
      case Up    => copy(row = row - value)
      case Down  => copy(row = row + value)
      case Left  => copy(column = column - value)
      case Right => copy(column = column + value)
  }
}

def calculateTrenchArea(digPlan: DigPlan) = {
  val trenchVertices = calculateTrenchVertices(digPlan)
  shoelaceArea(trenchVertices) + perimeter(digPlan) / 2 + 1
}

private def calculateTrenchVertices(digPlan: DigPlan) = {
  @tailrec
  def loop(digSteps: List[DigStep], trenchVertices: List[Position]): List[Position] =
    digSteps match
      case step :: tail =>
        val newVertex = trenchVertices.head.move(step)
        loop(tail, newVertex :: trenchVertices)
      case Nil => trenchVertices

  val initialVertex = Position(0, 0)
  loop(digPlan.toList, trenchVertices = List(initialVertex))
}

private def shoelaceArea(trenchVertices: List[Position]) =
  trenchVertices
    .sliding(2)
    .map { case List(Position(row1, column1), Position(row2, column2)) =>
      (row2 + row1) * (column2 - column1)
    }
    .sum / 2

private def perimeter(digPlan: DigPlan) =
  digPlan.map(_.value).sum
