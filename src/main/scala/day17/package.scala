package io.github.avapl
package day17

import scala.collection.mutable

type HeatCost = Int
type CityMap = Vector[Vector[HeatCost]]
type Path = List[Position]

case class Position(
    row: Int,
    column: Int
)

sealed trait Direction {
  def cardinality: Int
  def isOppositeTo(other: Direction): Boolean
  def move(direction: Direction): Direction

  def isSameDirectionAs(other: Direction): Boolean =
    getClass == other.getClass
}

case class Up(cardinality: Int) extends Direction {

  override def isOppositeTo(other: Direction): Boolean =
    other.isInstanceOf[Down]

  override def move(direction: Direction): Direction =
    direction match
      case other: Up => copy(cardinality = cardinality + other.cardinality)
      case other     => other
}

case class Down(cardinality: Int) extends Direction {

  override def isOppositeTo(other: Direction): Boolean =
    other.isInstanceOf[Up]

  override def move(direction: Direction): Direction =
    direction match
      case other: Down => copy(cardinality = cardinality + other.cardinality)
      case other       => other
}

case class Left(cardinality: Int) extends Direction {

  override def isOppositeTo(other: Direction): Boolean =
    other.isInstanceOf[Right]

  override def move(direction: Direction): Direction =
    direction match
      case other: Left => copy(cardinality = cardinality + other.cardinality)
      case other       => other
}

case class Right(cardinality: Int) extends Direction {

  override def isOppositeTo(other: Direction): Boolean =
    other.isInstanceOf[Left]

  override def move(direction: Direction): Direction =
    direction match
      case other: Right => copy(cardinality = cardinality + other.cardinality)
      case other        => other
}

abstract class Dijkstra(cityMap: CityMap) {

  private val rowsCount = cityMap.size
  private val columnsCount = cityMap.head.size
  private val startPosition = Position(0, 0)
  private val endPosition = Position(cityMap.indices.last, cityMap.head.indices.last)

  lazy val costs: Vector[Vector[Int]] = {
    val costs = Array.fill(rowsCount)(
      Array.fill(columnsCount)(
        mutable.Map[Direction, HeatCost]().withDefaultValue(Int.MaxValue)
      )
    )
    costs(startPosition.row)(startPosition.column).put(Right(0), 0)
    val visited = mutable.PriorityQueue[(Position, Direction, HeatCost)]()(Ordering.by((_, _, cost) => cost))
    visited.enqueue((startPosition, Right(0), 0))
    visited.enqueue((startPosition, Down(0), 0))
    while (visited.nonEmpty) {
      val (currentPosition, currentDirection, currentCost) = visited.dequeue()
      getPossibleSteps(currentPosition, currentDirection).foreach {
        case (targetPosition @ Position(targetRow, targetColumn), targetDirection) =>
          val newCost = currentCost + cityMap(targetRow)(targetColumn)
          if (newCost < costs(targetRow)(targetColumn)(targetDirection) && newCost < heuristicFinalPathCost) {
            costs(targetRow)(targetColumn).put(targetDirection, newCost)
            visited.enqueue((targetPosition, targetDirection, newCost))
          }
      }
    }
    for {
      costsRow <- costs.toVector
    } yield for {
      costMap <- costsRow.toVector
    } yield costMap.map((_, cost) => cost).minOption.getOrElse(Int.MaxValue)
  }

  private def getPossibleSteps(position: Position, direction: Direction) = {
    val Position(row, column) = position
    Seq(
      (Position(row, column - 1), direction.move(Left(1))),
      (Position(row, column + 1), direction.move(Right(1))),
      (Position(row - 1, column), direction.move(Up(1))),
      (Position(row + 1, column), direction.move(Down(1)))
    ).collect {
      case step @ (newPosition, newDirection)
          if isStepValid(newPosition, previousDirection = direction, newDirection) =>
        step
    }
  }

  private def isStepValid(position: Position, previousDirection: Direction, newDirection: Direction) = {
    val Position(row, column) = position
    0 <= row && row < rowsCount &&
    0 <= column && column < columnsCount &&
    isDirectionChangeValid(previousDirection, newDirection)
  }

  protected def isDirectionChangeValid(previousDirection: Direction, newDirection: Direction): Boolean

  // Assumes that city map has the same number of rows as columns.
  // Traverses neighboring cities 1 by 1 and doesn't validate the steps, may not work for all inputs.
  private lazy val heuristicFinalPathCost = {
    for {
      row <- cityMap.indices
      column <- List(row, row + 1)
    } yield cityMap(row).lift(column).getOrElse(0)
  }.sum
}
