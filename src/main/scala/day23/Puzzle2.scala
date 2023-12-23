package io.github.avapl
package day23

import com.softwaremill.quicklens.*

import scala.collection.mutable

@main def puzzle2(): Unit = {
  val hikingTrailsMap = PuzzleInputParser.parsedInput
  val startPosition = Position(0, 1)
  val endPosition = Position(hikingTrailsMap.indices.last, hikingTrailsMap.head.indices.last - 1)
  val result = new LongestPathIgnoringSlopes(hikingTrailsMap, startPosition, endPosition).length
  println(result)
}

class LongestPathIgnoringSlopes(hikingTrailsMap: HikingTrailsMap, startPosition: Position, endPosition: Position) {

  lazy val length: Int = {
    def loop(currentNode: Position, visitedNodes: Set[Position], currentPathLength: Int): Int =
      if (currentNode == endPosition)
        currentPathLength + 1 // include the end position
      else
        graphNeighbors
          .getOrElse(currentNode, Map.empty)
          .filterNot((step, _) => visitedNodes.contains(step))
          .map((step, pathLength) => loop(step, visitedNodes + step, currentPathLength + pathLength))
          .maxOption
          .getOrElse(-1)

    loop(currentNode = startPosition, visitedNodes = Set(startPosition), currentPathLength = -1)
  }

  private lazy val graphNeighbors = {
    val nodeToNeighbors = mutable.Map[Position, Map[Position, Int]]()
    graphNodes.foreach { node =>
      nodeToNeighbors.addOne(node -> determineNodeNeighbors(node))
    }
    nodeToNeighbors.toMap
  }

  private lazy val graphNodes = {
    for {
      (row, rowIndex) <- hikingTrailsMap.zipWithIndex
      (column, columnIndex) <- row.zipWithIndex
      position = Position(rowIndex, columnIndex)
      if isJunction(position)
    } yield position
  }.toSet + startPosition + endPosition

  private def isJunction(position: Position) =
    hikingTrailsMap(position.row)(position.column) == Path &&
      possibleSteps(position, visitedPositions = Set.empty).size >= 3

  private def possibleSteps(position: Position, visitedPositions: Set[Position]) =
    List(
      position.modify(_.row).using(_ - 1),
      position.modify(_.row).using(_ + 1),
      position.modify(_.column).using(_ - 1),
      position.modify(_.column).using(_ + 1)
    ).filter { case position @ Position(row, column) =>
      row >= 0 &&
      row < hikingTrailsMap.size &&
      column >= 0 &&
      column < hikingTrailsMap.head.size &&
      hikingTrailsMap(row)(column) != Forest &&
      !visitedPositions.contains(position)
    }

  private def determineNodeNeighbors(node: Position) = {
    val nodeNeighbors = mutable.Map[Position, Int]()

    def loop(currentPosition: Position, visitedPositions: Set[Position]): Unit =
      if (graphNodes.contains(currentPosition) && currentPosition != node)
        nodeNeighbors.addOne(currentPosition, visitedPositions.size - 1)
      else
        possibleSteps(currentPosition, visitedPositions)
          .foreach(step => loop(step, visitedPositions + step))

    loop(currentPosition = node, visitedPositions = Set(node))
    nodeNeighbors.toMap
  }
}
