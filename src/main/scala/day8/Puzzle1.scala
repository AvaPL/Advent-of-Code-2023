package io.github.avapl
package day8

import scala.annotation.tailrec

@main def puzzle1(): Unit = {
  val (directions, network) = PuzzleInputParser.parsedInput
  val result = countStepsStartToEnd(directions, network)
  println(result)
}

private def countStepsStartToEnd(directions: Seq[Direction], network: Network): Int = {
  var stepsCount = 0

  @tailrec
  def loop(currentNode: Node, remainingDirections: List[Direction]): Int =
    currentNode match
      case node if node == endNode => stepsCount
      case node =>
        remainingDirections match
          case direction :: tail =>
            stepsCount += 1
            val (leftNode, rightNode) = network(currentNode)
            direction match
              case Left  => loop(leftNode, tail)
              case Right => loop(rightNode, tail)
          case Nil => loop(currentNode, remainingDirections = directions.toList)

  loop(currentNode = startNode, remainingDirections = directions.toList)
}

private lazy val startNode = "AAA"
private lazy val endNode = "ZZZ"
