package io.github.avapl
package day8

import scala.annotation.tailrec

@main def puzzle2(): Unit = {
  val (directions, network) = PuzzleInputParser.parsedInput
  val startNodes = determineStartNodes(network)
  val stepsStartToEnd = startNodes.map(countStepsStartToEndForNode(_, directions, network))
  val result = leastCommonMultiple(stepsStartToEnd) // I'm disappointed that this works :(
  println(result)
}

private def determineStartNodes(network: Network) =
  network.collect {
    case (node, _) if node.endsWith(startNodePostfix) => node
  }.toList

private def countStepsStartToEndForNode(node: Node, directions: Seq[Direction], network: Network): Long = {
  var stepsCount = 0L

  @tailrec
  def loop(currentNode: Node, remainingDirections: List[Direction]): Long =
    if (currentNode.endsWith(endNodeSuffix))
      stepsCount
    else
      remainingDirections match
        case direction :: tail =>
          stepsCount += 1
          val (leftNode, rightNode) = network(currentNode)
          direction match
            case Left  => loop(leftNode, tail)
            case Right => loop(rightNode, tail)
        case Nil => loop(currentNode, remainingDirections = directions.toList)

  loop(currentNode = node, remainingDirections = directions.toList)
}

@tailrec
private def leastCommonMultiple(longs: List[Long]): Long =
  longs match
    case first :: second :: tail => leastCommonMultiple(leastCommonMultiple(first, second) :: tail)
    case first :: Nil            => first

private def leastCommonMultiple(first: Long, second: Long) =
  first / greatestCommonDivisor(first, second) * second

@tailrec
private def greatestCommonDivisor(first: Long, second: Long): Long =
  if (second == 0) first
  else greatestCommonDivisor(second, first % second)

private lazy val startNodePostfix = "A"
private lazy val endNodeSuffix = "Z"
