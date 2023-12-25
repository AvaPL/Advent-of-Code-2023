package io.github.avapl
package day25

import scala.collection.mutable
import scala.util.Random

@main def puzzle1(): Unit = {
  val edges = PuzzleInputParser.parsedInput
  val random = Random(16) // Got the best performance with this seed
  val edgesToCut = findEdgesToCut(edges)(random)
  val finalEdges = edges.filterNot(edgesToCut.contains)
  val (group1Size, group2Size) = nodeGroupSizes(finalEdges)
  val result = group1Size * group2Size
  println(result)
}

/** Uses Karger's algorithm to find a cut of exactly 3 edges.
  * @see
  *   [[https://en.wikipedia.org/wiki/Karger%27s_algorithm]]
  */
private def findEdgesToCut(edges: Seq[Edge])(random: Random) = {
  case class CompactedEdge(node1: Node, node2: Node, source: Edge)

  var compactedEdges = mutable.ArrayBuffer.empty[CompactedEdge]

  while (compactedEdges.size != 3) {
    if (compactedEdges.isEmpty) // Reset, a cut of exactly 3 edges not found
      compactedEdges = mutable.ArrayBuffer.from(edges.map(e => CompactedEdge(e.node1, e.node2, source = e)).toList)
    else { // Compact two random nodes into one and remove edges between them
      val CompactedEdge(removedNode, compactedNode, _) = compactedEdges.remove(random.between(0, compactedEdges.size))
      compactedEdges = compactedEdges.flatMap {
        case CompactedEdge(`compactedNode`, `removedNode`, _) | CompactedEdge(`removedNode`, `compactedNode`, _) => Nil
        case CompactedEdge(`removedNode`, node2, source) => CompactedEdge(compactedNode, node2, source) :: Nil
        case CompactedEdge(node1, `removedNode`, source) => CompactedEdge(node1, compactedNode, source) :: Nil
        case edge                                        => edge :: Nil
      }
    }
  }

  compactedEdges.map(_.source).toList
}

private def toNodes(edges: Seq[Edge]) =
  edges.flatMap { case Edge(node1, node2) =>
    Seq(node1, node2)
  }.distinct

private def nodeGroupSizes(edges: Seq[Edge]) = {
  val nodeToNeighbors = mutable.Map.from(toNeighborsMap(edges))
  val visitedNodes = mutable.Set[Node]()
  val nodesToVisit = mutable.Queue[Node](nodeToNeighbors.keys.head)

  while (nodesToVisit.nonEmpty) {
    val node = nodesToVisit.dequeue()
    visitedNodes.add(node)
    val newNodesToVisit = nodeToNeighbors.getOrElse(node, Nil).filterNot(visitedNodes.contains)
    nodesToVisit.enqueueAll(newNodesToVisit)
    nodeToNeighbors.remove(node)
  }

  val group1Size = visitedNodes.size
  val group2Size = nodeToNeighbors.size
  (group1Size, group2Size)
}

private def toNeighborsMap(edges: Seq[Edge]) = {
  edges
    .flatMap { case Edge(node1, node2) =>
      List(node1 -> node2, node2 -> node1)
    }
    .groupMap(_._1)(_._2)
}
