package io.github.avapl
package day25

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Seq[Edge]](day = 25) {

  override protected def parse(string: String): Seq[Edge] =
    for {
      s"$startNode: $endNodes" <- string.splitLines
      endNode <- endNodes.splitBy(" ")
    } yield Edge(startNode, endNode)

}
