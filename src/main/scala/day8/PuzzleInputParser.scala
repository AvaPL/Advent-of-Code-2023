package io.github.avapl
package day8

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[(Seq[Direction], Network)](day = 8) {

  override protected def parse(string: String): (Seq[Direction], Network) = {
    val blocks = string.splitBlocks
    val directions = parseDirections(blocks(0))
    val network = parseNetwork(blocks(1))
    (directions, network)
  }

  private def parseDirections(directionsString: String) =
    directionsString.map {
      case 'R' => Right
      case 'L' => Left
    }

  private def parseNetwork(networkMapString: String) = {
    for {
      s"$source = ($left, $right)" <- networkMapString.splitLines
    } yield source -> (left, right)
  }.toMap
}
