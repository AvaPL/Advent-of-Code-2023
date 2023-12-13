package io.github.avapl
package day13

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Seq[Pattern]](day = 13) {

  override protected def parse(string: String): Seq[Pattern] =
    for {
      block <- string.splitBlocks
    } yield block.splitLines.toVector
}
