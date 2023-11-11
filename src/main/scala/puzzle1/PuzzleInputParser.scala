package io.github.avapl
package puzzle1

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Seq[ElfFood]](day = 1) {
  
  override def parse(string: String): Seq[ElfFood] =
    for {
      block <- string.splitBlocks
    } yield for {
      blockLine <- block.splitLines
    } yield blockLine.toInt
}
