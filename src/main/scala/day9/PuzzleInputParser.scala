package io.github.avapl
package day9

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Seq[History]](day = 9) {

  override protected def parse(string: String): Seq[History] =
    for {
      line <- string.splitLines
    } yield line.splitBy(" ").map(_.toInt).toList
}
