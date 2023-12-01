package io.github.avapl
package day1

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Seq[String]](day = 1) {

  override def parse(string: String): Seq[String] =
    string.splitLines
}
