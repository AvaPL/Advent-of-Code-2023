package io.github.avapl
package day15.puzzle1

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[InitializationSequence](day = 15) {

  override protected def parse(string: String): InitializationSequence =
    string.splitBy(",").toVector
}
