package io.github.avapl
package day14

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Platform](day = 14) {

  override protected def parse(string: String): Platform =
    string.splitLines.toVector
}
