package io.github.avapl
package day15.puzzle2

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Seq[Step]](day = 15) {

  override protected def parse(string: String): Seq[Step] =
    string.splitBy(",").map {
      case s"$label=$focalLength" => InsertLens(label, focalLength.toInt)
      case s"$label-"             => RemoveLens(label)
    }
}
