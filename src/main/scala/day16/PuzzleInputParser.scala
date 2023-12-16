package io.github.avapl
package day16

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Contraption](day = 16) {

  override protected def parse(string: String): Contraption =
    for {
      line <- string.splitLines.toVector
    } yield for {
      char <- line.toVector
    } yield char match
      case '.'  => EmptySpace
      case '/'  => SlashMirror
      case '\\' => BackslashMirror
      case '|'  => VerticalSplitter
      case '-'  => HorizontalSplitter
}
