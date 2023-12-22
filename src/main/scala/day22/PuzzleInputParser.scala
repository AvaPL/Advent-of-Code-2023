package io.github.avapl
package day22

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Seq[Brick]](day = 22) {

  override protected def parse(string: String): Seq[Brick] =
    for {
      s"$x1,$y1,$z1~$x2,$y2,$z2" <- string.splitLines
    } yield {
      val start = Coordinate(x1.toInt, y1.toInt, z1.toInt)
      val end = Coordinate(x2.toInt, y2.toInt, z2.toInt)
      Brick(start, end)
    }
}
