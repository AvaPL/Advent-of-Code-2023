package io.github.avapl
package day24

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Seq[Hailstone]](day = 24) {

  override protected def parse(string: String): Seq[Hailstone] =
    for {
      s"$x0, $y0, $_ @ $vx, $vy, $_" <- string.splitLines
    } yield Hailstone(
      initialPosition = Position(x0.toDouble, y0.toDouble),
      velocity = Velocity(vx.toDouble, vy.toDouble)
    )
}
