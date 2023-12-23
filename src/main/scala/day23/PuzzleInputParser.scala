package io.github.avapl
package day23

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[HikingTrailsMap](day = 23) {

  override protected def parse(string: String): HikingTrailsMap =
    for {
      line <- string.splitLines.toVector
    } yield for {
      trailElement <- line.toVector
    } yield trailElement match
      case '.' => Path
      case '#' => Forest
      case '^' => SlopeUp
      case 'v' => SlopeDown
      case '<' => SlopeLeft
      case '>' => SlopeRight
}
