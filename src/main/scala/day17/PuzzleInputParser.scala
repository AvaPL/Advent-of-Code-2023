package io.github.avapl
package day17

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[CityMap](day = 17) {

  override protected def parse(string: String): CityMap =
    for {
      line <- string.splitLines.toVector
    } yield for {
      heatCost <- line.toVector
    } yield heatCost.asDigit
}
