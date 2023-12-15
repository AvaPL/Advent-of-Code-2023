package io.github.avapl
package day12.puzzle2

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Seq[ConditionRecord]](day = 12) {

  override protected def parse(string: String): Seq[ConditionRecord] =
    for {
      line <- string.splitLines
    } yield {
      val s"$recordValue $damagedGroupSizesString" = line
      val damagedGroupSizes = damagedGroupSizesString.splitBy(",").map(_.toInt).toList
      val unfoldedValue = List.fill(5)(recordValue).mkString(unknownState.toString)
      val unfoldedDamagedGroupSizes = List.fill(5)(damagedGroupSizes).flatten
      ConditionRecord(unfoldedValue, unfoldedDamagedGroupSizes)
    }
}
