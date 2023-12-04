package io.github.avapl
package day4

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Seq[Card]](day = 4) {

  private val cardLineRegex = "Card\\s+(\\d+):([\\s\\d]+)\\|([\\s\\d]*)".r

  override protected def parse(string: String): Seq[Card] =
    for {
      line <- string.splitLines
    } yield {
      val cardLineRegex(idString, winningNumbersString, myNumbersString) = line
      val id = idString.toInt
      val winningNumbers = winningNumbersString.trim.splitByRegex("\\s+").map(_.toInt).toSet
      val myNumbers = myNumbersString.trim.splitByRegex("\\s+").map(_.toInt).toSet
      Card(id, winningNumbers, myNumbers)
    }
}
