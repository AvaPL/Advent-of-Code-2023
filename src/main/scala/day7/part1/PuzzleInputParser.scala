package io.github.avapl
package day7.part1

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Seq[(Hand, Bid)]](day = 7) {

  override protected def parse(string: String): Seq[(Hand, Bid)] =
    for {
      s"$handString $bidString" <- string.splitLines
    } yield {
      val hand = parseHand(handString)
      val bid = bidString.toInt
      (hand, bid)
    }

  private def parseHand(handString: String) = {
    val cards = handString.map {
      case 'A' => Ace
      case 'K' => King
      case 'Q' => Queen
      case 'J' => Jack
      case 'T' => Numeral10
      case '9' => Numeral9
      case '8' => Numeral8
      case '7' => Numeral7
      case '6' => Numeral6
      case '5' => Numeral5
      case '4' => Numeral4
      case '3' => Numeral3
      case '2' => Numeral2
    }
    Hand(cards.toList)
  }
}
