package io.github.avapl
package day7.part1

@main def puzzle1(): Unit = {
  val handsWithBids = PuzzleInputParser.parsedInput
  val orderedBidsAsc = handsWithBids
    .sortBy { case (hand, _) =>
      hand
    }
    .map { case (_, bid) =>
      bid
    }
  val winnings = orderedBidsAsc.zipWithIndex.map { case (bid, i) =>
    bid * (i + 1)
  }
  val result = winnings.sum
  println(result)
}
