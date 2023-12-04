package io.github.avapl
package day4

@main def puzzle1(): Unit = {
  val cards = PuzzleInputParser.parsedInput
  val result = cards.map(calculatePoints).sum
  println(result)
}

private def calculatePoints(card: Card): Int = {
  val myWinningNumbersCount = card.winningNumbers.intersect(card.myNumbers).size
  if (myWinningNumbersCount == 0) 0
  else math.pow(2, myWinningNumbersCount - 1).toInt
}
