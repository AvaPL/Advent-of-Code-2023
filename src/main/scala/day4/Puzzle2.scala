package io.github.avapl
package day4

import scala.annotation.tailrec

@main def puzzle2(): Unit = {
  val cards = PuzzleInputParser.parsedInput
  val result = calculateTotalCardsAfterEvaluation(cards)
  println(result)
}

private def calculateTotalCardsAfterEvaluation(cards: Seq[Card]) = {
  val cardsToEvaluate = cards.toList
  val initialCardIdToCount = cards.map(_.id -> 1).toMap
  val evaluatedCardIdToCount = countEvaluatedCards(cardsToEvaluate, initialCardIdToCount)
  evaluatedCardIdToCount.map((_, count) => count).sum
}

@tailrec
private def countEvaluatedCards(cardsToEvaluate: List[Card], cardIdToCount: Map[Int, Int]): Map[Int, Int] =
  cardsToEvaluate match
    case Nil => cardIdToCount
    case currentCard :: tail =>
      val myWinningNumbersCount = currentCard.winningNumbers.intersect(currentCard.myNumbers).size
      val wonCopiesCardIds = (currentCard.id + 1) to (currentCard.id + myWinningNumbersCount)
      val newCardIdToCount = cardIdToCount.map {
        case (cardId, count) if wonCopiesCardIds.contains(cardId) =>
          val newCount = count + cardIdToCount(currentCard.id)
          cardId -> newCount
        case old => old
      }
      countEvaluatedCards(tail, newCardIdToCount)
