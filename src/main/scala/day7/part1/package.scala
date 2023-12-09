package io.github.avapl
package day7.part1

import day7.*

import scala.math.Ordered.orderingToOrdered

case class Hand(
    cards: List[Card]
) {

  lazy val handType: HandType = {
    val countsDescending = cardToCount.values.toList.sorted(Ordering[Int].reverse)
    countsDescending match
      case 5 :: _                     => FiveOfAKind
      case 4 :: _                     => FourOfAKind
      case 3 :: 2 :: _                => FullHouse
      case 3 :: _                     => ThreeOfAKind
      case counts if counts.size == 3 => TwoPair
      case counts if counts.size == 4 => OnePair
      case counts if counts.size == 5 => HighCard
  }

  private lazy val cardToCount = cards
    .groupBy(identity)
    .map { case (card, group) =>
      (card, group.size)
    }
}

object Hand {

  implicit val ordering: Ordering[Hand] = Ordering.fromLessThan {
    case (left, right) if left.handType != right.handType => left.handType < right.handType
    case (left, right)                                    => left.cards < right.cards
  }
}

sealed trait Card
case object Ace extends Card
case object King extends Card
case object Queen extends Card
case object Jack extends Card
case object Numeral10 extends Card
case object Numeral9 extends Card
case object Numeral8 extends Card
case object Numeral7 extends Card
case object Numeral6 extends Card
case object Numeral5 extends Card
case object Numeral4 extends Card
case object Numeral3 extends Card
case object Numeral2 extends Card

object Card {

  implicit val ordering: Ordering[Card] = Ordering.by {
    case Ace       => 14
    case King      => 13
    case Queen     => 12
    case Jack      => 11
    case Numeral10 => 10
    case Numeral9  => 9
    case Numeral8  => 8
    case Numeral7  => 7
    case Numeral6  => 6
    case Numeral5  => 5
    case Numeral4  => 4
    case Numeral3  => 3
    case Numeral2  => 2
  }
}
