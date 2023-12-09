package io.github.avapl
package day7

type Bid = Int

sealed trait HandType
case object FiveOfAKind extends HandType
case object FourOfAKind extends HandType
case object FullHouse extends HandType
case object ThreeOfAKind extends HandType
case object TwoPair extends HandType
case object OnePair extends HandType
case object HighCard extends HandType

object HandType {

  implicit val ordering: Ordering[HandType] = Ordering.by {
    case FiveOfAKind  => 7
    case FourOfAKind  => 6
    case FullHouse    => 5
    case ThreeOfAKind => 4
    case TwoPair      => 3
    case OnePair      => 2
    case HighCard     => 1
  }
}
