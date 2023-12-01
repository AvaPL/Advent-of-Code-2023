package io.github.avapl
package day1

import scala.annotation.tailrec

@main def puzzle2(): Unit = {
  val lines = PuzzleInputParser.parsedInput
  val calibrationLines = lines
    .map(extractFirstAndLastDigit)
    .map { (firstDigit, lastDigit) =>
      firstDigit * 10 + lastDigit
    }
  val result = calibrationLines.sum
  println(result)
}

@tailrec
private def extractFirstAndLastDigit(string: String): (Int, Int) = {
  val firstDigit = digitsMap.collectFirst {
    case (digitString, value) if string.startsWith(digitString) => value
  }
  val lastDigit = digitsMap.collect {
    case (digitString, value) if string.endsWith(digitString) => value
  }.lastOption
  (firstDigit, lastDigit) match
    case (Some(firstDigit), Some(lastDigit)) => (firstDigit, lastDigit)
    case (None, _)                           => extractFirstAndLastDigit(string.tail)
    case (_, None)                           => extractFirstAndLastDigit(string.init)
}

private val digitsMap = Map(
  ("1", "one") -> 1,
  ("2", "two") -> 2,
  ("3", "three") -> 3,
  ("4", "four") -> 4,
  ("5", "five") -> 5,
  ("6", "six") -> 6,
  ("7", "seven") -> 7,
  ("8", "eight") -> 8,
  ("9", "nine") -> 9
).flatMap { case ((digitString1, digitString2), value) =>
  Map(digitString1 -> value, digitString2 -> value)
}
