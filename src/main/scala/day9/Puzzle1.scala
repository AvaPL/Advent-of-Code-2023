package io.github.avapl
package day9

@main def puzzle1(): Unit = {
  val histories = PuzzleInputParser.parsedInput
  val extrapolatedValues = histories.map(extrapolateNext)
  val result = extrapolatedValues.sum
  println(result)
}

private def extrapolateNext(history: History) = {
  val differences = calculateDifferences(history)
  extrapolateLastValue(history, differences)
}

private def extrapolateLastValue(history: History, differences: List[Vector[Int]]) =
  (history +: differences).foldLeft(0) { case (extrapolatedValue, differences) =>
    extrapolatedValue + differences.last
  }
