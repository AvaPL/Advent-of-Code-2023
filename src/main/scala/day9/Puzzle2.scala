package io.github.avapl
package day9

@main def puzzle2(): Unit = {
  val histories = PuzzleInputParser.parsedInput
  val extrapolatedValues = histories.map(extrapolatePrevious)
  val result = extrapolatedValues.sum
  println(result)
}

private def extrapolatePrevious(history: History) = {
  val differences = calculateDifferences(history)
  extrapolateFirstValue(history, differences)
}

private def extrapolateFirstValue(history: History, differences: List[Vector[Int]]) =
  (history +: differences).reverse.foldLeft(0) { case (extrapolatedValue, differences) =>
    differences.head - extrapolatedValue
  }
