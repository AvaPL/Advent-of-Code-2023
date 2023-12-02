package io.github.avapl
package day1

@main def puzzle1(): Unit = {
  val lines = PuzzleInputParser.parsedInput
  val calibrationValues = lines
    .map(extractDigits)
    .map { digits =>
      digits.head * 10 + digits.last
    }
  val result = calibrationValues.sum
  println(result)
}

private def extractDigits(string: String) =
  string.filter(_.isDigit).map(_.asDigit)
