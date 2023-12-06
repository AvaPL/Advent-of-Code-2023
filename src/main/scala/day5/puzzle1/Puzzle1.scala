package io.github.avapl
package day5.puzzle1

@main def puzzle1(): Unit = {
  val almanac = PuzzleInputParser.parsedInput
  val result = almanac.locations.min
  println(result)
}
