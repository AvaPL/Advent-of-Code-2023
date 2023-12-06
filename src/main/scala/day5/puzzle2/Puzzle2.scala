package io.github.avapl
package day5.puzzle2

@main def puzzle2(): Unit = {
  val almanac = PuzzleInputParser.parsedInput
  val result = almanac.locations.map(_.start).min
  println(result)
}
