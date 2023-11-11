package io.github.avapl
package puzzle1

@main def puzzle1() = {
  val elfFoods = PuzzleInputParser.parsedInput
  val result = elfFoods.map(_.sum).max
  println(result)
}
