package io.github.avapl
package day18.puzzle1

import day18.calculateTrenchArea

@main def puzzle1(): Unit = {
  val digPlan = PuzzleInputParser.parsedInput
  val result = calculateTrenchArea(digPlan)
  println(result)
}
