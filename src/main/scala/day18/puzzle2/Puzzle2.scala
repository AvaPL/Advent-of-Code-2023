package io.github.avapl
package day18.puzzle2

import day18.calculateTrenchArea

@main def puzzle2(): Unit = {
  val digPlan = PuzzleInputParser.parsedInput
  val result = calculateTrenchArea(digPlan)
  println(result)
}
