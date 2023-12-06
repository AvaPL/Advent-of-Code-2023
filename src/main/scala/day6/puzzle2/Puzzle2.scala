package io.github.avapl
package day6.puzzle2

import day6.determineButtonHoldTimesToWin

@main def puzzle2(): Unit = {
  val race = PuzzleInputParser.parsedInput
  val buttonHoldTimesToWin = determineButtonHoldTimesToWin(race)
  val result = buttonHoldTimesToWin.size
  println(result)
}
