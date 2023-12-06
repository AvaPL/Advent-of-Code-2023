package io.github.avapl
package day6.puzzle1

import day6.determineButtonHoldTimesToWin

@main def puzzle1(): Unit = {
  val races = PuzzleInputParser.parsedInput
  val buttonHoldTimesToWin = races.map(determineButtonHoldTimesToWin)
  val result = buttonHoldTimesToWin.map(_.size).product
  println(result)
}
