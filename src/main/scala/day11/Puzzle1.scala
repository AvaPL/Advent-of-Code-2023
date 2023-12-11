package io.github.avapl
package day11

@main def puzzle1(): Unit = {
  val galaxyImage = PuzzleInputParser.parsedInput
  val extensionMultiplier = 2L
  val result = calculateGalaxyDistancesSum(galaxyImage, extensionMultiplier)
  println(result)
}
