package io.github.avapl
package day11

@main def puzzle2(): Unit = {
  val galaxyImage = PuzzleInputParser.parsedInput
  val extensionMultiplier = 1_000_000L
  val result = calculateGalaxyDistancesSum(galaxyImage, extensionMultiplier)
  println(result)
}
