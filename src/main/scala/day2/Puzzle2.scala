package io.github.avapl
package day2

@main def puzzle2(): Unit = {
  val games = PuzzleInputParser.parsedInput
  val minimalCubes = calculateMinimalCubes(games)
  val powers = calculatePowers(minimalCubes)
  val result = powers.sum
  println(result)
}

private def calculateMinimalCubes(games: Seq[Game]) =
  games.map { case Game(_, cubeSubsets) =>
    val minimalRed = cubeSubsets.map(_.red).max
    val minimalGreen = cubeSubsets.map(_.green).max
    val minimalBlue = cubeSubsets.map(_.blue).max
    Cubes(minimalRed, minimalGreen, minimalBlue)
  }

private def calculatePowers(minimalCubes: Seq[Cubes]) =
  minimalCubes.map { case Cubes(red, green, blue) =>
    red * green * blue
  }
