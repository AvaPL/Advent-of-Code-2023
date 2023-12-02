package io.github.avapl
package day2

@main def puzzle1(): Unit = {
  val games = PuzzleInputParser.parsedInput
  val possibleGames = getPossibleGames(games)
  val result = possibleGames.map(_.id).sum
  println(result)
}

private def getPossibleGames(games: Seq[Game]) =
  games.filter { game =>
    game.cubeSubsets.forall { case Cubes(red, green, blue) =>
      red <= 12 && green <= 13 && blue <= 14
    }
  }
