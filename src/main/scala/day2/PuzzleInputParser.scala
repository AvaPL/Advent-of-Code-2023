package io.github.avapl
package day2

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Seq[Game]](day = 2) {

  override protected def parse(string: String): Seq[Game] =
    for {
      s"Game $idString: $cubeSubsetsString" <- string.splitLines
    } yield {
      val id = idString.toInt
      val cubeSubsets = parseCubeSubsets(cubeSubsetsString)
      Game(id, cubeSubsets)
    }

  private def parseCubeSubsets(cubeSubsetsString: String) = 
    cubeSubsetsString
      .splitBy("; ")
      .map(parseCubeSubset)
      .toList

  private def parseCubeSubset(cubeSubsetString: String) = {
    val cubesString = cubeSubsetString.splitBy(", ")
    val redCubes = parseCubeCount(cubesString, "red")
    val greenCubes = parseCubeCount(cubesString, "green")
    val blueCubes = parseCubeCount(cubesString, "blue")
    Cubes(redCubes, greenCubes, blueCubes)
  }

  private def parseCubeCount(cubesString: Seq[String], color: String) =
    cubesString
      .collectFirst {
        case s"$count $colorString" if colorString == color =>
          count.toInt
      }
      .getOrElse(0)
}
