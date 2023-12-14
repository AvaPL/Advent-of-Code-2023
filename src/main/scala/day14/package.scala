package io.github.avapl
package day14

import scala.annotation.tailrec
import scala.util.chaining.*

type Platform = Vector[String]

val roundedRock = 'O'
val cubeRock = '#'
val emptySpace = '.'

def moveRocksWest(platform: Platform) =
  platform.map(moveRocksLeft)

@tailrec
private def moveRocksLeft(row: String): String = {
  val firstRockMovedLeft = row.replaceFirst(s"\\$emptySpace$roundedRock", s"$roundedRock$emptySpace")
  if (firstRockMovedLeft == row) row // no change, all rocks on the left
  else moveRocksLeft(firstRockMovedLeft)
}

def moveRocksNorth(platform: Platform) =
  platform
    .pipe(transposePlatform)
    .pipe(moveRocksWest)
    .pipe(transposePlatform)

private def transposePlatform(platform: Platform) =
  platform.transpose.map(_.mkString)

def calculateLoad(platform: Platform) =
  platform.reverse.zipWithIndex.map { case (row, i) =>
    row.count(_ == roundedRock) * (i + 1)
  }.sum
