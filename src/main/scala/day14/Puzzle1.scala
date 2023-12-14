package io.github.avapl
package day14

@main def puzzle1(): Unit = {
  val platform = PuzzleInputParser.parsedInput
  val platformWithRocksNorth = moveRocksNorth(platform)
  val result = calculateLoad(platformWithRocksNorth)
  println(result)
}
