package io.github.avapl
package day5.puzzle1

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Almanac](day = 5) {

  override protected def parse(string: String): Almanac = {
    val blocks = string.splitBlocks
    Almanac(
      seeds = parseSeeds(blocks(0)),
      seedToSoilMap = parseRangeMap(blocks(1)),
      soilToFertilizerMap = parseRangeMap(blocks(2)),
      fertilizerToWaterMap = parseRangeMap(blocks(3)),
      waterToLightMap = parseRangeMap(blocks(4)),
      lightToTemperatureMap = parseRangeMap(blocks(5)),
      temperatureToHumidityMap = parseRangeMap(blocks(6)),
      humidityToLocationMap = parseRangeMap(blocks(7))
    )
  }

  private def parseSeeds(block: String) =
    block.splitBy(" ").drop(1).map(_.toLong).toList

  private def parseRangeMap(block: String) = {
    val ranges = block.splitLines.drop(1).map(parseRange).toList
    RangeMap(ranges)
  }

  private def parseRange(line: String) = {
    val s"$destinationNumber $sourceNumber $length" = line
    RangeMapper(
      sourceNumber = sourceNumber.toLong,
      destinationNumber = destinationNumber.toLong,
      length = length.toLong
    )
  }
}
