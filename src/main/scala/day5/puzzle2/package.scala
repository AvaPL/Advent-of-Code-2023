package io.github.avapl
package day5.puzzle2

import scala.collection.immutable.NumericRange
import scala.util.chaining.*

type RangeLong = NumericRange[Long]
type NotMappedRanges = List[RangeLong]

case class Almanac(
    seeds: List[RangeLong],
    seedToSoilMap: RangeMap,
    soilToFertilizerMap: RangeMap,
    fertilizerToWaterMap: RangeMap,
    waterToLightMap: RangeMap,
    lightToTemperatureMap: RangeMap,
    temperatureToHumidityMap: RangeMap,
    humidityToLocationMap: RangeMap
) {

  lazy val soils: List[RangeLong] = seedToSoilMap.mapRanges(seeds)
  lazy val fertilizers: List[RangeLong] = soilToFertilizerMap.mapRanges(soils)
  lazy val waters: List[RangeLong] = fertilizerToWaterMap.mapRanges(fertilizers)
  lazy val lights: List[RangeLong] = waterToLightMap.mapRanges(waters)
  lazy val temperatures: List[RangeLong] = lightToTemperatureMap.mapRanges(lights)
  lazy val humidities: List[RangeLong] = temperatureToHumidityMap.mapRanges(temperatures)
  lazy val locations: List[RangeLong] = humidityToLocationMap.mapRanges(humidities)
}

case class RangeMap(
    rangeMappers: List[RangeMapper]
) {

  def mapRanges(ranges: List[RangeLong]): List[RangeLong] = ???
}

case class RangeMapper(
    sourceNumber: Long,
    destinationNumber: Long,
    length: Long
) {

  val lastSourceNumber: Long = sourceNumber + length - 1

  def mapRange(range: RangeLong): (Option[RangeLong], NotMappedRanges) =
    if (areOverlapping(range)) {
      val maxStart = sourceNumber.max(range.start)
      val minEnd = lastSourceNumber.min(range.last)
      val leftNonOverlapping = range.start until maxStart
      val mappedOverlapping =
        (destinationNumber + (maxStart - sourceNumber)) to (destinationNumber + (minEnd - sourceNumber))
      val rightNonOverlapping = (minEnd + 1) to range.last
      (Some(mappedOverlapping), List(leftNonOverlapping, rightNonOverlapping).filter(_.nonEmpty))
    } else (None, List(range))

  // TODO: Does it have to be a separate method?
  def areOverlapping(range: RangeLong): Boolean = {
    val maxStart = sourceNumber.max(range.start)
    val minEnd = lastSourceNumber.min(range.last)
    maxStart <= minEnd
  }
}
