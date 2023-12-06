package io.github.avapl
package day5.puzzle1

case class Almanac(
    seeds: List[Long],
    seedToSoilMap: RangeMap,
    soilToFertilizerMap: RangeMap,
    fertilizerToWaterMap: RangeMap,
    waterToLightMap: RangeMap,
    lightToTemperatureMap: RangeMap,
    temperatureToHumidityMap: RangeMap,
    humidityToLocationMap: RangeMap
) {

  lazy val soils: List[Long] = seeds.map(seedToSoilMap.mapNumber)
  lazy val fertilizers: List[Long] = soils.map(soilToFertilizerMap.mapNumber)
  lazy val waters: List[Long] = fertilizers.map(fertilizerToWaterMap.mapNumber)
  lazy val lights: List[Long] = waters.map(waterToLightMap.mapNumber)
  lazy val temperatures: List[Long] = lights.map(lightToTemperatureMap.mapNumber)
  lazy val humidities: List[Long] = temperatures.map(temperatureToHumidityMap.mapNumber)
  lazy val locations: List[Long] = humidities.map(humidityToLocationMap.mapNumber)
}

case class RangeMap(
    rangeMappers: List[RangeMapper]
) {

  def mapNumber(number: Long): Long =
    rangeMappers
      .find(_.contains(number))
      .map(_.mapNumber(number))
      .getOrElse(number)
}

case class RangeMapper(
    sourceNumber: Long,
    destinationNumber: Long,
    length: Long
) {

  def mapNumber(number: Long): Long =
    if (contains(number))
      destinationNumber + (number - sourceNumber)
    else number

  def contains(number: Long): Boolean =
    sourceNumber <= number && number < sourceNumber + length
}
