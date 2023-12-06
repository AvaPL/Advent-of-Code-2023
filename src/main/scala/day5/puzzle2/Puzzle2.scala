package io.github.avapl
package day5.puzzle2

@main def puzzle2(): Unit = {
  val almanac = PuzzleInputParser.parsedInput
  println(s"almanac.seeds = ${almanac.seeds.map(_.mkString("(", ", ", ")"))}")
  println(s"almanac.soils = ${almanac.soils.map(_.mkString("(", ", ", ")"))}")
//  println(s"almanac.fertilizers = ${almanac.fertilizers.map(_.mkString("(", ", ", ")"))}")
//  println(s"almanac.waters = ${almanac.waters.map(_.mkString("(", ", ", ")"))}")
//  println(s"almanac.lights = ${almanac.lights.map(_.mkString("(", ", ", ")"))}")
//  println(s"almanac.temperatures = ${almanac.temperatures.map(_.mkString("(", ", ", ")"))}")
//  println(s"almanac.humidities = ${almanac.humidities.map(_.mkString("(", ", ", ")"))}")
//  println(s"almanac.locations = ${almanac.locations.map(_.mkString("(", ", ", ")"))}")
//  val result = almanac.locations.map(_.start).min
//  println(result)
}
