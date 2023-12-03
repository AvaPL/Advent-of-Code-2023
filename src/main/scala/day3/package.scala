package io.github.avapl
package day3

case class Schematic(
    numbers: List[Number],
    symbols: List[Symbol]
)

case class Number(
    value: Int,
    location: Location
) {
  val occupiedLocations: List[Location] = {
    val Location(row, column) = location
    for {
      offset <- 0 until value.toString.size
    } yield Location(row, column + offset)
  }.toList
}

case class Location(
    row: Int,
    column: Int
)

case class Symbol(
    value: Char,
    location: Location
)
