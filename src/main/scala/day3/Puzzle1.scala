package io.github.avapl
package day3

@main def puzzle1(): Unit = {
  val schematic = PuzzleInputParser.parsedInput
  val partNumbers = determinePartNumbers(schematic)
  val result = partNumbers.map(_.value).sum
  println(result)
}

private def determinePartNumbers(schematic: Schematic) =
  schematic.numbers.filter(isPartNumber(schematic.symbols))

private def isPartNumber(symbols: List[Symbol])(number: Number) = {
  val adjacentLocations = getAdjacentLocations(number)
  symbols.exists(symbol => adjacentLocations.contains(symbol.location))
}

private def getAdjacentLocations(number: Number) = {
  val numberRowIndex = number.occupiedLocations.head.row
  for {
    rowIndex <- (numberRowIndex - 1) to (numberRowIndex + 1)
    columnIndex <- (number.occupiedLocations.head.column - 1) to (number.occupiedLocations.last.column + 1)
    location = Location(rowIndex, columnIndex)
    if !number.occupiedLocations.contains(location)
  } yield location
}
