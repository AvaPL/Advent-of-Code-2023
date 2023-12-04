package io.github.avapl
package day3

@main def puzzle2(): Unit = {
  val schematic = PuzzleInputParser.parsedInput
  val gearPairs = determineGearPairs(schematic)
  val gearRatios = gearPairs.map((first, second) => first.value * second.value)
  val result = gearRatios.sum
  println(result)
}

private def determineGearPairs(schematic: Schematic) = {
  val starSymbols = schematic.symbols.filter(_.value == '*')
  starSymbols
    .map(_.location)
    .map(getAdjacentNumbers(schematic.numbers))
    .collect { case first :: second :: Nil =>
      (first, second)
    }
}

private def getAdjacentNumbers(numbers: List[Number])(symbolLocation: Location) = {
  val adjacentLocations = getAdjacentLocations(symbolLocation)
  numbers.filter(number => adjacentLocations.exists(number.occupiedLocations.contains))
}

private def getAdjacentLocations(symbolLocation: Location)= {
  val Location(row, column) = symbolLocation
  for {
    rowIndex <- (row - 1) to (row + 1)
    columnIndex <- (column - 1) to (column + 1)
    location = Location(rowIndex, columnIndex)
    if location != symbolLocation
  } yield location
}
