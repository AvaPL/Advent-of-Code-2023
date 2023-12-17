package io.github.avapl
package day17

// Requires a few minutes to run
@main def puzzle2(): Unit = {
  val cityMap = PuzzleInputParser.parsedInput
  val costs = new Dijkstra(cityMap) {
    override protected def isDirectionChangeValid(previousDirection: Direction, newDirection: Direction): Boolean =
      !newDirection.isOppositeTo(previousDirection) &&
        (newDirection.isSameDirectionAs(previousDirection) || previousDirection.cardinality >= 4) &&
        newDirection.cardinality <= 10
  }.costs
  val result = costs.last.last
  println(result)
}
