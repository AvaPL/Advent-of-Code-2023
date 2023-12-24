package io.github.avapl
package day24

@main def puzzle1(): Unit = {
  val hailstones = PuzzleInputParser.parsedInput
  val intersectionAreaMin = 200_000_000_000_000L
  val intersectionAreaMax = 400_000_000_000_000L
  val trajectoryIntersections = hailstones
    .combinations(2)
    .flatMap { case Seq(first, second) =>
      first.trajectoryIntersection(second)
    }
    .filter { case Position(x, y) =>
      intersectionAreaMin <= x && x <= intersectionAreaMax &&
      intersectionAreaMin <= y && y <= intersectionAreaMax
    }
    .toList
  val result = trajectoryIntersections.size
  println(result)
}
