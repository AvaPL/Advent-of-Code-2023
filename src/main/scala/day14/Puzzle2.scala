package io.github.avapl
package day14

import scala.collection.mutable
import scala.util.chaining.*

@main def puzzle2(): Unit = {
  val platform = PuzzleInputParser.parsedInput
  val stateCycle = detectStateCycle(platform)
  val cyclesToCalculate = stateCycle.index + (1_000_000_000L - stateCycle.index) % stateCycle.length
  val finalPlatform = (0L until cyclesToCalculate).foldLeft(platform)((p, _) => cycle(p))
  val result = calculateLoad(finalPlatform)
  println(result)
}

private def detectStateCycle(platform: Platform) = {
  var currentState = platform
  val observedStates = mutable.LinkedHashSet[Platform]()
  var stateCycleIndex = Option.empty[Int]
  var stateCycleLength = Option.empty[Int]
  while (stateCycleLength.isEmpty) {
    observedStates.add(currentState)
    currentState = cycle(currentState)
    if (observedStates.contains(currentState)) {
      stateCycleIndex = Some(observedStates.iterator.indexOf(currentState))
      stateCycleLength = Some(observedStates.size - stateCycleIndex.get)
    }
  }
  StateCycle(stateCycleIndex.get, stateCycleLength.get)
}

private def cycle(platform: Platform) =
  platform
    .pipe(moveRocksNorth)
    .pipe(moveRocksWest)
    .pipe(moveRocksSouth)
    .pipe(moveRocksEast)

private def moveRocksSouth(platform: Platform) =
  platform.reverse
    .pipe(moveRocksNorth)
    .reverse

private def moveRocksEast(platform: Platform) =
  platform
    .map(_.reverse)
    .pipe(moveRocksWest)
    .map(_.reverse)

private case class StateCycle(index: Int, length: Int)
