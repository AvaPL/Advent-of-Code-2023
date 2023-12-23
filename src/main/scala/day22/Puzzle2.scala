package io.github.avapl
package day22

import scala.collection.mutable
import scala.util.chaining.*

@main def puzzle2(): Unit = {
  val bricks = PuzzleInputParser.parsedInput
  val brickToCollidingBricks = dropBricks(bricks)
  val fallingChainCounts = brickToCollidingBricks.keys.toList.map(countFallingChain(brickToCollidingBricks))
  val result = fallingChainCounts.sum
  println(result)
}

private def countFallingChain(brickToCollidingBricks: Map[Brick, Set[Brick]])(brick: Brick) = {
  val desintegratedBricks = mutable.Set[Brick](brick)
  var remainingBricks = brickToCollidingBricks.removed(brick)
  var isChainReactionFinished = false

  while (!isChainReactionFinished) {
    val (newDesintegratedBricks, newRemainingBricks) = remainingBricks
      .partition { (_, collidingBricks) =>
        collidingBricks.nonEmpty && collidingBricks.subsetOf(desintegratedBricks)
      }
      .pipe((desintegrated, remaining) => (desintegrated.keySet, remaining))
    if (newDesintegratedBricks.nonEmpty) {
      desintegratedBricks.addAll(newDesintegratedBricks)
      remainingBricks = newRemainingBricks
    } else isChainReactionFinished = true
  }

  desintegratedBricks.size - 1 // don't include the initial brick
}
