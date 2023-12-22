package io.github.avapl
package day22

@main def puzzle1(): Unit = {
  val bricks = PuzzleInputParser.parsedInput
  val brickToCollidingBricks = dropBricks(bricks)
  val desintegratableBricks = getDesintegratableBricks(brickToCollidingBricks)
  val result = desintegratableBricks.size
  println(result)
}

private def getDesintegratableBricks(brickToCollidingBricks: Map[Brick, Set[Brick]]) = {
  val nonDesintegratableBricks = brickToCollidingBricks.collect {
    case (_, collidingBricks) if collidingBricks.size == 1 =>
      collidingBricks.head // the only brick that holds the brick above
  }.toSet
  brickToCollidingBricks.keySet.diff(nonDesintegratableBricks)
}
