package io.github.avapl
package day22

import com.softwaremill.quicklens.*

import scala.collection.mutable

case class Coordinate(x: Int, y: Int, z: Int)

case class Brick(start: Coordinate, end: Coordinate) {

  lazy val moveDown: Brick =
    this.modifyAll(_.start.z, _.end.z).using(_ - 1)

  def collidesWith(other: Brick): Boolean =
    axisOverlaps(other)(_.x) &&
      axisOverlaps(other)(_.y) &&
      axisOverlaps(other)(_.z)

  private def axisOverlaps(other: Brick)(axisSelector: Coordinate => Int) = {
    val min1 = axisSelector(start).min(axisSelector(end))
    val max1 = axisSelector(start).max(axisSelector(end))
    val min2 = axisSelector(other.start).min(axisSelector(other.end))
    val max2 = axisSelector(other.start).max(axisSelector(other.end))
    max1 >= min2 && max2 >= min1
  }
}

def dropBricks(bricks: Seq[Brick]) = {
  val bricksByZAsc = bricks.sortBy(brick => brick.start.z.min(brick.end.z))
  val remainingBricks = mutable.Stack.from(bricksByZAsc)
  val droppedBricks = mutable.Map[Brick, Set[Brick]]()

  while (remainingBricks.nonEmpty) {
    val brick = remainingBricks.pop()
    val brickMovedDown = brick.moveDown
    lazy val collidingBricks = droppedBricks.collect {
      case (droppedBrick, _) if brickMovedDown.collidesWith(droppedBrick) => droppedBrick
    }.toSet
    if (collidesWithGround(brickMovedDown) || collidingBricks.nonEmpty)
      droppedBricks.put(brick, collidingBricks)
    else
      remainingBricks.push(brickMovedDown)
  }

  droppedBricks.toMap
}

private def collidesWithGround(brick: Brick) =
  brick.start.z == 0 || brick.end.z == 0
