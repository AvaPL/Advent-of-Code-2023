package io.github.avapl
package day24

import scala.util.Try

case class Position(
    x: Double,
    y: Double
)

case class Velocity(
    x: Double,
    y: Double
)

case class Hailstone(
    initialPosition: Position,
    velocity: Velocity
) {

  def positionAfterNanos(nanos: Double): Position = {
    val x = velocity.x * nanos + initialPosition.x
    val y = velocity.y * nanos + initialPosition.y
    Position(x, y)
  }

  def trajectoryIntersection(other: Hailstone): Option[Position] =
    Try {
      // x1 = x2 -> v_x1 * t_1 + x_01 = v_x2 * t_2 + x_02
      // y1 = y2 -> v_y1 * t_1 + y_01 = v_y2 * t_2 + y_02
      val t1 =
        (other.velocity.x * (other.initialPosition.y - initialPosition.y) + other.velocity.y * (initialPosition.x - other.initialPosition.x)) / (other.velocity.x * velocity.y - velocity.x * other.velocity.y)
      val t2 =
        (velocity.x * (other.initialPosition.y - initialPosition.y) + velocity.y * (initialPosition.x - other.initialPosition.x)).toDouble / (other.velocity.x * velocity.y - velocity.x * other.velocity.y)
      Option.when(t1 > 0 && t2 > 0)(positionAfterNanos(t1))
    }.toOption.flatten
}
