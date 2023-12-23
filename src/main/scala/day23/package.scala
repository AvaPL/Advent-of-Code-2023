package io.github.avapl
package day23

type HikingTrailsMap = Vector[Vector[TrailElement]]

sealed trait TrailElement
case object Path extends TrailElement
case object Forest extends TrailElement

sealed trait Slope extends TrailElement
case object SlopeUp extends Slope
case object SlopeDown extends Slope
case object SlopeLeft extends Slope
case object SlopeRight extends Slope

case class Position(
    row: Int,
    column: Int
)
