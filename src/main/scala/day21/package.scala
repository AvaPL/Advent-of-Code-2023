package io.github.avapl
package day21

type GardenMap = Vector[Vector[GardenElement]]

sealed trait GardenElement
case object GardenPlot extends GardenElement
case object Rock extends GardenElement

case class Position(
    row: Int,
    column: Int
)
