package io.github.avapl
package day11

type GalaxyImage = Vector[Vector[GalaxyImageElement]]

sealed trait GalaxyImageElement
case object Galaxy extends GalaxyImageElement
case object EmptySpace extends GalaxyImageElement

case class Position(
    row: Int,
    column: Int
)

def calculateGalaxyDistancesSum(galaxyImage: GalaxyImage, extensionMultiplier: Long) = {
  val expandedRowIndices = getExpandedRowsIndices(galaxyImage)
  val expandedColumnIndices = getExpandedColumnsIndices(galaxyImage)
  val galaxyPositions = getGalaxyPositions(galaxyImage)
  val galaxyPairs = getGalaxyPairs(galaxyPositions)
  val galaxyPairDistances =
    galaxyPairs.map(calculateExpandedDistance(expandedRowIndices, expandedColumnIndices, extensionMultiplier))
  galaxyPairDistances.sum
}

private def getExpandedRowsIndices(galaxyImage: GalaxyImage) =
  galaxyImage.zipWithIndex.collect {
    case (row, i) if row.forall(_ == EmptySpace) => i
  }.toSet

private def getExpandedColumnsIndices(galaxyImage: GalaxyImage) =
  galaxyImage.transpose.zipWithIndex.collect {
    case (column, i) if column.forall(_ == EmptySpace) => i
  }.toSet

private def getGalaxyPositions(galaxyImage: GalaxyImage) = {
  for {
    row <- galaxyImage.indices
    column <- galaxyImage.head.indices
    if galaxyImage(row)(column) == Galaxy
  } yield Position(row, column)
}.toList

private def getGalaxyPairs(galaxyPositions: List[Position]) =
  galaxyPositions
    .combinations(2)
    .map { case List(first, second) =>
      (first, second)
    }
    .toList

private def calculateExpandedDistance(
    expandedRowIndices: Set[Int],
    expandedColumnIndices: Set[Int],
    expansionMultiplier: Long
)(galaxyPosition1: Position, galaxyPosition2: Position) = {
  val minRow = galaxyPosition1.row.min(galaxyPosition2.row)
  val maxRow = galaxyPosition1.row.max(galaxyPosition2.row)
  val minColumn = galaxyPosition1.column.min(galaxyPosition2.column)
  val maxColumn = galaxyPosition1.column.max(galaxyPosition2.column)
  val rowRange = (minRow to maxRow).toSet
  val columnRange = (minColumn to maxColumn).toSet
  val rowCost = rowRange.size + expandedRowIndices.intersect(rowRange).size * (expansionMultiplier - 1) - 1
  val columnCost = columnRange.size + expandedColumnIndices.intersect(columnRange).size * (expansionMultiplier - 1) - 1
  rowCost + columnCost
}
