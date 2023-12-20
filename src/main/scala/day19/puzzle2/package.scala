package io.github.avapl
package day19.puzzle2

case class PartRanges(
    extremelyCoolLooking: Range,
    musical: Range,
    aerodynamic: Range,
    shiny: Range
) {
  val combinations: Long =
    extremelyCoolLooking.size.toLong * musical.size * aerodynamic.size * shiny.size
}

case class Workflow(
    name: String,
    rules: List[Rule]
)

sealed trait Rule

sealed trait Action extends Rule

sealed trait PartState
case object Accept extends Action with PartState
case object Reject extends Action with PartState

case class SendToWorkflow(name: String) extends Action

case class Comparison(partitionPartRanges: PartRanges => (PartRanges, PartRanges), ifFulfilled: Action) extends Rule
