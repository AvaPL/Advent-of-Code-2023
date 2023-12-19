package io.github.avapl
package day19.puzzle1

case class Part(
    extremelyCoolLooking: Int,
    musical: Int,
    aerodynamic: Int,
    shiny: Int
) {
  val rating: Int = extremelyCoolLooking + musical + aerodynamic + shiny
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

case class Comparison(condition: Part => Boolean, ifFulfilled: Action) extends Rule
