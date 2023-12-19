package io.github.avapl
package day19.puzzle1

import scala.annotation.tailrec

@main def puzzle1(): Unit = {
  val (workflows, parts) = PuzzleInputParser.parsedInput
  val workflowNameToWorkflow = workflows.map(w => w.name -> w).toMap
  val partsWithState = getPartStates(workflowNameToWorkflow, parts)
  val acceptedParts = partsWithState.collect { case (part, Accept) => part }
  val acceptedPartsRatings = acceptedParts.map(_.rating)
  val result = acceptedPartsRatings.sum
  println(result)
}

private def getPartStates(workflowNameToWorkflow: Map[String, Workflow], parts: Seq[Part]) = {

  @tailrec
  def loop(workflow: Workflow, part: Part): PartState = {
    passThroughWorkflow(workflow, part) match
      case state: PartState     => state
      case SendToWorkflow(name) => loop(workflowNameToWorkflow(name), part)
  }

  val initialWorkflow = workflowNameToWorkflow("in")
  parts.map(part => part -> loop(initialWorkflow, part))
}

private def passThroughWorkflow(workflow: Workflow, part: Part): Action = {

  @tailrec
  def loop(remainingRules: List[Rule]): Action =
    remainingRules match
      case (action: Action) :: _ =>
        action
      case Comparison(condition, ifFulfilled) :: tail =>
        if (condition(part)) ifFulfilled else loop(tail)

  loop(workflow.rules)
}
