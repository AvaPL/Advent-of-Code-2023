package io.github.avapl
package day19.puzzle1

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[(Seq[Workflow], Seq[Part])](day = 19) {

  override protected def parse(string: String): (Seq[Workflow], Seq[Part]) = {
    val blocks = string.splitBlocks
    val workflows = parseWorkflows(blocks(0))
    val parts = parseParts(blocks(1))
    (workflows, parts)
  }

  private def parseWorkflows(workflows: String) =
    workflows.splitLines.map(parseWorkflow)

  private def parseWorkflow(workflow: String) = {
    val s"$name{$rulesString}" = workflow
    val rules = rulesString.splitBy(",").map(parseRule).toList
    Workflow(name, rules)
  }

  private def parseRule(rule: String) =
    rule match
      case "A" => Accept
      case "R" => Reject
      case s"$variable<$value:$action" =>
        val condition = extractVariable(variable).andThen(_ < value.toInt)
        val ifFulfilled = parseAction(action)
        Comparison(condition, ifFulfilled)
      case s"$variable>$value:$action" =>
        val condition = extractVariable(variable).andThen(_ > value.toInt)
        val ifFulfilled = parseAction(action)
        Comparison(condition, ifFulfilled)
      case workflowName => SendToWorkflow(workflowName)

  private def extractVariable(name: String)(part: Part) =
    name match
      case "x" => part.extremelyCoolLooking
      case "m" => part.musical
      case "a" => part.aerodynamic
      case "s" => part.shiny

  private def parseAction(action: String) =
    action match
      case "A"          => Accept
      case "R"          => Reject
      case workflowName => SendToWorkflow(workflowName)

  private def parseParts(part: String) =
    for {
      s"{x=$x,m=$m,a=$a,s=$s}" <- part.splitLines
    } yield Part(
      extremelyCoolLooking = x.toInt,
      musical = m.toInt,
      aerodynamic = a.toInt,
      shiny = s.toInt
    )
}
