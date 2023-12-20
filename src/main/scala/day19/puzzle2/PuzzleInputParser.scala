package io.github.avapl
package day19.puzzle2

import util.InputParser
import util.StringOps.*
import scala.util.chaining.*

object PuzzleInputParser extends InputParser[Seq[Workflow]](day = 19) {

  override protected def parse(string: String): Seq[Workflow] = {
    val blocks = string.splitBlocks
    parseWorkflows(blocks(0))
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
        val partitionRange = (range: Range) => {
          val pivot = value.toInt
          (range.start until pivot, pivot to range.last)
        }
        Comparison(
          partitionPartRanges = partitionPartRanges(rangeSelector(variable), partitionRange, rangeUpdater(variable)),
          ifFulfilled = parseAction(action)
        )
      case s"$variable>$value:$action" =>
        val partitionRange = (range: Range) => {
          val pivot = value.toInt
          (pivot + 1 to range.last, range.start to pivot)
        }
        Comparison(
          partitionPartRanges = partitionPartRanges(rangeSelector(variable), partitionRange, rangeUpdater(variable)),
          ifFulfilled = parseAction(action)
        )
      case workflowName => SendToWorkflow(workflowName)

  private def partitionPartRanges(
      selectRange: PartRanges => Range,
      partitionRange: Range => (Range, Range),
      updatePartRanges: (PartRanges, Range) => PartRanges
  )(partRanges: PartRanges) =
    selectRange(partRanges)
      .pipe(partitionRange)
      .pipe { (conditionFulfilled, conditionNotFulfilled) =>
        (updatePartRanges(partRanges, conditionFulfilled), updatePartRanges(partRanges, conditionNotFulfilled))
      }

  private def rangeSelector(name: String)(partRanges: PartRanges) =
    name match
      case "x" => partRanges.extremelyCoolLooking
      case "m" => partRanges.musical
      case "a" => partRanges.aerodynamic
      case "s" => partRanges.shiny

  private def rangeUpdater(name: String)(partRanges: PartRanges, range: Range) =
    name match
      case "x" => partRanges.copy(extremelyCoolLooking = range)
      case "m" => partRanges.copy(musical = range)
      case "a" => partRanges.copy(aerodynamic = range)
      case "s" => partRanges.copy(shiny = range)

  private def parseAction(action: String) =
    action match
      case "A"          => Accept
      case "R"          => Reject
      case workflowName => SendToWorkflow(workflowName)
}
