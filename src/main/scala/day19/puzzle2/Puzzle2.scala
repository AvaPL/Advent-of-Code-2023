package io.github.avapl
package day19.puzzle2

@main def puzzle2(): Unit = {
  val workflows = PuzzleInputParser.parsedInput
  val workflowNameToWorkflow = workflows.map(w => w.name -> w).toMap
  val result = countAcceptedCombinations(workflows)
  println(result)
}

private def countAcceptedCombinations(workflows: Seq[Workflow]) = {
  val workflowNameToWorkflow = workflows.map(w => w.name -> w).toMap

  def loop(rules: List[Rule], partRanges: PartRanges): Long = {
    rules match
      case Accept :: _               => partRanges.combinations
      case Reject :: _               => 0
      case SendToWorkflow(name) :: _ => loop(workflowNameToWorkflow(name).rules, partRanges)
      case Comparison(partitionPartRanges, ifFulfilled) :: tail =>
        val (conditionFulfilled, conditionNotFulfilled) = partitionPartRanges(partRanges)
        loop(List(ifFulfilled), conditionFulfilled) + loop(tail, conditionNotFulfilled)
  }

  val initialWorkflow = workflowNameToWorkflow("in")
  val maxRange = 1 to 4000
  val initialPartRanges = PartRanges(
    extremelyCoolLooking = maxRange,
    musical = maxRange,
    aerodynamic = maxRange,
    shiny = maxRange
  )
  loop(initialWorkflow.rules, initialPartRanges)
}
