package io.github.avapl
package day12.puzzle1

import util.StringOps.*

@main def puzzle1(): Unit = {
  val conditionRecords = PuzzleInputParser.parsedInput
  val springArrangements = conditionRecords.map(countSpringArrangements)
  val result = springArrangements.sum
  println(result)
}

private def countSpringArrangements(conditionRecord: ConditionRecord) = {
  val unknownStateIndices = conditionRecord.value.zipWithIndex.collect {
    case (state, i) if state == unknownState => i
  }
  val knownDamagedCount = conditionRecord.value.count(_ == damagedState)
  val totalDamagedCount = conditionRecord.damagedGroupSizes.sum
  val potentialArrangementCombinations =
    unknownStateIndices.combinations(totalDamagedCount - knownDamagedCount).map(_.toSet)
  val validArrangements = potentialArrangementCombinations
    .map(toArrangement(conditionRecord.value))
    .filter(isArrangementValid(conditionRecord.damagedGroupSizes))
  validArrangements.size
}

private def toArrangement(conditionRecordValue: String)(damagedStateIndices: Set[Int]): String =
  conditionRecordValue.zipWithIndex.map {
    case (state, i) if state == unknownState && damagedStateIndices.contains(i) => damagedState
    case (state, i) if state == unknownState                                    => operationalState
    case (state, _)                                                             => state
  }.mkString

private def isArrangementValid(targetDamagedGroupSizes: List[Int])(arrangement: String) = {
  val arrangementDamagedGroupSizes = arrangement
    .splitByRegex(s"\\$operationalState+")
    .collect {
      case group if group.nonEmpty => group.size
    }
    .toList
  arrangementDamagedGroupSizes == targetDamagedGroupSizes
}
