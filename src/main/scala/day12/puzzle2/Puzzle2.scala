package io.github.avapl
package day12.puzzle2

import util.StringOps.*

import scala.collection.mutable

@main def puzzle2(): Unit = {
  val conditionRecords = PuzzleInputParser.parsedInput
  val springArrangements = conditionRecords.map(countSpringArrangements)
  val result = springArrangements.sum
  println(result)
}

private def countSpringArrangements(conditionRecord: ConditionRecord): Long = {
  val cache = mutable.Map[ConditionRecord, Long]()

  def loop(conditionRecord: ConditionRecord): Long = {
    lazy val result = {
      val shortenedRecordValue = conditionRecord.value.dropWhile(_ == operationalState)
      conditionRecord.damagedGroupSizes match
        case damagedGroupSize :: tail =>
          val withoutDamagedGroup = removePotentialDamagedGroupLeft(shortenedRecordValue, damagedGroupSize)
          val tailArrangements = calculateTailArrangements(shortenedRecordValue, conditionRecord.damagedGroupSizes)
          if (withoutDamagedGroup == shortenedRecordValue) tailArrangements
          else tailArrangements + loop(ConditionRecord(withoutDamagedGroup, tail))
        case Nil if shortenedRecordValue.matches(s"[$operationalState$unknownState]*") => 1
        case Nil                                                                        => 0
    }
    cache.getOrElseUpdate(conditionRecord, result)
  }

  def removePotentialDamagedGroupLeft(recordValue: String, damagedGroupSize: Int) = {
    val damagedGroupRegex = s"^[$unknownState$damagedState]{$damagedGroupSize}([$operationalState$unknownState]|$$)"
    recordValue.replaceFirst(damagedGroupRegex, "")
  }

  def calculateTailArrangements(recordValue: String, damagedGroupSizes: List[Int]) =
    if (recordValue.isEmpty || recordValue.startsWith(damagedState.toString)) 0
    else loop(ConditionRecord(recordValue.tail, damagedGroupSizes))

  loop(conditionRecord)
}

