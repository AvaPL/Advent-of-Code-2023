package io.github.avapl
package day12.puzzle2

case class ConditionRecord(
    value: String,
    damagedGroupSizes: List[Int]
)

val operationalState = '.'
val damagedState = '#'
val unknownState = '?'
