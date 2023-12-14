package io.github.avapl
package day12.puzzle1

case class ConditionRecord(
    value: String,
    damagedGroupSizes: List[Int]
)

val operationalState = '.'
val damagedState = '#'
val unknownState = '?'
