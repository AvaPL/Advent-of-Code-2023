package io.github.avapl
package day6

case class Race(
    duration: Long,
    recordDistance: Long
)

def determineButtonHoldTimesToWin(race: Race) = {
  val holdTimesToCheck = 1L until race.duration
  holdTimesToCheck.filter { holdTime =>
    val time = race.duration - holdTime
    val distance = calculateDistance(velocity = holdTime, time)
    distance > race.recordDistance
  }
}

private def calculateDistance(velocity: Long, time: Long) =
  velocity * time
