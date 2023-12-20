package io.github.avapl
package day20

import scala.collection.mutable

@main def puzzle1(): Unit = {
  val (modules, outputConnections) = PuzzleInputParser.parsedInput
  val (lowPulsesCount, highPulsesCount) = countPulsesAfterNButtonPushes(modules, outputConnections, pushes = 1000)
  val result = lowPulsesCount * highPulsesCount
  println(result)
}

private def countPulsesAfterNButtonPushes(
    modules: Seq[Module],
    outputConnections: OutputConnections,
    pushes: Int
): (LowPulsesCount, HighPulsesCount) = {
  val moduleNameToModule = modules.map(module => module.name -> module).toMap
  val (_, lowPulsesCount, highPulsesCount) = (1 to pushes).foldLeft((moduleNameToModule, 0L, 0L)) {
    case ((state, totalLowPulsesCount, totalHighPulsesCount), _) =>
      val (newState, lowPulsesCount, highPulsesCount) = pushButtonAndCountPulses(state, outputConnections)
      (newState, totalLowPulsesCount + lowPulsesCount, totalHighPulsesCount + highPulsesCount)
  }
  (lowPulsesCount, highPulsesCount)
}

private def pushButtonAndCountPulses(
    modules: Map[ModuleName, Module],
    outputConnections: OutputConnections
): (Map[ModuleName, Module], LowPulsesCount, HighPulsesCount) = {
  val currentModules = mutable.Map.from(modules)
  var lowPulsesCount = 1L // Include the initial Low pulse
  var highPulsesCount = 0L
  val initialPulse = Low(Broadcaster.name)
  val remainingPulses = mutable.Queue[Pulse](initialPulse)

  while (remainingPulses.nonEmpty) {
    val pulse = remainingPulses.dequeue()
    val destinationModuleNames = outputConnections(pulse.source)
    destinationModuleNames.foreach { destinationModuleName =>
      pulse match {
        case _: Low  => lowPulsesCount += 1
        case _: High => highPulsesCount += 1
      }
      currentModules.updateWith(destinationModuleName) {
        case Some(module) =>
          val (newModule, outputPulse) = module.process(pulse)
          outputPulse.foreach(remainingPulses.enqueue)
          Some(newModule)
        case None => None
      }
    }
  }

  (currentModules.toMap, lowPulsesCount, highPulsesCount)
}
