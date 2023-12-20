package io.github.avapl
package day20

import scala.annotation.tailrec
import scala.collection.mutable

// The solution bases on the assumption that rx's input is a conjunction
@main def puzzle2(): Unit = {
  val (modules, outputConnections) = PuzzleInputParser.parsedInput
  val rxModuleName = "rx"
  val rxInput = findInputsFor(rxModuleName, outputConnections).head
  val rxInputInputs = findInputsFor(rxInput, outputConnections)
  val result = countButtonPressesToRxLowPulse(modules, outputConnections, rxInputInputs)
  println(result)
}

private def findInputsFor(moduleName: ModuleName, outputConnections: OutputConnections) = 
  outputConnections
    .filter((_, outputs) => outputs.contains(moduleName))
    .map((moduleName, _) => moduleName)
    .toSet

private def countButtonPressesToRxLowPulse(
    modules: Seq[Module],
    outputConnections: OutputConnections,
    rxInputInputs: Set[ModuleName]
) = {
  var currentModules = modules.map(module => module.name -> module).toMap
  var pushes = 0
  val cycles = mutable.ListBuffer[Long]()
  val remainingPulsesToFind = mutable.Set.from[Pulse](rxInputInputs.map(High.apply))
  
  while (remainingPulsesToFind.nonEmpty) {
    pushes += 1
    val (newModules, sentPulses) = pushButtonAndGetPulses(currentModules, outputConnections)
    remainingPulsesToFind.foreach { moduleName =>
      val pulsesIntersection = sentPulses.intersect(remainingPulsesToFind.toSet)
      if (pulsesIntersection.nonEmpty) {
        pulsesIntersection.foreach(_ => cycles.addOne(pushes))
        pulsesIntersection.foreach(remainingPulsesToFind.remove)
      }
    }
    currentModules = newModules
  }
  
  leastCommonMultiple(cycles.toList)
}

private def pushButtonAndGetPulses(
    modules: Map[ModuleName, Module],
    outputConnections: OutputConnections
): (Map[ModuleName, Module], Set[Pulse]) = {
  val currentModules = mutable.Map.from(modules)
  val initialPulse = Low(Broadcaster.name)
  val remainingPulses = mutable.Queue[Pulse](initialPulse)
  val sentPulses = mutable.Set[Pulse]()

  while (remainingPulses.nonEmpty) {
    val pulse = remainingPulses.dequeue()
    sentPulses.add(pulse)
    val destinationModuleNames = outputConnections(pulse.source)
    destinationModuleNames.foreach { destinationModuleName =>
      currentModules.updateWith(destinationModuleName) {
        case Some(module) =>
          val (newModule, outputPulse) = module.process(pulse)
          outputPulse.foreach(remainingPulses.enqueue)
          Some(newModule)
        case None => None
      }
    }
  }
  
  (currentModules.toMap, sentPulses.toSet)
}

@tailrec
private def leastCommonMultiple(longs: List[Long]): Long =
  longs match
    case first :: second :: tail => leastCommonMultiple(leastCommonMultiple(first, second) :: tail)
    case first :: Nil            => first

private def leastCommonMultiple(first: Long, second: Long) =
  first / greatestCommonDivisor(first, second) * second

@tailrec
private def greatestCommonDivisor(first: Long, second: Long): Long =
  if (second == 0) first
  else greatestCommonDivisor(second, first % second)
