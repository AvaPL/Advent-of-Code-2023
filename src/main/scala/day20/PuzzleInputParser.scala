package io.github.avapl
package day20

import util.InputParser
import util.StringOps.*

import scala.collection.mutable

object PuzzleInputParser extends InputParser[(Seq[Module], OutputConnections)](day = 20) {

  override protected def parse(string: String): (Seq[Module], OutputConnections) = {
    val outputConnections = mutable.Map[ModuleName, List[ModuleName]]()
    val modules = for {
      s"$identifier -> $outputs" <- string.splitLines
    } yield {
      val module = parseModule(identifier)
      outputConnections.put(module.name, outputs.splitBy(", ").toList)
      module
    }
    val initializedModules = initializeModules(modules, outputConnections.toMap)
    (initializedModules, outputConnections.toMap)
  }

  private def parseModule(identifier: String) =
    identifier match
      case "broadcaster" => Broadcaster
      case s"%$name"     => FlipFlop(name)
      case s"&$name"     => Conjunction(name)

  private def initializeModules(modules: Seq[Module], outputConnections: OutputConnections) = {
    val inputConnections = outputConnections.toList.flatMap {
      (moduleName, outputModules) => outputModules.map(_ -> moduleName)
    }.groupMap((moduleName, _) => moduleName)((_, input) => input)
    modules.map {
      case conjunction @ Conjunction(name, _) =>
        val inputNames = inputConnections.getOrElse(name, Nil)
        val inputStates = inputNames.map(_ -> Conjunction.InputState.Low).toMap
        conjunction.copy(inputStates = inputStates)
      case other => other
    }
  }
}
