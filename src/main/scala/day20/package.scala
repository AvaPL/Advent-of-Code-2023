package io.github.avapl
package day20

import day20.Conjunction.InputState

type ModuleName = String
type OutputConnections = Map[ModuleName, List[ModuleName]]
type LowPulsesCount = Long
type HighPulsesCount = Long

sealed trait Pulse {
  def source: ModuleName
}
case class Low(source: ModuleName) extends Pulse
case class High(source: ModuleName) extends Pulse

sealed trait Module {
  def name: ModuleName
  def process(pulse: Pulse): (Module, Option[Pulse])
}

case object Broadcaster extends Module {
  val name = "broadcaster"

  override def process(pulse: Pulse): (Module, Option[Pulse]) = {
    val newPulse = pulse match
      case Low(source)  => Low(source = name)
      case High(source) => High(source = name)
    (Broadcaster, Some(newPulse))
  }
}

case class FlipFlop(
    name: ModuleName,
    state: FlipFlop.State = FlipFlop.State.Off
) extends Module {

  override def process(pulse: Pulse): (Module, Option[Pulse]) =
    (pulse, state) match
      case (_: High, _)                      => (this, None)
      case (Low(source), FlipFlop.State.Off) => (copy(state = FlipFlop.State.On), Some(High(source = name)))
      case (Low(source), FlipFlop.State.On)  => (copy(state = FlipFlop.State.Off), Some(Low(source = name)))
}

object FlipFlop {

  sealed trait State

  object State {
    case object On extends State
    case object Off extends State
  }
}

case class Conjunction(
    name: ModuleName,
    inputStates: Map[ModuleName, Conjunction.InputState] = Map.empty
) extends Module {

  override def process(pulse: Pulse): (Module, Option[Pulse]) = {
    val newInputState = pulse match
      case _: Low  => Conjunction.InputState.Low
      case _: High => Conjunction.InputState.High
    val newInputStates = inputStates + (pulse.source -> newInputState)
    val newPulse =
      if (newInputStates.forall((_, inputState) => inputState == Conjunction.InputState.High)) Low(source = name)
      else High(source = name)
    (copy(inputStates = newInputStates), Some(newPulse))
  }
}

object Conjunction {

  sealed trait InputState

  object InputState {
    case object Low extends InputState
    case object High extends InputState
  }
}
