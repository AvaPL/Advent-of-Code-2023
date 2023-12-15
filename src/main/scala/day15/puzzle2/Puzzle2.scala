package io.github.avapl
package day15.puzzle2

import day15.hash

import scala.collection.mutable

@main def puzzle2(): Unit = {
  val steps = PuzzleInputParser.parsedInput
  val boxes = determineBoxesState(steps)
  val result = calculateFocusingPower(boxes)
  println(result)
}

private def determineBoxesState(steps: Seq[Step]) = {
  val state = Vector.fill(256)(mutable.LinkedHashMap[Label, FocalLength]())
  steps.foreach {
    case RemoveLens(label) =>
      val boxIndex = hash(label)
      state(boxIndex).remove(label)
    case InsertLens(label, focalLength) =>
      val boxIndex = hash(label)
      state(boxIndex).put(label, focalLength)
  }
  state.map(_.toVector)
}

private def calculateFocusingPower(boxes: Vector[Vector[(Label, FocalLength)]]) = {
  for {
    (lenses, boxIndex) <- boxes.zipWithIndex
    ((_, focalLength), lensIndex) <- lenses.zipWithIndex
  } yield (boxIndex + 1) * (lensIndex + 1) * focalLength
}.sum
