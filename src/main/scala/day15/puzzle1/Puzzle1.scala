package io.github.avapl
package day15.puzzle1

import day15.hash

@main def puzzle1(): Unit = {
  val initializationSequence = PuzzleInputParser.parsedInput
  val hashes = initializationSequence.map(hash)
  val result = hashes.sum
  println(result)
}
