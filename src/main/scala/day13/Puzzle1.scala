package io.github.avapl
package day13

@main def puzzle1(): Unit = {
  val pattern = PuzzleInputParser.parsedInput
  val mirrors = pattern.map(findMirror)
  val scores = mirrors.map {
    case VerticalMirror(index)   => index
    case HorizontalMirror(index) => 100 * index
  }
  val result = scores.sum
  println(result)
}

private def findMirror(pattern: Pattern) =
  findVerticalMirror(pattern)
    .orElse(findHorizontalMirror(pattern))
    .get

private def findVerticalMirror(pattern: Pattern) = {
  val width = pattern.head.length
  val startShift = width % 2
  (startShift until width - 1 by 2).collectFirst {
    case shift if hasRightMirror(pattern, shift) => VerticalMirror(shift + (width - shift) / 2)
    case shift if hasLeftMirror(pattern, shift)  => VerticalMirror((width - shift) / 2)
  }
}

private def hasRightMirror(pattern: Pattern, shift: Int) =
  pattern.map(_.drop(shift)).forall(row => row == row.reverse)

private def hasLeftMirror(pattern: Pattern, shift: Int) =
  pattern.map(_.dropRight(shift)).forall(row => row == row.reverse)

private def findHorizontalMirror(pattern: Pattern) = {
  val transposedPattern = pattern.transpose.map(_.mkString)
  findVerticalMirror(transposedPattern)
    .map { case VerticalMirror(index) =>
      HorizontalMirror(index)
    }
}

sealed trait Mirror
case class VerticalMirror(index: Int) extends Mirror
case class HorizontalMirror(index: Int) extends Mirror
