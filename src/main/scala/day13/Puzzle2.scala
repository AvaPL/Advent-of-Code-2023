package io.github.avapl
package day13

@main def puzzle2(): Unit = {
  val patterns = PuzzleInputParser.parsedInput
  val fixedMirrors = patterns.map(findFixedMirror)
  val scores = fixedMirrors.map {
    case VerticalMirror(index)   => index
    case HorizontalMirror(index) => 100 * index
  }
  val result = scores.sum
  println(result)
}

private def findFixedMirror(pattern: Pattern) =
  findFixedVerticalMirror(pattern)
    .orElse(findFixedHorizontalMirror(pattern))
    .get

private def findFixedVerticalMirror(pattern: Pattern) = {
  val width = pattern.head.length
  val startShift = width % 2
  (startShift until width - 1 by 2).collectFirst {
    case shift if hasRightFixedMirror(pattern, shift) => VerticalMirror(shift + (width - shift) / 2)
    case shift if hasLeftFixedMirror(pattern, shift)  => VerticalMirror((width - shift) / 2)
  }
}

private def hasRightFixedMirror(pattern: Pattern, shift: Int) = {
  var totalMismatches = 0
  pattern.map(_.drop(shift)).foreach { row =>
    totalMismatches += row.zip(row.reverse).count(_ != _)
  }
  totalMismatches == 2 // a smudge causes exactly 2 mismatches
}

private def hasLeftFixedMirror(pattern: Pattern, shift: Int) = {
  var totalMismatches = 0
  pattern.map(_.dropRight(shift)).foreach { row =>
    totalMismatches += row.zip(row.reverse).count(_ != _)
  }
  totalMismatches == 2 // a smudge causes exactly 2 mismatches
}

private def findFixedHorizontalMirror(pattern: Pattern) = {
  val transposedPattern = pattern.transpose.map(_.mkString)
  findFixedVerticalMirror(transposedPattern)
    .map { case VerticalMirror(index) =>
      HorizontalMirror(index)
    }
}
