package io.github.avapl
package day9

type History = List[Int]

def calculateDifferences(history: History) =
  List.unfold(history.toVector) { differences =>
    Option.when(!differences.forall(_ == 0)) {
      val newDifferences = calculateNewDifferences(differences)
      (newDifferences, newDifferences)
    }
  }

private def calculateNewDifferences(differences: Vector[Int]) =
  differences
    .sliding(2)
    .map { case Vector(first, second) =>
      second - first
    }
    .toVector
