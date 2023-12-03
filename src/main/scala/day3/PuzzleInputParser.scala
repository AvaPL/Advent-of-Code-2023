package io.github.avapl
package day3

import util.InputParser
import util.StringOps.*

import scala.annotation.tailrec
import scala.collection.mutable

case object PuzzleInputParser extends InputParser[Schematic](day = 3) {

  override protected def parse(string: String): Schematic = {
    val lines = string.splitLines
    val numbers = extractNumbers(lines)
    val symbols = extractSymbols(lines)
    Schematic(numbers, symbols)
  }

  private def extractNumbers(schematic: Seq[String]): List[Number] = {
    for {
      (row, rowIndex) <- schematic.zipWithIndex
    } yield extractNumbers(row, rowIndex)
  }.toList.flatten

  private def extractNumbers(row: String, rowIndex: Int): List[Number] = {
    @tailrec
    def loop(row: List[(Char, Int)], acc: List[Number]): List[Number] =
      row match
        case Nil                                => acc
        case (head, _) :: tail if !head.isDigit => loop(tail, acc)
        case list =>
          val (chars, indices) = list.unzip
          val valueString = chars.takeWhile(_.isDigit).mkString
          val number = Number(value = valueString.toInt, Location(rowIndex, column = indices.head))
          loop(list.drop(valueString.size), number :: acc)

    loop(row.zipWithIndex.toList, acc = Nil)
  }

  private def extractSymbols(schematic: Seq[String]) = {
    val symbols = mutable.ListBuffer[Symbol]()
    for {
      (row, rowIndex) <- schematic.zipWithIndex
      (value, columnIndex) <- row.zipWithIndex
      if value != '.' && !value.isDigit
    } symbols.addOne(Symbol(value, Location(rowIndex, columnIndex)))
    symbols.toList
  }
}
