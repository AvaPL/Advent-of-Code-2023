package io.github.avapl
package day15

private def hash(string: String) =
  string.foldLeft(0) { (currentValue, char) =>
    val asciiValue = char.toInt
    (currentValue + asciiValue) * 17 % 256
  }
