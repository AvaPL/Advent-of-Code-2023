package io.github.avapl
package day13

type Pattern = Vector[String]

sealed trait Mirror
case class VerticalMirror(index: Int) extends Mirror
case class HorizontalMirror(index: Int) extends Mirror
