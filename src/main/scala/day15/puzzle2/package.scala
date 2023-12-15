package io.github.avapl
package day15.puzzle2

type Label = String
type FocalLength = Int

sealed trait Step
case class RemoveLens(label: Label) extends Step
case class InsertLens(label: Label, focalLength: FocalLength) extends Step