package io.github.avapl
package day8

sealed trait Direction
case object Right extends Direction
case object Left extends Direction

type Node = String
type Network = Map[Node, (Node, Node)]
