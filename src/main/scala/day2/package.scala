package io.github.avapl
package day2

case class Game(
    id: Int,
    cubeSubsets: List[Cubes]
)

case class Cubes(
    red: Int,
    green: Int,
    blue: Int
)
