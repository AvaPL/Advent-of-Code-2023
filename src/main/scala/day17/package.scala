package io.github.avapl
package day17

type HeatCost = Int
type CityMap = Vector[Vector[HeatCost]]
type Path = List[Position]

case class Position(
    row: Int,
    column: Int
)

sealed trait Direction {
  def cardinality: Int
  def isOppositeTo(direction: Direction): Boolean
  def move(direction: Direction): Direction
}

case class Up(cardinality: Int) extends Direction {
  
  override def isOppositeTo(direction: Direction): Boolean = 
    direction.isInstanceOf[Down]

  override def move(direction: Direction): Direction =
    direction match
      case other: Up => copy(cardinality = cardinality + other.cardinality)
      case other     => other
}

case class Down(cardinality: Int) extends Direction {

  override def isOppositeTo(direction: Direction): Boolean =
    direction.isInstanceOf[Up]
    
  override def move(direction: Direction): Direction =
    direction match
      case other: Down => copy(cardinality = cardinality + other.cardinality)
      case other       => other
}

case class Left(cardinality: Int) extends Direction {

  override def isOppositeTo(direction: Direction): Boolean =
    direction.isInstanceOf[Right]
    
  override def move(direction: Direction): Direction =
    direction match
      case other: Left => copy(cardinality = cardinality + other.cardinality)
      case other       => other
}

case class Right(cardinality: Int) extends Direction {

  override def isOppositeTo(direction: Direction): Boolean =
    direction.isInstanceOf[Left]
    
  override def move(direction: Direction): Direction =
    direction match
      case other: Right => copy(cardinality = cardinality + other.cardinality)
      case other        => other
}
