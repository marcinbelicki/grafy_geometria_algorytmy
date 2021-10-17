package classes


trait P

final class Point(a: Double, b: Double, outside: Boolean = false) extends P{
  val x: Double = a
  val y: Double = b
  val out : Boolean = outside

  override def toString: String = s"Point($x,$y)"

}

object NoPoint extends P

object Infinite extends P

