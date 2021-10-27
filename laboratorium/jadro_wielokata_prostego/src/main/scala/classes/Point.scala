package classes
import Math._

trait P

final class Point(a: Double, b: Double, outside: Boolean = false) extends P{
  val x: Double = a
  val y: Double = b
  val out : Boolean = outside

  override def toString: String = s"($x,$y)"
  private def orientation(previous: Point, next: Point): O = {
    signum(
      previous.x * this.y + previous.y * next.x + this.x * next.y
        - this.y * next.x - previous.x * next.y - previous.y * this.x
    ) match {
      case 1 => Left
      case 0 => NoTurn
      case _ => Right
    }
  }
  private def checkLocal(f: (Double, Double) => Boolean)(previous: Point, next: Point): E = {
    this.orientation(previous, next) match {
      case Right =>
        this.y match {
          case previous.y if f(next.y, y) => PotentialEx(y)
          case next.y if f(previous.y, y) => PotentialEx(y)
          case  a if f(previous.y,a) & f(next.y,a) => Extremum(y)
          case _ => NoEx
        }
      case _ => NoEx
    }

  }
  private val checkLocalMax: (Point,Point) => E = checkLocal(_ < _)
  private val checkLocalMin: (Point,Point) => E = checkLocal(_ > _)
  def addToLocalMins(localMins: List[E])(previous: Point, next: Point): List[E] =
    checkLocalMin(previous,next) match {
      case Extremum(y) => Extremum(y)::localMins
      case a: PotentialEx => localMins match {
        case PotentialEx(a.value)::tail =>
          Extremum(a.value)::tail
        case _ => a::localMins
      }
      case _ => localMins
    }
  def addToLocalMaxs(localMaxs: List[E])(previous: Point, next: Point): List[E] = {
    checkLocalMax(previous,next) match {
      case Extremum(y) => Extremum(y)::localMaxs
      case a: PotentialEx => localMaxs match {
        case PotentialEx(a.value)::tail =>
          Extremum(a.value)::tail
        case _ => a::localMaxs
      }
      case _ => localMaxs
    }
  }
  def distance(that: Point): Double = {
    Math.sqrt(Math.pow(that.x - this.x,2) + Math.pow(that.y - this.y,2))
  }
}

object NoPoint extends P

object Infinite extends P


class E(y: Double) {
  val value: Double = y
}

case class Extremum(y: Double) extends E(y)

case class PotentialEx(y: Double) extends E(y)

case object NoEx extends E(1)
object Point {
  def P(x: Double, y: Double): Point = new Point(x,y)
}

