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
        - this.y * next.x - previous.x * next.y - previous.y *this.x) match {
      case 1 => Left
      case 0 => NoTurn
      case _ => Right
    }
  }
  private def checkLocal(f: (Double, Double) => Boolean )(previous: Point, next: Point): Option[Double] = {

    val y = this.y

    if (f(previous.y,y) && f(next.y,y)) {
      this.orientation(previous, next) match {
        case Right => Some(y)
        case _ =>
          None
      }
    } else {
      None
    }
  }
  private val checkLocalMax: (Point,Point) => Option[Double] = checkLocal(_ < _)
  private val checkLocalMin: (Point,Point) => Option[Double] = checkLocal(_ > _)
  def addToLocalMins(localMins: List[Double])(previous: Point, next: Point): List[Double] =
    checkLocalMin(previous,next) match {
      case Some(y) => y::localMins
      case _ => localMins
    }
  def addToLocalMaxs(localMaxs: List[Double])(previous: Point, next: Point): List[Double] = {
    checkLocalMax(previous,next) match {

      case Some(y) =>   y::localMaxs
      case _ => localMaxs
    }
  }
  def distance(that: Point): Double = {
    Math.sqrt(Math.pow(that.x - this.x,2) + Math.pow(that.y - this.y,2))
  }
}

object NoPoint extends P

object Infinite extends P

object Point {
  def P(x: Double, y: Double): Point = new Point(x,y)
}

