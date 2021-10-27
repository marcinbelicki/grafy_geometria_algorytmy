package classes

class Line(p1: Point, p2: Point) extends L{
  val A: Double = p1.y - p2.y
  val B: Double = p2.x - p1.x
  val C: Double = p1.x * p2.y - p2.x * p1.y

  override def toString: String = s"A: $A B: $B C: $C"
  private def isBetween(x01: Option[Double], x02: Option[Double])(x: Double): Boolean ={

    val a: Double => Boolean = x01 match {
      case Some(x1) => _ >= x1
      case None => _ => true
    }
    val b: Double => Boolean = x02 match {
      case Some(x2) => _ <= x2
      case None => _ => true
    }
    a(x) && b(x)
  }


  private def limit(p1: Point,p2: Point, f: Point => Double): List[Option[Double]] ={
    List(p1,p2)
      .sortBy(f)
      .map {
        case p if p.out => None
        case p => Some(f(p))
      }
  }


  val List(x1,x2): List[Option[Double]] = limit(p1,p2,_.x)
  val List(y1,y2): List[Option[Double]] = limit(p1,p2,_.y)



  def crossP(that: Line): P ={
    val mian = that.A * this.B - this.A  * that.B
    val xBefore = this.C  * that.B - that.C * this.B
    if (mian == 0) {
      if (xBefore == 0) {
        Infinite
      } else {
        NoPoint
      }
    } else {
      val x = xBefore/mian
      val y = (that.C * this.A - this.C * that.A)/mian
      if (isBetween(x1,x2)(x) && isBetween(that.x1,that.x2)(x) &&
          isBetween(y1,y2)(y) && isBetween(that.y1,that.y2)(y)) {
        new Point(x,y)
      } else {
        NoPoint
      }
    }
  }
  def crossPoint(that: Line): List[Point] => List[Point] = {
    crossP(that) match {
      case a: Point => a::_
      case _ => l => l
    }
  }

}

object Line {
  def YEquals(y: Double): Line = new Line(new Point(0,y,true),new Point(1,y,true))
}

