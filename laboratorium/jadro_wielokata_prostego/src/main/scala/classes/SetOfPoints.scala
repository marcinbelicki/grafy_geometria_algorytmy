package classes

import classes.Point.P

import scala.annotation.tailrec

class SetOfPoints(x: List[(Point,Int)] , y: List[(Point,Int)] ) {
  val sX: List[(Point,Int)] = x
  val sY: List[(Point,Int)] = y

  private def halve[A](list: List[(A,Int)],list2: List[(A,Int)]): ((List[(A,Int)], List[(A,Int)]),(List[(A,Int)], List[(A,Int)])) = {
    val firstIndex: Int =  list.head._2
    val half: Int = firstIndex + (list.length * .5).toInt
    @tailrec
    def Helper(list: List[(A,Int)], first: List[(A,Int)]): (List[(A,Int)], List[(A,Int)]) = {
      list match {
        case (head @ (_,index)) ::tail if index <= half =>  Helper(tail,head::first)
        case _ =>
          (first,list)
      }
    }
    @tailrec
    def Helper2(list: List[(A,Int)], first: List[(A,Int)], second: List[(A,Int)]): (List[(A,Int)], List[(A,Int)]) = {
      list match {
        case (head @ (_,index)) ::tail if index <= half & index >= firstIndex=>  Helper2(tail,head::first, second)
        case head::tail => Helper2(tail,first, head::second)
        case Nil => (first,second)
      }
    }
    (Helper(list,Nil),Helper2(list2,Nil,Nil))
  }
  def getClosestPair: ((P,Int), (P,Int),Double) = {
    sX match {
      case a::Nil => (a,(NoPoint,-1),a._1.distance(NoPoint))
      case a::b::Nil => (a,b,a._1.distance(b._1))
      case _ =>
        val ((rx,s2x),(ry,s2y)): ((List[(Point,Int)],List[(Point,Int)]),(List[(Point,Int)],List[(Point,Int)]))= halve(sX,sY)
        val s1x = rx.reverse
        val s1y = ry.reverse
        val l = rx.head._1.x
        val s1 = new SetOfPoints(s1x,s1y)
        val s2 = new SetOfPoints(s2x,s2y)
        val delta = List(s1.getClosestPair,s2.getClosestPair).minBy(_._3)
        val deltaLength = delta._3
        val s1DeltaL = s1.getDeltaL(deltaLength,l)
        val s2DeltaL = s2.getDeltaL(deltaLength,l)
        val a = List(List(delta),s1DeltaL.getClosestInDeltaL(s2DeltaL,deltaLength)).flatten.minBy(_._3)
        a
    }
  }
  private def getDeltaL(delta: Double, l: Double): SetOfPoints = {
    val f: List[(Point,Int)] => List[(Point,Int)] = _.filter(p => Math.abs( p._1.x - l) <= delta)
    new SetOfPoints(f(sX),f(sY))
  }
  private def getClosestInDeltaL(that: SetOfPoints,delta: Double): List[((Point,Int), (Point,Int),Double)] = {
    def joinTwo(s1: SetOfPoints)(s2: SetOfPoints): List[((Point,Int), (Point,Int),Double)] = {
      s1
        .sY
        .foldLeft(s2.sY,Nil.asInstanceOf[List[((Point,Int), (Point,Int),Double)]]) {
          case ((l,result),p) =>
            val r = l
              .dropWhile(_._1.y < p._1.y)
            (
              r,
              r
                .take(4)
                .map(p2 => (p,p2,p._1.distance(p2._1))) ::: result
            )
        }
        ._2
    }
    List(joinTwo(this)(that),joinTwo(that)(this))
      .flatten
      .minByOption(_._3)
      .toList
  }
}

object SetOfPoints {
  def apply(l: List[Point]): SetOfPoints ={
    val sX = l.sortBy(p => (p.x,p.y)).zipWithIndex
    val sY = sX.sortBy(_._1.y)
    new SetOfPoints(sX, sY)
  }
}
object ABC extends App {
 println(List(1,2,3,4,2).dropWhile(_ < 3))
}