package classes

import classes.Line.YEquals
import classes.Point.P

import java.awt.Polygon

class Polygon(ps: List[Point]){
  val points: List[Point] = ps
  private val (firstPoint,secondPoint,tailPoints) = points match {
    case a::(tail @ b::_) => (a,b,tail)
  }

  private def getPolygon(yMin: Double, yMax: Double, scale: Double = 1): Polygon = {

    require(yMin < yMax)
    val yMinLine = YEquals(yMin)
    val yMaxLine = YEquals(yMax)
    val yMinBoolFirst = firstPoint.y > yMin
    val yMaxBoolFirst = firstPoint.y < yMax

    def Helper(l: List[Point],l2: List[Point], lastMin: Boolean, lastMax: Boolean, previous: Point): List[Point] = {
      l match {
        case recent::tail =>
          val yMinBool = recent.y > yMin
          val yMaxBool = recent.y < yMax
          (yMaxBool,yMinBool,lastMax,lastMin) match {
            case (true,true,true,true) => Helper(tail,recent::l2,yMinBool,yMaxBool,recent)
            case (true,true,false,_) => Helper(tail,recent::new Line(previous,recent).crossPoint(yMaxLine)::l2,yMinBool,yMaxBool,recent)
            case (true,true,_,false) => Helper(tail,recent::new Line(previous,recent).crossPoint(yMinLine)::l2,yMinBool,yMaxBool,recent)
            case (_,false,true,true) => Helper(tail,new Line(previous,recent).crossPoint(yMinLine)::l2,yMinBool,yMaxBool,recent)
            case (false,_,true,true) => Helper(tail,new Line(previous,recent).crossPoint(yMaxLine)::l2,yMinBool,yMaxBool,recent)
            case (false,_,_,false) =>   Helper(tail,new Line(previous,recent).crossPoint(yMaxLine)::new Line(previous,recent).crossPoint(yMinLine)::l2,yMinBool,yMaxBool,recent)
            case (_,false,false,_) => Helper(tail,new Line(previous,recent).crossPoint(yMinLine)::new Line(previous,recent).crossPoint(yMaxLine)::l2,yMinBool,yMaxBool,recent)
            case _ =>  Helper(tail,l2,yMinBool,yMaxBool,recent)
          }
        case Nil =>
          (yMaxBoolFirst,yMinBoolFirst,lastMax,lastMin) match {
            case (true,true,true,true) => firstPoint::l2
            case (true,true,false,_) => firstPoint::new Line(previous,firstPoint).crossPoint(yMaxLine)::l2
            case (true,true,_,false) => firstPoint::new Line(previous,firstPoint).crossPoint(yMinLine)::l2
            case (_,false,true,true) => new Line(previous,firstPoint).crossPoint(yMinLine)::l2
            case (false,_,true,true) => new Line(previous,firstPoint).crossPoint(yMaxLine)::l2
            case (false,_,_,false) => new Line(previous,firstPoint).crossPoint(yMaxLine)::new Line(previous,firstPoint).crossPoint(yMinLine)::l
            case (_,false,false,_) => new Line(previous,firstPoint).crossPoint(yMinLine)::new Line(previous,firstPoint).crossPoint(yMaxLine)::l2
            case _ => l2
          }
      }
    }
    val l = Helper(tailPoints,Nil,yMinBoolFirst,yMaxBoolFirst,firstPoint)
    val firstPointPolygon = l.head
    def reverseAndCalculateCircuit(l: List[Point],l2: List[Point],c: Double): (Polygon,Double) = {
      l match {
        case head::(tail @ next::_) => reverseAndCalculateCircuit(tail,head::l2,c+ head.distance(next))
        case head::Nil => (new Polygon(head::l2),c + head.distance(firstPointPolygon))
      }
    }
    val (poly,c) = reverseAndCalculateCircuit(l,Nil,0)

    println(s"Jądro istnieje a jego obwód to ${c/scale}")
    poly
  }

  def calculateJadro(scale: Double = 1): Option[Polygon] = {
    def checkExt(f: (Double, Double) => Boolean)(a: Double)(b: Double): Double = {
      if (f(a,b)) {
        b
      } else {
        a
      }
    }
    val checkMax: Double => Double => Double = checkExt(_ < _)
    val checkMin: Double => Double => Double = checkExt(_ > _)

    def Helper(l: Seq[Point], yMin: Double, yMax: Double, yLocalMin: List[Double], yLocalMax: List[Double], previous: Point): Option[Polygon] = {
      val thisCheckMax: Double => Double = checkMax(yMax)
      val thisCheckMin: Double => Double = checkMin(yMin)
      val addToLocalMax: (Point, Point) => List[Double] = _.addToLocalMaxs(yLocalMax)(previous,_)
      val addToLocalMin: (Point, Point) => List[Double] = _.addToLocalMins(yLocalMin)(previous,_)
      l match {
        case recent::(rest @ next::_) =>
          val y = recent.y
          Helper(
            l = rest,
            yMin = thisCheckMin(y),
            yMax = thisCheckMax(y),
            yLocalMin = addToLocalMin(recent,next),
            yLocalMax = addToLocalMax(recent,next),
            recent
          )
        case recent::Nil =>
          val y = recent.y
          Helper(
            l = Nil,
            yMin = thisCheckMin(y),
            yMax = thisCheckMax(y),
            yLocalMin = addToLocalMin(recent,firstPoint),
            yLocalMax = addToLocalMax(recent,firstPoint),
            recent
          )
        case Nil =>
          val finalYMax: Double = addToLocalMin(firstPoint,secondPoint).minOption match {
            case Some(y) => y
            case _ => yMax
          }
          val finalYMin: Double = addToLocalMax(firstPoint,secondPoint).maxOption match {
            case Some(y) => y
            case _ => yMin
          }
          if ( finalYMin < finalYMax  ) {
            Some(getPolygon(finalYMin, finalYMax,scale))
          } else {
            println("Jądro nie istnieje")
            None
          }
      }
    }
   Helper(tailPoints,firstPoint.y,firstPoint.y,Nil,Nil,firstPoint)
  }
}

