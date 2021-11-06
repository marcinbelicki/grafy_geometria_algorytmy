package classes
import classes.Point.P
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Circle, Polygon}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


object Circle {
  def apply(x: Double, y: Double) = new Circle() {
    centerX = x
    centerY = y
    radius = 5
    fill <== when(hover) choose Green otherwise Red
  }
}


object Zad2 extends JFXApp {
  val defaultWidth = 1000
  val defaultHeight = 1000
  val scale = 100

  val l: List[(Double,Double)] = List(
    (1,1),(3,1),(3,2),(4,2),(4,4),(3,4),(3,3),(2,3),(2,2),(1,2)
  )

  val newList: List[Point] = l.map {
    case (x,y) => P(x*scale+defaultWidth/2,y*scale -defaultHeight/2)
  }
  println(newList)

  val a: ListBuffer[Point] = ListBuffer.empty
  stage = new JFXApp.PrimaryStage {
    title.value = "Jądro wielokąta"
    width = defaultWidth
    height = defaultHeight

    scene = new Scene {
      fill = LightGreen
      val square = SetOfPoints(newList)
      square.getClosestPair  match {
        case a @ ((p1: Point, _), (p2: Point, _), _) =>
          println(a)
          content += List(p1, p2).toPolygon(Transparent)
        case _ => ()
      }
     newList.foreach{
        p=>
          content+= Circle(p.x,-p.y)
      }
      onMouseClicked = (me: MouseEvent) => {
        content = Nil
        a += P(me.x, -me.y)
        if (a.length > 1) {
          a.foreach {
            p => content+= Circle(p.x,-p.y)
          }
           SetOfPoints(a.toList).getClosestPair
          match {
             case a @ ((p1: Point, _), (p2: Point, _), _) =>
               println(a)
              content += List(p1, p2).toPolygon(Transparent)
            case _ => ()

          }

        }
      }
    }


  }
  implicit class Poli(punkty: List[Point]) {
    def toPolygon(color: Color, color2: Color = Black): Polygon = {
      @tailrec
      def Helper(punkty: List[Point], poli: Polygon): Polygon = {
        punkty match {
          case point::tail =>
            poli.getPoints.addAll(point.x,-point.y)
            Helper(tail,poli)
          case List() =>
            poli
        }
      }
      val res = Helper(punkty, Polygon())

      res.stroke = color2
      res.fill = color
      res
    }
  }
}
//}
