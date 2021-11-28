package classes
import classes.Point.P
import scalafx.scene.shape.Polygon
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer




object JadroWielokataProstego extends JFXApp {

  val defaultWidth = 1000
  val defaultHeight = 1000
  val scale = 100

  val l: List[(Double,Double)] = List(
//    (1,1),(2,2),(3,1),(3,4),(2,3),(1,4)
//    (1,1),(2,2),(3,2),(4,1),(4,4),(3,4),(2,3),(1,4)
    (1,1),(3,1),(3,2),(4,2),(4,4),(3,4),(3,3),(2,3),(2,2),(1,2)
//    (1,1),(2,2),(3,2),(2,2)
//    (1,1),(2,2),(3,1),(2,2),(3,3),(2,2),(1,3),(2,2)
//    (1,1),(2,1),(3,2),(4,1),(4,3),(3,3),(2,2),(1,3)
//    (1,1),(2,1),(2,4),(3,4),(3,1),(6,1),(6,5),(5,5),(5,2),(4,2),(4,5),(1,5)
//    (1,1),(2,1),(3,2),(4,1),(5,1),(3,3)
  )

  val newList: List[Point] = l.map {
    case (x,y) => P(x*scale+defaultWidth/2,y*scale -defaultHeight/2)
  }
  val a: ListBuffer[Point] = ListBuffer.empty
  stage = new JFXApp.PrimaryStage {
    title.value = "Jądro wielokąta"
    width = defaultWidth
    height = defaultHeight
    scene = new Scene {
      fill = LightGreen
      val square = new classes.Polygon(newList)
      square.calculateJadro(scale)
        .foreach{
          a => content+= a.points.toPolygon(Grey,Transparent)
        }
      content += square.points.toPolygon(Transparent)
      onMouseClicked = (me: MouseEvent) => {
        content = Nil
        a += P(me.x,-me.y)
        if (a.length > 1) {
          val p = new classes.Polygon(a.toList)
          p
            .calculateJadro()
            .foreach(content+= _.points.toPolygon(Grey,Transparent))
          content += p.points.toPolygon(Transparent)
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
