package classes
import classes.Point.P
import classes.Presentation.a
import scalafx.scene.shape.{Circle, Polygon}
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._

import scala.collection.mutable.ListBuffer

//object Circle {
//  def apply(x: Double, y: Double) = new Circle() {
//    centerX = x
//    centerY = y
//    radius = 5
//    fill <== when(hover) choose Green otherwise Red
//  }
//}

object Circle {
  def apply(x: Double, y: Double) = new Circle() {
    centerX = x
    centerY = y
    radius = 5
    fill <== when(hover) choose Green otherwise Red
  }
}

object Presentation extends JFXApp {

  val defaultWidth = 1000
  val defaultHeight = 1000
  val scale = 100
  val l = List(
    P(0,0),
    P(1,0),
    P(1,1),
    P(.5,.5),
    P(0,1)
  )
  val newList: List[Point] = l.map(p => P(p.x*scale+defaultWidth/2,p.y*scale -defaultHeight/2))
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
        if (a.length > 2) {
          val p = new classes.Polygon(a.toList)

          p.calculateJadro()
            .foreach{
              a => content+= a.points.toPolygon(Grey,Transparent)
            }
          content += p.points.toPolygon(Transparent)
        }

      }
    }
  }
  implicit class Poli(punkty: List[Point]) { // konwersja listy Tuple na wielokąt
    def toPolygon(color: Color, color2: Color = Black): Polygon = {
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
