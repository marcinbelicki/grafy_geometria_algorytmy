package classes

import classes.Point.P



object Tree {
  private var tree = null.asInstanceOf[TreeX]

  def makeTree(list: List[Point]): Unit = {
    val sXwithIndex = list.sortBy(_.x).zipWithIndex
    val sY = sXwithIndex
      .sortBy(_._1.y)
      .zipWithIndex
      .map{
        case ((p,x),y) => (p,(y,x))
      }
    val sX = sY.map{
      case (p,(y,x)) => (p,(x,y))
    }.sortBy(_._2._1)
   tree =new TreeX(sX,sY)
  }

  var a = Nil.asInstanceOf[List[(Point,(Int, Int))]]

  def findSpace(minX: Double,minY: Double, maxX: Double, maxY: Double) = {
    a = Nil.asInstanceOf[List[(Point,(Int, Int))]]
    tree.findSpace(minX,minY, maxX, maxY)
    a
  }


  abstract class TreeKD(sX: List[(Point,(Int, Int))], sY: List[(Point,(Int, Int))]) {
    val (setX, setY) = (sX, sY)

    override def toString: String = setX.toString()
    protected def split(firstSet: List[(Point,(Int, Int))], secondSet: List[(Point,(Int, Int))], f: Point => Double): Option[((List[(Point,(Int, Int))],List[(Point,(Int, Int))]),(List[(Point,(Int, Int))],List[(Point,(Int, Int))]),Double)] = {
      val length = firstSet.length
      val (half, even): (Int, Boolean) = {
        ((length *.5).toInt,length % 2 == 0)
      }
      def helperFirst(setRight: List[(Point,(Int, Int))],setLeft: List[(Point,(Int, Int))], index: Int):  (List[(Point,(Int, Int))],List[(Point,(Int, Int))], Double, Int) = {
        setRight match {
          case head::tail if index <  half =>
            helperFirst(tail,head::setLeft,index + 1)
          case (firstEl @ (p1,(max,_)))::(tail @ ((p,_)::_)) if even =>
            (tail,(firstEl::setLeft).reverse,(f(p1)+f(p))/2,max)
          case (firstEl @ (p1,(max,_)))::tail =>
            (tail,(firstEl::setLeft).reverse,f(p1),max)
        }
      }

      if (length <= 1) {
        None
      } else {
        val (sFirst1,sFirst2,l,max) = helperFirst(firstSet,Nil,1)
        def helperSecond(set:List[(Point,(Int, Int))], setRight: List[(Point,(Int, Int))],setLeft: List[(Point,(Int, Int))]):  (List[(Point,(Int, Int))],List[(Point,(Int, Int))]) = {
          set match {
            case (head @ (_,(_,index)))::tail if index <= max =>
              helperSecond(tail,setRight,head::setLeft)
            case head::tail =>
              helperSecond(tail,head::setRight,setLeft)
            case Nil =>
              (setRight.reverse,setLeft.reverse)
          }
        }
        val (sSecond1,sSecond2) = helperSecond(secondSet,Nil,Nil)
        Some((sFirst1,sSecond1),(sFirst2,sSecond2),l)
      }
    }


  }

  class TreeX(sX: List[(Point,(Int, Int))], sY: List[(Point,(Int, Int))]) extends TreeKD(sX: List[(Point,(Int, Int))], sY: List[(Point,(Int, Int))]) {
    val (left,right,l) = split(setX,setY,_.x) match {
      case None => (None, None, None)
      case Some(((sFirst1, sSecond1), (sFirst2, sSecond2), l)) => (Some(new TreeY(sFirst1, sSecond1)), Some(new TreeY(sFirst2, sSecond2)), Some(l))
    }

    def findSpace(minX: Double,minY: Double, maxX: Double, maxY: Double): Unit= {
      l match {
        case Some(value) =>
          (value >= minX,value <= maxX) match {
            case (true,true) =>
              List(left,right)
                .foreach(_.get.findSpace(minX,minY,maxX,maxY))
            case (true,false) =>
              List(right)
                .foreach(_.get.findSpace(minX,minY,maxX,maxY))
            case (false,true) =>
              List(left)
                .foreach(_.get.findSpace(minX,minY,maxX,maxY))
          }
        case None =>
          setX.filter {
            case (p,_) =>
              val y = p.y
              val x = p.x
              y >= minY && y <= maxY &&  x >= minX && x <= maxX
          }.foreach(b => a = b::a)
      }
    }
  }
  class TreeY(sX: List[(Point,(Int, Int))], sY: List[(Point,(Int, Int))]) extends TreeKD(sX: List[(Point,(Int, Int))], sY: List[(Point,(Int, Int))]) {
    val (left,right,l) = split(setY,setX,_.y) match {
      case None => (None, None, None)
      case Some(((sFirst1, sSecond1), (sFirst2, sSecond2), l)) => (Some(new TreeX(sSecond1,sFirst1)), Some(new TreeX( sSecond2,sFirst2)), Some(l))
    }
    def findSpace(minX: Double,minY: Double, maxX: Double, maxY: Double): Unit = {
      l match {
        case Some(value) =>
          (value >= minY,value <= maxY) match {
            case (true,true) =>
              List(left,right)
                .foreach(_.get.findSpace(minX,minY,maxX,maxY))
            case (true,false) =>
              List(right)
                .foreach(_.get.findSpace(minX,minY,maxX,maxY))
            case (false,true) =>
              List(left)
                .foreach(_.get.findSpace(minX,minY,maxX,maxY))
          }
        case None =>
          setX.filter {
            case (p,_) =>
              val y = p.y
              val x = p.x
              y >= minY && y <= maxY &&  x >= minX && x <= maxX
          }.foreach(b => a = b::a)
      }
    }
  }

}

object Test extends App {




  val b = Tree

  b.makeTree((
    List(
      P(1,4),
      P(2,4),
      P(3,4),
      P(4,4),
      P(1,3),
      P(2,3),
      P(3,3),
      P(4,3),
      P(1,2),
      P(2,2),
      P(3,2),
      P(4,2),
      P(1,1),
      P(2,1),
      P(3,1),
      P(4,1)
    )
    ))



  println(b.findSpace(2,2,3,3).map(_._1))




}
