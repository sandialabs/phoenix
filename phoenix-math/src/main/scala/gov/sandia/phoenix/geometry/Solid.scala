package gov.sandia.phoenix.geometry

import scala.math._

trait Solid extends LineSegmentIntersectable {
  def volume : Double
  def area : Double
  
  /**
   * A ball is solid.
   */
  abstract override def intersect(ls : LineSegment) = super.intersect(ls) match {
    case Array(i, f) => {
      if(f < 0.0 || i > 1.0) Array() else {
        val a = max(i, 0.0)
        val b = min(f, 1.0)
        if(a == b) Array(a) else Array(a, b)
      }
    }
    case x => x
  }
}