package gov.sandia.phoenix.geometry
import scala.language.postfixOps

trait Shell extends LineSegmentIntersectable {
  def volume : Double
  def area : Double
  
  /**
   * A shell is hollow, so only penetrations (line segment parameters [0, 1]) count.
   */
  abstract override def intersect(ls : LineSegment) = super.intersect(ls) match {
    case Array(i, f) => Array(if (i >= 0.0 && i <= 1.0) Some(i) else None,
      if (f >= 0.0 && f <= 1.0) Some(f) else None).flatten
    case x => x 
  }  
  
}