package gov.sandia.phoenix.geometry

trait LineSegmentIntersectable {
  def intersect(ls : LineSegment) : Array[Double]
  def intersects(ls : LineSegment) = intersect(ls).length != 0
}
