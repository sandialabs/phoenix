package gov.sandia.phoenix.geometry

object SphericalPolygonGenerator {
  def apply(poly : Array[AzEl]) =
    SphericalPolygon(poly.map(_.toSEZ), AzEls.guessExteriorPoint(poly))
}

case class SphericalPolygon(points : Array[Vector3], outer : Vector3) {
  val arcs = points.indices flatMap { i =>
    val pi = points(i)
    val pf = points((i + 1) % points.length)
    if(pi != pf) Some(Arc(pi, pf)) else None
  }
  def contains(p : Vector3) = if(p close -outer){
    //If p and outer are antipodal, create two arbitrary colinear arcs and sum the results.
    val perp = ~p
    val a = Arc(p, perp)
    val b = Arc(perp, outer)
    val ac = crossings(a)
    val bc = crossings(b)
    (ac + bc) % 2 == 1
  } else crossings(Arc(p, outer)) % 2 == 1

  def crossings(arc : Arc) = arcs count { _.intersects(arc) }
  def intersections(plane : IPlane) = arcs flatMap { _.intersect(plane) }
  def sample(pointsPerSegment : Int) = arcs flatMap { _.sample(pointsPerSegment) }
  def transform(q : Quaternion) = SphericalPolygon(points.map(q * _), q * outer)

  def intersects(a : Vector3, theta : Angle) = {
    val plane = Plane(theta.cos, a)
    contains(a) || points.exists(_ > plane) || arcs.exists(_.generalIntersection(plane).size > 0)
  }
}
