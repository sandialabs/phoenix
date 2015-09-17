package gov.sandia.phoenix.geometry

object Linear {
  /**
   * Note that the shortest line will be mutually perpendicular to both segments.
   * Note that a 0 discriminant mean you are parallel or coincident, so there is no solution.
   * I need to combine this with the LineSegment version.
   */
  def closestPoint(p : Vector3, u : Vector3, q : Vector3, v : Vector3) = {
    val w = p - q
    val a = u * u
    val b = u * v
    val c = v * v
    val det = a * c - b * b

    if(det == 0) None else Some {
      val d = u * w
      val e = v * w
      ((b * e - c * d) / det, (a * e - b * d) / det)
    }
  }
}
