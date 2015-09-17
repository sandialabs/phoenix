package gov.sandia.phoenix.geometry

case class Triangle(p0 : Vector3, p1 : Vector3, p2 : Vector3) {
  val s0 = p2 - p0
  val s1 = p1 - p0
  val normal = (s0 ⨯ s1).normalized
  def plane = new Plane(p0, normal)

  def intersect(ray : Ray) = {
    val p = plane.intersect(ray)
    val u = p * s0
    val v = p * s1
    if(u >= 0 && u <= 1 && v >= 0 &&  u + v <= 1) Some(p) else None
  }
}
