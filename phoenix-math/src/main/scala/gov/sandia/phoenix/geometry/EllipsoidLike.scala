package gov.sandia.phoenix.geometry

import scala.math._

trait EllipsoidLike extends RayIntersection with LineSegmentIntersectable {
  def center : Vector3
  def axes : Vector3
  
  val volume = 4.0 * Pi * axes.x * axes.y * axes.z / 3.0
  
  def normal(p : Vector3) = {
    val n = p - center
    Vector3(n.x / (axes.x * axes.x), n.y / (axes.y * axes.y), n.z / (axes.z * axes.z)).normalized
  }

  def intersect(ray : Ray) = {
    val d = ray.origin - center
    val m = new Vector3(d.x / axes.x, d.y / axes.y, d.z / axes.z)
    val n = new Vector3(ray.direction.x / axes.x, ray.direction.y / axes.y, ray.direction.z / axes.z)
    val a = n * n
    val b = m * n
    val c = m * m - 1.0
    val des = b * b - a * c

    if(des > 0)
      Array((-b - math.sqrt(des)) / a, ( -b + math.sqrt(des) ) / a)
    else if(des == 0.0)
      Array(-b / a)
    else
      new Array[Double](0)
  }
  
  /**
   * How to derive this:
   * Eqn. for parametric point: p(t) = p0 + t * (p1 - p0)           eq(1)
   * Eqn. for parametric ellipse: [(p[i] - o[i]) * s[i]]^2 = 1      eq(2)
   * note: s = <1/r.x, 1/r.y, 1/r.z)
   * substitute 1 -> 2
   * [(p0[i] + t * (p1[i] - p0[i]) - o[i]) * s[i]]^2 = 1
   * let m = (p0[i] - o[i]) * s[i]
   * let n = (p1[i] - p0[i]) * s[i]
   * (m[i] + t * n[i])^2 = 1
   * expressed as a vector operation...
   * (m + t * n)*(m + t * n) = 1
   * n * n * t * t + 2 * m * n + t + (m * m - 1) = 0
   * Matching terms from the quadratic:
   * a = n * n
   * b = 2 * m * n
   * c = m * m - 1
   * Plugging in to the standard form:
   * num = - 2 * mn +/- sqrt(4 * mn * mn - 4 * nn * (mm - 1))
   * den = 2 * nn
   * by simplification all of the integers drop out....
   * num = - mn +/- sqrt(mn * mn - nn * (mm - 1))
   * den = nn
   * 
   * Note that these are vector dot products, so you cannot cancel the n's. Also,
   * be careful to not expand (mn * mn) in the descriminant to (mm * nn) and then
   * cancel the nn in the second term.
   */
  def intersect(ls : LineSegment) : Array[Double] = {
    val d = ls.start - center
    val v = ls.end - ls.start
    val m = new Vector3(d.x / axes.x, d.y / axes.y, d.z / axes.z)
    val n = new Vector3(v.x / axes.x, v.y / axes.y, v.z / axes.z)
    val a = n * n
    val b = m * n
    val c = m * m - 1.0
    val des = b * b - a * c
      
    if(des > 0) {
      Array((-b - math.sqrt(des)) / a, (-b + math.sqrt(des)) / a)
    } else if(des == 0.0) Array(-b) else Array()
  }
}
