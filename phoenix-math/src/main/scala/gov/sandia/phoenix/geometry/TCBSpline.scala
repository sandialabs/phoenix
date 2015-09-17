package gov.sandia.phoenix.geometry

import scala.math._

object TCBSpline { 
  def startingTangentsFor(p : Seq[Vector3], t : Double, c : Double, b : Double) = {
    Seq.tabulate(p.size - 1) {
      i => 
      if(i == 0)
        p(1) - p.head
      else {
        ((p(i) - p(i-1)) * ((1 - t)*(1 + b)*(1 + c) * 0.5)) +
        (p(i + 1) - p(i)) * ((1 - t)*(1 - b)*(1 - c) * 0.5)
      }
    }
  }
  
  def endingTangentsFor(p : Seq[Vector3], t : Double, c : Double, b : Double) = {
    Seq.tabulate(p.size - 1) {
      i =>
      if(i == p.size - 2)
        p.last - p(p.size - 2)
      else {
        ((p(i + 1) - p(i)) * ((1 - t)*(1 + b)*(1 - c) * 0.5)) +
        (p(i + 2) - p(i + 1)) * ((1 - t)*(1 - b)*(1 + c) * 0.5)
      }
    }
  }
  
  def tangentsFor(p : Seq[Vector3], t : Double = 0, c : Double = 0, b : Double = 0) = startingTangentsFor(p, t, c, b) zip endingTangentsFor(p, t, c, b)
}

/**
 * From http://en.wikipedia.org/wiki/Kochanek%E2%80%93Bartels_spline
 * 
 * Note: I'm clamping the input to apply. Should it just extrapolate off the endpoints instead?
 */
class TCBSpline(val t : Seq[Double], val p : Seq[Vector3], val tangents : Seq[(Vector3, Vector3)]) {
  def this(t : Seq[Double], p : Seq[Vector3], tension : Double = 0, continuity : Double = 0, bias : Double = 0) = this(t, p, TCBSpline.tangentsFor(p, tension, continuity, bias))
  
  def apply(tt : Double) : Option[Vector3] = {
    val n = java.util.Arrays.binarySearch(t.toArray, max(min(tt, t.last), t.head))
    
    if(n >= 0)
      Some(p(n))
    else {
      -n - 1 match {
        case pos if pos >= t.length => None
        case pos =>
          val i = pos - 1

          val delta = t(i + 1) - t(i)

          val x = (tt - t(i)) / delta

          val t3 = x*x*x
          val t2 = x*x
          val t1 = x

          val m0 = tangents(i)._1
          val m1 = tangents(i)._2

          val p0 = p(i)
          val p1 = p(i + 1)

          val hermite =  p0 * (2*t3 - 3*t2 + 1) + m0 * (t3 -2*t2 + t1) + p1 * (-2*t3 + 3*t2) + m1 * (t3 - t2)
          Some(hermite)
      }
    }
  }
}