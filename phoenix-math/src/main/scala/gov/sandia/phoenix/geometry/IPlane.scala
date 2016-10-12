/*
 * Copyright (c) 2016 Sandia Corporation. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by the
 * terms of this license.
 * You must not remove this notice, or any other, from this software.
 *
 * Contributors:
 * - Mark Bastian: Original author.
 * - See Git logs.
 */

package gov.sandia.phoenix.geometry

import gov.sandia.phoenix.math._

import scala.math._

trait IPlane {
  def n : Vector3
  def d : Double

  def o = n * d

  def dist(p : Vector3) = p * n - d

  def > (p : Vector3) = dist(p) < 0.0
  def < (p : Vector3) = dist(p) > 0.0
  def >= (p : Vector3) = dist(p) <= 0.0
  def <= (p : Vector3) = dist(p) >= 0.0
  def == (p : Vector3) = abs(dist(p)) < 1E-10

  def intersect(ray : Ray) = {
    val den = ray.direction * n
    val num = d - ray.origin * n
    val t = num / den
    ray.parametricPoint(t)
  }

  def intersect(ls : LineSegment) = {
    val ds = dist(ls.start)
    val de = dist(ls.end)
    if(ds * de > 0) None else {
      val ads = abs(ds)
      val ade = abs(de)
      Some(ls.interpolate(ads / (ads + ade)))
    }
  }

  def cut(ls : LineSegment) = {
    val ds = dist(ls.start)
    val de = dist(ls.end)
    if(ds * de >= 0) None else Some {
      val ads = abs(ds)
      val ade = abs(de)
      val pt = ls.interpolate(ads / (ads + ade))
      if(ds < 0) (new LineSegment(ls.start, pt), new LineSegment(pt, ls.end))
      else (new LineSegment(ls.end, pt), new LineSegment(pt, ls.start))
    }
  }

  def dihedral(that : IPlane) = Radians(cacos(this.n * that.n))

  def intersect(that : IPlane) : Option[Ray] = if(dihedral(that).abs < 1.0E-10) None else Some {
    val dir = (this.n ⨯ that.n).normalized
    if(this.d == 0.0 && that.d == 0.0) Ray(this.o, dir) else {
      val zdet = this.n.x * that.n.y - this.n.y * that.n.x
      if(zdet != 0){
        val x = (that.n.y * this.d - this.n.y * that.d) / zdet
        val y = (this.n.x * that.d - that.n.x * this.d) / zdet
        Ray(Vector3(x, y, 0), dir)
      } else {
        val ydet = this.n.x * that.n.z - this.n.z * that.n.x
        if(ydet != 0) {
          val x = (that.n.z * this.d - this.n.z * that.d) / ydet
          val z = (this.n.x * that.d - that.n.x * this.d) / ydet
          Ray(Vector3(x, 0, z), dir)
        } else {
          val xdet = this.n.y * that.n.z - this.n.z * that.n.y
          val y = (that.n.z * this.d - this.n.z * that.d) / xdet
          val z = (this.n.y * that.d - that.n.y * this.d) / xdet
          Ray(Vector3(0, y, z), dir)
        }
      }
    }
  }
}