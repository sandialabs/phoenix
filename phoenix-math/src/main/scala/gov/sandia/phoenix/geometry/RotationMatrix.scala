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
import gov.sandia.phoenix.geometry.RotationMatrix._

import scala.collection.immutable.{Vector => ImmutableArray}
import scala.math._

/**
 * Rotations represented by a 3x3 array of doubles.
 * <p>
 * @author [[mailto: m b a s t i a @ s a n d i a.g o v M a r k B a s t i a n]]
 */
case class RotationMatrix(m: ImmutableArray[Double]) extends Rotation {
  //Java compatibility later
  def transpose = ~this

  def inverse = !this

  def mul(q: RotationMatrix) = this * q

  def mul(p: Vector3) = this * p

  def getRow(row: Int, vec: Array[Double]): Unit = {
    vec(0) = 3 * row
    vec(1) = 3 * row + 1
    vec(2) = 3 * row + 2
  }

  def *(v: Vector3) = {
    val x = M00 * v.x + M01 * v.y + M02 * v.z
    val y = M10 * v.x + M11 * v.y + M12 * v.z
    val z = M20 * v.x + M21 * v.y + M22 * v.z
    Vector3(x, y, z)
  }

  def *(M: RotationMatrix) = {
    val n = M.m
    new RotationMatrix(ImmutableArray(
      M00 * n(I00) + M01 * n(I10) + M02 * n(I20),
      M00 * n(I01) + M01 * n(I11) + M02 * n(I21),
      M00 * n(I02) + M01 * n(I12) + M02 * n(I22),
      M10 * n(I00) + M11 * n(I10) + M12 * n(I20),
      M10 * n(I01) + M11 * n(I11) + M12 * n(I21),
      M10 * n(I02) + M11 * n(I12) + M12 * n(I22),
      M20 * n(I00) + M21 * n(I10) + M22 * n(I20),
      M20 * n(I01) + M21 * n(I11) + M22 * n(I21),
      M20 * n(I02) + M21 * n(I12) + M22 * n(I22)))
  }

  lazy val det = M00 * (M11 * M22 - M12 * M21) - M01 * (M10 * M22 - M12 * M20) + M02 * (M10 * M21 - M11 * M20)

  //Transpose
  def unary_~ = new RotationMatrix(ImmutableArray
    (M00, M10, M20,
        M01, M11, M21,
        M02, M12, M22))

  //Inverse
  def unary_! = {
    val d = 1.0 / det
    val minv = ImmutableArray(
      (M11 * M22 - M21 * M12) * d,
      (M21 * M02 - M01 * M22) * d,
      (M01 * M12 - M11 * M02) * d,
      (M20 * M12 - M10 * M22) * d,
      (M00 * M22 - M20 * M02) * d,
      (M10 * M02 - M00 * M12) * d,
      (M10 * M21 - M20 * M11) * d,
      (M20 * M01 - M00 * M21) * d,
      (M00 * M11 - M10 * M01) * d)

    new RotationMatrix(minv)
  }

  def M00 = m(I00)

  def M01 = m(I01)

  def M02 = m(I02)

  def M10 = m(I10)

  def M11 = m(I11)

  def M12 = m(I12)

  def M20 = m(I20)

  def M21 = m(I21)

  def M22 = m(I22)

  /**
   * See http://www.j3d.org/matrix_faq/matrfaq_latest.html
   */
  def toQuaternion: Quaternion = {
    val trace = 1 + M00 + M11 + M22
    if (trace > FLT_EPSILON) {
      val s = sqrt(trace) * 2
      val x = (M21 - M12) / s
      val y = (M02 - M20) / s
      val z = (M10 - M01) / s
      val w = 0.25 * s
      new Quaternion(w, new Vector3(x, y, z))
    }
    else if (M00 > M11 && M00 > M22) {
      // Column 0:
      val s = sqrt(1.0 + M00 - M11 - M22) * 2
      val x = 0.25 * s
      val y = (M10 + M01) / s
      val z = (M02 + M20) / s
      val w = (M21 - M12) / s
      new Quaternion(w, new Vector3(x, y, z))
    }
    else if (M11 > M22) {
      // Column 1:
      val s = sqrt(1.0 + M11 - M00 - M22) * 2
      val x = (M10 + M01) / s
      val y = 0.25 * s
      val z = (M21 + M12) / s
      val w = (M02 - M20) / s
      new Quaternion(w, new Vector3(x, y, z))
    }
    else {
      // Column 2:
      val s = sqrt(1.0 + M22 - M00 - M11) * 2
      val x = (M02 + M20) / s
      val y = (M21 + M12) / s
      val z = 0.25 * s
      val w = (M10 - M01) / s
      new Quaternion(w, new Vector3(x, y, z))
    }
  }

  override def toString = {
    M00 + ",\t" + M01 + ", " + M02 + "\n" +
      M10 + ",\t" + M11 + ", " + M12 + "\n" +
      M20 + ",\t" + M21 + ", " + M22 + "\n"
  }

  def ==(p: RotationMatrix): Boolean = (for (i <- m.indices)
  yield (m(i) - p.m(i)) * (m(i) - p.m(i))).sum < FLT_EPSILON
}

object RotationMatrix {
  val I00 = 0
  val I01 = 1
  val I02 = 2
  val I10 = 3
  val I11 = 4
  val I12 = 5
  val I20 = 6
  val I21 = 7
  val I22 = 8

  def rotX(theta: Double) = {
    new RotationMatrix(ImmutableArray(
      1.0, 0.0, 0.0,
      0.0, cos(theta), -sin(theta),
      0.0, sin(theta), cos(theta)))
  }

  def rotY(theta: Double) = {
    new RotationMatrix(ImmutableArray(
      cos(theta), 0, sin(theta),
      0, 1, 0,
      -sin(theta), 0, cos(theta)))
  }

  def rotZ(theta: Double) = {
    new RotationMatrix(ImmutableArray(
      cos(theta), -sin(theta), 0,
      sin(theta), cos(theta), 0,
      0, 0, 1))
  }

  def axes(u: Vector3, v: Vector3, w: Vector3) = {
    new RotationMatrix(ImmutableArray(
      u.x, v.x, w.x,
      u.y, v.y, w.y,
      u.z, v.z, w.z))
  }

  def identity = new RotationMatrix(ImmutableArray(1, 0, 0, 0, 1, 0, 0, 0, 1))

  def rand = {
    val x = new Vector3(random - 0.5, random - 0.5, random - 0.5).normalized
    val v = new Vector3(random - 0.5, random - 0.5, random - 0.5)
    val z = (x % v).normalized
    axes(x, z % x, z)
  }

  def AzEl(azimuth: Double, elevation: Double): RotationMatrix = {
    val az = this.rotZ(toRadians(azimuth))
    val el = this.rotY(toRadians(90 - elevation))
    az * el
  }
}

object IDENTITY_ROTATION extends RotationMatrix(ImmutableArray(1, 0, 0, 0, 1, 0, 0, 0, 1)){
  def getInstance = this
}