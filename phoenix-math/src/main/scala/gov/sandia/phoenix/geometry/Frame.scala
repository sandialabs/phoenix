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

/**
 * Representation of a full rotation + translation frame.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class Frame(rotation : Quaternion, translation : Vector3) {
  //Java compatibility layer
  lazy val inverse = !this
  def mul(p : Vector3) = this * p
  def mul(p : Frame) = this * p

  def * (v : Vector3) = rotation * v + translation
  def * (B : Frame) : Frame = new Frame(B.rotation * rotation, B.rotation * translation + B.translation)
  def unary_! = {
    val invRotation = !rotation
    new Frame(invRotation, invRotation * -translation)
  }

  def pretty = {
    val m = rotation.toMatrix
    "|" +  m.m(0) + ",\t" + m.m(1) + ", " + m.m(2) + "|   " + translation.x + "\n" +
      "|" +  m.m(3) + ",\t" + m.m(4) + ", " + m.m(5) + "| + " + translation.y + "\n" +
      "|" +  m.m(6) + ",\t" + m.m(7) + ", " + m.m(8) + "|   " + translation.z + "\n"
  }

  def toSEZ(a : AzElR) = Degrees(-a.azimuth).rz * Degrees(a.elevation).ry * Vector3(-a.range, 0, 0)

  def toSEZ(v : Vector3) : Vector3 = !this * v

  def toAzElR(v : Vector3) = toSEZ(v).toAzElR
}

object ORIGIN_FRAME extends Frame(IDENTITY_QUATERNION, ORIGIN)