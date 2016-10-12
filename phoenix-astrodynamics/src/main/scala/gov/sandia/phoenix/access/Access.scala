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

package gov.sandia.phoenix.access

import gov.sandia.phoenix.constants.WGS84
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry._

/**
 * Used to determine if two physical objects have direct line of sight.
 */
object Access {
  def apply(obs : Any, tgt : Any) = (obs, tgt) match {
    case (a : Geodetic, b : Geodetic) => geo_geo(a, b, Angle.ZERO)
    case (a : Geodetic, b : Vector3) =>
      ecef_ecef(a.toECEF, b, Angle.ZERO)
    case (a : Vector3, b : Geodetic) =>
      ecef_ecef(a, b.toECEF, Angle.ZERO)
    case (a : Vector3, b : Vector3) => ecef_ecef(a, b)
    case (a : ECIStateVector, b : ECIStateVector) => eci_eci(a.position, b.position)
    case (a : ECIStateVector, b : Vector3) => eci_eci(a.position, b)
    case (a : Vector3, b : ECIStateVector) => eci_eci(a, b.position)
    case _ =>
      val args = obs :: tgt :: Nil
      val txt = args zip (args map { _.getClass.getName }) map { case (arg, className) => arg.toString + " (" + className + ")" }
      logger.info("Unable to determine appropriate access function for " + txt.mkString(", ") + ".")
      false
  }

  def apply(obs : Any, tgt : Any, mask : Angle) = (obs, tgt, mask) match {
    case (a : Geodetic, b : Geodetic, maskAngle : Angle) => geo_geo(a, b, maskAngle)
    case (a : Vector3, b : Vector3, maskAngle : Angle) => ecef_ecef(a, b, maskAngle)
    case _ =>
      val args = obs :: tgt :: mask :: Nil
      val txt = args zip (args map { _.getClass.getName }) map { case (arg, className) => arg.toString + " (" + className + ")" }
      logger.info("Unable to determine appropriate access function for " + txt.mkString(", ") + ".")
      false
  }

  def eci_eci(src : Vector3, target : Vector3) : Boolean = !WGS84.ellipsoid.intersects(new LineSegment(src, target))

  def ecef_ecef(src : Vector3, target : Vector3) : Boolean = !WGS84.ellipsoid.intersects(new LineSegment(src, target))

  def geo_geo(src : Geodetic, target : Geodetic, maskAngle : Angle) : Boolean =
    ecef_ecef(src.toECEF, target.toECEF, maskAngle)

  def ecef_ecef(src : Vector3, target : Vector3, maskAngle : Angle) : Boolean = if(src == target) false else {
    val elevation = WGS84.ellipsoid.normal(src).angle(target - src).complement
    elevation.degrees > maskAngle.degrees
  }
}