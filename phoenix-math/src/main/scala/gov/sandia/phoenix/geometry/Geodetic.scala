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

import gov.sandia.phoenix.constants.WGS84
import gov.sandia.phoenix.math._

import scala.math._

/**
 * Geodetic location. Note that the representation is longitude, latitude, then
 * elevation (in meters). Also, note that care must be taken to convert using geodetic (not geocentric) formulation.
 * Geodetic latitude is the angle between the ellipsoid normal and the equatorial plane, not the angle between the
 * position vector and the equatorial plane.
 *
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class Geodetic(longitude : Double, latitude : Double, elevation : Double) {
  def pretty = "lon = " + longitude + ", lat = " + latitude + ", ele = " + elevation

  def flatEarthDistance(that: Geodetic) = distVincenty(this.latitude, this.longitude, that.latitude, that.longitude)

//  /**
//   * http://www.esrl.noaa.gov/gmd/grad/solcalc/solareqns.PDF
//   */
//  def sunrise_sunset(t : JD) = {
//    val g = t.toGregorianDate
//    val γ = 2.0 * Pi / 365.0 * (t.getDayOfYear - 1.0 + (g.hourOfDay - 12.0) / 24.0)
//    val cγ = cos(γ)
//    val sγ = sin(γ)
//    val c2γ = cos(2.0 * γ)
//    val s2γ = sin(2.0 * γ)
//    val c3γ = cos(3.0 * γ)
//    val s3γ = sin(3.0 * γ)
//    val eqtime = 229.18 * (0.000075 + 0.001868 * cγ - 0.032077 * sγ - 0.014615 * c2γ - 0.040849 * s2γ)
//    val decl = 0.006918 - 0.399912 * cγ + 0.070257 * sγ - 0.006758 * c2γ + 0.000907 * s2γ - 0.002697 * c3γ + 0.00148 * s3γ
//    val theta = Twilight.zeta
//    val clat = cos(latitude.toRadians)
//    val cdec = cos(decl)
//    cos(theta.toRadians) / (clat * cdec) - tan(latitude.toRadians) * tan(decl) match {
//      case bad if bad < -1.0 || bad > 1.0 => None
//      case zenith => Some {
//        val ha = acos(zenith).toDegrees
//        val sunrise = 720.0 + 4.0 * (-longitude - ha) - eqtime
//        val sunset = 720.0 + 4.0 * (-longitude + ha) - eqtime
//        (t.floor plusMinutes sunrise, t.floor plusMinutes sunset)
//      }
//    }
//  }


  def toECEF(azelr: AzElR): Vector3 = toSEZFrame * azelr.toSEZ

  def toSEZFrame = {
    val ecef = toECEF
    val n = WGS84.ellipsoid.normal(ecef)
    Frame(Axes.ZX(n, -Z_AXIS), ecef)
  }

  def toECEF = {
    val sinGD = sin(latitude.toRadians)
    val cosGD = cos(latitude.toRadians)

    val den = 1.0 / sqrt(1.0 - WGS84.e_squared * sinGD * sinGD)

    val C = WGS84.R_EQ_M * den
    val S = WGS84.R_EQ_M * (1.0 - WGS84.e_squared) * den

    val rd = (C + elevation) * cosGD
    val rk = (S + elevation) * sinGD

    Vector3(rd * cos(longitude.toRadians), rd * sin(longitude.toRadians), rk)
  }

  /**
   * Interpolate along the shortest length of the great circle between this
   * point and that point. Elevation is linearly interpolated.
   */
  def interpolate(that: Geodetic, t: Double) = if (t <= 0) this else if (t >= 1) that else if (this == that) this else {
    val res = this.toECEF.slerp(that.toECEF, t) * WGS84.sphere.radius
    val geo = res.toGeodetic
    Geodetic(geo.longitude, geo.latitude, this.elevation + t * (that.elevation - this.elevation))
  }
}