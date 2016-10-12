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

package gov.sandia.phoenix.solarsystem

import gov.sandia.phoenix.constants._
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry._
import gov.sandia.phoenix.sp.{SPForceProvider, TwoBody}
import gov.sandia.phoenix.time._

import scala.math._

object Luna extends SPForceProvider {
  val mu = 4902.8000
  val GM = mu * 1E9
  //http://nssdc.gsfc.nasa.gov/planetary/factsheet/moonfact.html
  val R_EQ_KM = 1736.0
  val R_EQ_M = R_EQ_KM * 1000.0
  val ellipticity = 0.0012
  val R_POLAR_KM = R_EQ_KM * sqrt(1 - ellipticity * ellipticity)
  val R_POLAR_M = R_POLAR_KM * 1000.0
  def sphere(t : JD) = new Sphere(position(t), R_EQ_M)
  def ball(t : JD) = Ball(position(t), R_EQ_M)

  /**
   * Compute the geocentric J2K position of the Moon in Earth Radii. See Vallado, 3rd Ed., p. 290, Algorithm 31.
   * This is a pretty good link for visual checking for reality http://www.timeanddate.com/worldclock/sunearth.html.
   */
  def positionER(time : JD) = {
    val t = time.toJulianCenturyJ2000
    val lambdaEcliptic = toRadians((218.32 + 481267.883 * t + 
                  6.29 * sin(toRadians(134.9 + 477198.85 * t)) - 
                  1.27 * sin(toRadians(259.2 - 413335.38 * t)) + 
                  0.66 * sin(toRadians(235.7 + 890534.23 * t)) + 
                  0.21 * sin(toRadians(269.9 + 954397.70 * t)) - 
                  0.19 * sin(toRadians(357.5 + 35999.05 * t)) - 
                  0.11 * sin(toRadians(186.6 + 966404.05 * t))) % 360.0)
    
    val phiEcliptic = toRadians((5.13 * sin(toRadians(93.3 + 483202.03 * t)) + 
                       0.28 * sin(toRadians(228.2 + 960400.87 * t)) - 
                       0.28 * sin(toRadians(318.3 + 6003.18 * t)) - 
                       0.17 * sin(toRadians(217.6 - 407332.20 * t))) % 360.0)
    
   val parallax = toRadians((0.9508 + 
                   0.0518 * cos(toRadians(134.9 + 477198.85 * t)) + 
                   0.0095 * cos(toRadians(259.2 - 413335.38 * t)) + 
                   0.0078 * cos(toRadians(235.7 + 890534.23 * t)) + 
                   0.0028 * cos(toRadians(269.9 + 954397.70 * t))) % 360.0)
   
   val obliquity = toRadians((23.439291 + t * (-0.0130042 + t * (-1.64E-7 + 5.04E-7 * t))) % 360)

    val r = 1.0 / sin(parallax)

    val cosPhi = cos(phiEcliptic)
    val sinPhi = sin(phiEcliptic)
    val cosLam = cos(lambdaEcliptic)
    val sinLam = sin(lambdaEcliptic)
    val cosE = cos(obliquity)
    val sinE = sin(obliquity)
        
    Vector3(r * cosPhi * cosLam,
      r * (cosE * cosPhi * sinLam - sinE * sinPhi),
      r * (sinE * cosPhi * sinLam + cosE * sinPhi))
  }

  /**
   * Compute the geocentric J2K position of the Moon in meters. See Vallado, 3rd Ed., p. 290, Algorithm 31
   */
  def position(time : JD) = positionER(time) * WGS84.R_EQ_M

  def acceleration(t : JD, state : ECIStateVector) = TwoBody.acceleration(state.position, position(t), GM)

  def getInstance = this
}
