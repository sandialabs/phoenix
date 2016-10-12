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

package gov.sandia.phoenix.sp

import gov.sandia.phoenix.constants._
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry._
import gov.sandia.phoenix.solarsystem.Sol
import gov.sandia.phoenix.time._

import scala.math._

/**
 * Based off of section 3.4 of Montenbruck & Gill. The two key parameters here
 * are A/m (area to mass ratio) and ε, the reflectance of the object). From the
 * book, ε is generally in the range of 0.2 - 0.9.
 * 
 * STK has defaults of:
 *  Area/Mass Ratio = 0.1 m * m / kg
 *  Cr = 1.21 (epsilon = 0.21)
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object SolarRadiationPressure {
  def main(args : Array[String]) = {
    println("SRP")
    val t = TimeBuilder(2010, 7, 8, 16, 22)
    val r = Sol.state(t).get.position
    val v = -r.normalized * WGS84.sphere.radius * 3.0
    val axis = (~v).normalized
    val steps = 10000
    for(i <- 0 until steps; theta = Degrees((i * 360.0) / steps)) {
      apply(r, AxisAngle(axis, theta).toQuaternion * v)
    }
    println("Done")
  }

  def apply(pSun : Vector3, pSat : Vector3, amr : Double = 0.1, ε : Double = 0.21) = {
    val P = 4.56E-6
    //val m = 1000 //1000 kg
//    val ε = 0.21
    val CR = 1.0 + ε
    val r = pSun
    //val A = 100.0 //m*m
    val R = pSun.mag
    val AU = Celestial.AU
//    val amr = 0.1 //m * m / kg
    val nu = shadow(pSun, pSat)
    pSun * (-P * nu * amr * CR * AU * AU / (R * R * R))
  }

  def shadow(pSun : Vector3, pSat : Vector3) = {
    //r - spacecraft coords in arbitrary frame
    //s - spacecraft coords wrt B (no change for earth)
    val R = 6.96E8
    val RB = 6378137.0 //Radius of the Earth
    val d = pSun - pSat
    val den = d.mag
    if(R > den) 0.0 else {
      val a = asin(R / den)
      val sm = pSat.mag
      if(RB > sm) 0.0 else {
        val b = asin(RB / sm)
        val c = acos(-pSat * d / (sm * den))
        if((a + b) <= c) 1.0 else if (c < abs(b - a)) 0.0 else {
          val a2 = a * a
          val b2 = b * b
          val c2 = c * c
          val x = 0.5 * (c2 + a2 - b2) / c
          val y = sqrt(a2 - x * x)
          val A = a2 * acos(x / a) + b2 * acos((c - x) / b) - c * y
          1.0 - A / (Pi * a2)
        }}}
  }
}

case class SRP(amr : Double = 0.1, ε : Double = 0.21) extends SPForceProvider {
  def acceleration(t : JD, state : ECIStateVector) =
    SolarRadiationPressure(Sol.eciPosition(t), state.position, amr, ε) 
}
