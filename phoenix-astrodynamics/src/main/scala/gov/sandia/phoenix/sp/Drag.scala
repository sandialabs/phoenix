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

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.elements.tle.TLE
import gov.sandia.phoenix.geometry._
import gov.sandia.phoenix.propagators.sgp4.SGP4
import gov.sandia.phoenix.solarsystem.Sol
import gov.sandia.phoenix.time._

import scala.math._

/**
 * STK has defaults of:
 *  Drag coefficient: 2.2
 *  Area/Mass Ratio: 0.02 m * m / kg
 *  F10.7 = 150.0
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object Drag {
  def main(args : Array[String]) = {
    println("Drag")
    val l0 = "ISS (ZARYA)"
    val l1 = "1 25544U 98067A   15061.00329596  .00013725  00000-0  20780-3 0  9995"
    val l2 = "2 25544  51.6457 259.3732 0007864  63.2699  94.7821 15.54945907931352"
    val tle = TLE(Some(l0), l1, l2)
    val t = tle.epoch
    val sat = SGP4(tle)
    val state = sat.unsafe_state(t)
    val rho = HPDModel(t.ECItoGEO(state.position).elevation, SolarFlux.fit(t), Sol.direction(t).toRaDec, state)
    println(drag(2.0, 0.1, rho, sat.unsafe_state(t)))
    println("Done")
  }

  /**
   * See eqn. 3.97 of Montenbruck and Gill.
   * Note: Typical drag coefficients range from 1.5-3.0 (M&G, p. 84).
   */
  def drag(Cd : Double, amr : Double, rho : Double, state : ECIStateVector) = {
    val w = new Vector3(0, 0, 7.29211514670698e-5)
    val v = state.velocity - w % state.position
    v.normalized * (-0.5 * Cd * amr * rho * (v * v))
  }
}

/**
 * Values can be obtained and interpolated here.
 *
 * http://www.swpc.noaa.gov/SolarCycle/
 * http://www.swpc.noaa.gov/ftpdir/weekly/RecentIndices.txt
 * http://www.swpc.noaa.gov/ftpdir/weekly/Predict.txt
 *
 * You can also use the Schatten predicted values (see Fig. 8-10 of Vallado.)
 */
object SolarFlux {
  val F10_7 : Double => Double = t => 150.0 + 80.0 * cos(0.001696 * t) - 0.00001695 * t
  val tSince : (JD, JD) => Double = _.doubleValue - _.doubleValue
  val since198111 = tSince(_ : JD, TimeBuilder(1981, 1, 1))
  def fit = F10_7 compose since198111
}
