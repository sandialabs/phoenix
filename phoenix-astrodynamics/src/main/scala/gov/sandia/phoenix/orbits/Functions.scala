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

package gov.sandia.phoenix.orbits

import gov.sandia.phoenix.constants.WGS84
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.Vector3
import gov.sandia.phoenix.math._
import gov.sandia.phoenix.propagators.UniversalVariablePropagator

import scala.math._

/**
 * Functions related to orbital maneuvers.
 */
object Functions {
  /**
   * r0 : Initial position.
   * r : Final position.
   * dt : Time to get from r0 to r.
   * mu : Desired constant.
   * tol : tolerance for convergence
   *
   * out:
   * (v,,initial,,, v,,final,,)
   */
  def Lambert(r0 : Vector3, r : Vector3, dt : Double, shortPathTrajectory : Boolean = true,
              mu : Double = WGS84.GM, tol : Double = 1.0E-12) = {
    val sign = if(shortPathTrajectory) 1 else -1
    val cosDNu = r * r0 / (r.mag * r0.mag)
    val sinDNu = sign * sqrt(1.0 - cosDNu * cosDNu)
    sign * sqrt(r.mag * r0.mag * (1.0 + cosDNu)) match {
      case 0.0 => None
      case area => Some {
        val rtmu = sqrt(mu)
        def Y(psi : Double = 0.0, psiBounds : (Double, Double) = (- 4.0 * Pi, 4.0 * Pi * Pi),
              dtn : Double = Double.NaN, y : Double = Double.NaN) : Double =
          if(abs(dtn - dt) < tol || abs(psiBounds._2 - psiBounds._1) < tol) y else {
            require(psiBounds._1 <= psiBounds._2)
            val (c2, c3) = UniversalVariablePropagator.c2c3(psi)
            val y_n = r0.mag + r.mag + area * (psi * c3 - 1.0) / sqrt(c2)
            if(y_n < 0.0) {
              val newBounds = ((-(r0.mag + r.mag) * sqrt(c2) / area + 1.0) / c3, psiBounds._2)
              Y((newBounds._1 + newBounds._2) * 0.5, newBounds, 0.0, 0.0)
            } else {
              val chi = sqrt(y_n / c2)
              val dt_n = (chi * chi * chi * c3 + area * sqrt(y_n)) / rtmu
              val newBounds = if(dt_n <= dt) (psi, psiBounds._2) else (psiBounds._1, psi)
              Y((newBounds._1 + newBounds._2) * 0.5, newBounds, dt_n, y_n)
            }
          }

        val y = Y()
        val f = 1.0 - y / r0.mag
        val g = area * sqrt(y / mu)
        val dg = 1.0 - y / r.mag
        ((r - r0 * f) / g, (r * dg - r0) / g)
      }
    }
  }

  /**
   * See Algorithm 11 of Vallado. This has been enhanced to take into account
   * the direction of travel. As described in the text, the direction of travel
   * is not knowable, so if the final point is more than 0.5 revs out, the TOF
   * return is actually the time to get from the final to the initial point.
   * Without knowing h or some other quantity, the correct value is unknowable
   * as two points only doesn't tell you if the orbit is prograde or retrograde.
   */
  def timeOfFlight(state : ECIStateVector, r: Vector3): Double = {
    val ni = state.position.normalized
    val nf = r.normalized
    val cosdNu = max(-1.0, min(1.0, ni * nf))
    val axis = ni ⨯ nf
    val sindNu = max(-1.0, min(1.0, axis.mag * signum(state.h * axis)))
    val dNu = atan2(sindNu, cosdNu)

    if (Pi - abs(dNu) < 1.0E-6) state.toKeplerElements.period * 0.5 else {
      val rim = state.position.mag
      val rfm = r.mag
      val rr = rim * rfm
      val k = rr * (1.0 - cosdNu)
      val l = rim + rfm
      val m = rr * (1 + cosdNu)
      val p = state.toKeplerElements.p
      val a = m * k * p / ((2.0 * m - l * l) * p * p + 2.0 * k * l * p - k * k)
      val f = 1.0 - rfm / p * (1 - cosdNu)
      val g = rr * sindNu / sqrt(WGS84.GM * p)

      val dt = if (a > 0.0) {
        val df = sqrt(WGS84.GM / p) * tan(0.5 * dNu) * ((1.0 - cosdNu) / p - 1.0 / rim - 1.0 / rfm)
        val cosdE = max(-1.0, min(1.0, 1.0 - rim / a * (1.0 - f)))
        val sindE = max(-1.0, min(1.0, -rr * df / sqrt(WGS84.GM * a)))
        g + sqrt(a * a * a / WGS84.GM) * (atan2(sindE, cosdE) - sindE)
      } else if (a < 0.0) {
        val coshdH = 1.0 + (f - 1.0) * rim / a
        val dH = acosh(coshdH)
        g + sqrt(-a * a * a / WGS84.GM) * (sinh(dH) - dH)
      } else {
        val c = sqrt(rim * rim + rfm * rfm - 2.0 * rr * cosdNu)
        val s = 0.5 * (l + c)
        2.0 / 3.0 * sqrt(0.5 * s * s * s / WGS84.GM) * (1.0 - pow((s - c) / s, 1.5))
      }
      if (dt < 0) dt + state.toKeplerElements.period else dt
    }
  }
}
