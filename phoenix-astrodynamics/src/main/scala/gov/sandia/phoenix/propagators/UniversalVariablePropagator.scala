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

package gov.sandia.phoenix.propagators

import gov.sandia.phoenix.constants.WGS84
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.math._

import scala.math._

/**
 * Propagate using the universal variable formulation as described on p. 101 of Vallado (3rd ed.). One issue I've seen
 * with this propagator is numerical instability with high eccentricities. For now, I'd suggest using the Kepler
 * elements equivalent propagator.
 */
object UniversalVariablePropagator {
  val tol = 1.0E-12

  def c2c3(psi : Double) = if(psi > tol) {
    val rt = sqrt(psi)
    ((1 - cos(rt)) / psi, (rt - sin(rt)) / (rt * rt * rt))
  } else if(psi < -tol) {
    val rt = sqrt(-psi)
    ((1 - cosh(rt)) / psi, (sinh(rt) - rt) / (rt * rt * rt))
  }
  else (1.0 / 2.0, 1.0 / 6.0)

  def fandg(r : Double, r0 : Double, chi : Double, psi : Double, c2 : Double,
            c3 : Double, dt : Double, mu : Double = WGS84.GM) = {
    val rtmu = sqrt(mu)
    val f = 1.0 - chi * chi * c2 / r0
    val g = dt - chi * chi * chi * c3 / rtmu
    val dg = 1.0 - chi * chi * c2 / r
    val df = rtmu * chi * (psi * c3 - 1.0) / (r * r0)
    (f, g, df, dg)
  }

  /**
   * Propagate the state of this state vector using the universal formulation of
   * Kepler's problem as described on p. 101 of Vallado.
   */
  def state(state : ECIStateVector, dt : Double, mu : Double = WGS84.GM) = {
    val r0 = state.position.mag
    val alpha = - (state.velocity * state.velocity) / mu + 2.0 / r0
    val rv = state.position * state.velocity
    val rtmu = sqrt(mu)

    val X0 = if(alpha > 1E-6) rtmu * dt * alpha
    else if(abs(alpha) < 1E-6) {
      val p = (state.h * state.h) / mu
      val s = 0.5 * acot(3 * sqrt(mu / (p * p * p)) * dt)
      val w = atan(cbrt(tan(s)))
      sqrt(p) * 2 * cot(2 * w)
    } else {
      val a = 1.0 / alpha
      val sign = signum(dt)
      val den = rv + sign * sqrt(-a * mu) * (1 - r0 * alpha)
      sign * sqrt(-a) * log(-2 * mu * alpha * dt / den)
    }

    def step(X : Double) = {
      val X2 = X * X
      val X3 = X2 * X
      val psi = X2 * alpha
      val (c2, c3) = c2c3(psi)
      val r = X2 * c2 + rv / rtmu * X * (1 - psi * c3) + r0 * (1 - psi * c2)
      val Xplus = X + (rtmu * dt - X3 * c3 - rv / rtmu * X2 * c2 - r0 * X * (1 - psi * c3)) / r
      (Xplus, c2, c3, r, psi)
    }

    def iterate(Xold : Double, Xnew : (Double, Double, Double, Double, Double)) :
    (Double, Double, Double, Double, Double) = if(abs(Xold - Xnew._1) < 1E-6)
      Xnew else iterate(Xnew._1, step(Xnew._1))

    val (x, c2, c3, r, psi) = iterate(X0, step(X0))

    val (f, g, df, dg) = fandg(r, r0, x, psi, c2, c3, dt)
    //    val f = 1 - x * x * c2 / r0
    //    val g = dt - x * x * x * c3 / rtmu
    //    val dg = 1 - x * x * c2 / r
    //    val df = rtmu * x * (psi * c3 - 1) / (r * r0)
    if(abs((f * dg - df * g) - 1) > 1E-6) throw new Exception("Failed to converge!")

    ECIStateVector(state.position * f + state.velocity * g,
      state.position * df + state.velocity * dg)
  }
}
