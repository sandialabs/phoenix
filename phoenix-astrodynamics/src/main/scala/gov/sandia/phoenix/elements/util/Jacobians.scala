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

package gov.sandia.phoenix.elements.util

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.{Geodetic, Vector3}
import gov.sandia.phoenix.propagators.ECIStateVectorPropagator
import gov.sandia.phoenix.time.JD

/**
 * Functions for computing jacobians of elements.
 */
object Jacobians {
    def perturb(p : Vector3, d : Double) = scala.collection.immutable.Vector(
      Vector3(p.x + d, p.y, p.z),
      Vector3(p.x - d, p.y, p.z),
      Vector3(p.x, p.y + d, p.z),
      Vector3(p.x, p.y - d, p.z),
      Vector3(p.x, p.y, p.z + d),
      Vector3(p.x, p.y, p.z - d))

  def jacobian(g : Geodetic, dt: Double, epoch: JD, state: ECIStateVector, delta: Double = 500.0) =
    state.jacobian(this, delta) { (states, delta) =>
      val sez = g.toSEZFrame
      val azelrs = states.map(_.state(dt).position).map(epoch.ECItoECEF).map(sez.toAzElR)

      Array.tabulate(3, 6){
        case (0, 0) => 0.5 * (azelrs(0).range - azelrs(1).range) / delta
        case (0, 1) => 0.5 * (azelrs(2).range - azelrs(3).range) / delta
        case (0, 2) => 0.5 * (azelrs(4).range - azelrs(5).range) / delta
        case (0, 3) => 0.5 * (azelrs(6).range - azelrs(7).range) / delta
        case (0, 4) => 0.5 * (azelrs(8).range - azelrs(9).range) / delta
        case (0, 5) => 0.5 * (azelrs(10).range - azelrs(11).range) / delta

        case (1, 0) => 0.5 * (azelrs(0).azimuth - azelrs(1).azimuth) / delta
        case (1, 1) => 0.5 * (azelrs(2).azimuth - azelrs(3).azimuth) / delta
        case (1, 2) => 0.5 * (azelrs(4).azimuth - azelrs(5).azimuth) / delta
        case (1, 3) => 0.5 * (azelrs(6).azimuth - azelrs(7).azimuth) / delta
        case (1, 4) => 0.5 * (azelrs(8).azimuth - azelrs(9).azimuth) / delta
        case (1, 5) => 0.5 * (azelrs(10).azimuth - azelrs(11).azimuth) / delta

        case (2, 0) => 0.5 * (azelrs(0).elevation - azelrs(1).elevation) / delta
        case (2, 1) => 0.5 * (azelrs(2).elevation - azelrs(3).elevation) / delta
        case (2, 2) => 0.5 * (azelrs(4).elevation - azelrs(5).elevation) / delta
        case (2, 3) => 0.5 * (azelrs(6).elevation - azelrs(7).elevation) / delta
        case (2, 4) => 0.5 * (azelrs(8).elevation - azelrs(9).elevation) / delta
        case (2, 5) => 0.5 * (azelrs(10).elevation - azelrs(11).elevation) / delta
      }
    }

  def jac(g : Geodetic, prop: ECIStateVectorPropagator, t: JD, delta: Double = 500.0) = {
    val dt = if (prop.epoch > t)
      -(t until prop.epoch).getDurationSeconds.doubleValue
    else
      (prop.epoch until t).getDurationSeconds.doubleValue()
    Jacobians.jacobian(g, dt, prop.epoch, prop.state, delta)
  }
}
