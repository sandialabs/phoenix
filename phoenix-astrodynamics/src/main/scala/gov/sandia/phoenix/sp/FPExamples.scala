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

import clojure.java.api.Clojure
import gov.sandia.phoenix.constants.WGS84
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.elements.tle.TLE
import gov.sandia.phoenix.geometry.Vector3
import gov.sandia.phoenix.numerics.{FixedStepSizeController, RK_4}
import gov.sandia.phoenix.propagators.sgp4.SGP4
import gov.sandia.phoenix.solarsystem.{Luna, Sol}
import gov.sandia.phoenix.time.TimeBuilder
import scala.math._

object FPExamples extends App {
  val name = "0 ISS (ZARYA)"
  val l1 = "1 25544U 98067A   15124.08281398  .00011464  00000-0  16622-3 0  9998"
  val l2 = "2 25544 051.6470 304.3055 0005146 296.1365 166.6193 15.56342228941175"
  val tle = TLE(Some(name), l1, l2)
  val epoch = tle.epoch
  val propagator = SGP4(tle)

  //Initial conditions
  val initialState = propagator.unsafe_state(epoch)

  //Used for drag and solar radiation
  val areaToMassRatio = 0.1

  //Drag really only matters for LEOs and HEOs and that you must have a good dragCoefficient and areaToMassRatio for it
  // to work well. The defaults are DragForce(2.2, 0.02)
  val drag = DragForce(2.2, 0.02)

  //Solar Radiation Pressure
  val srp = SRP(areaToMassRatio, 0.21)

  //Create a force model with all of the selected forces
  //val fm = new SPModel(Set(Earth2BodyGravity, Luna, Sol, drag, srp))
//  val fm = new SPModel(Set(Earth2BodyGravity))
  //Could also use J2Gravity
//  val fm = new SPModel(Set(J2Gravity, Luna, Sol, srp))
  val fm = new SPModel(Set(J2Gravity))

  //Create a differential equation that takes time and state
  def dfdx(t : Double, y : IndexedSeq[Double]) = {
    val state = ECIStateVector(Vector3(y(0), y(1), y(2)), Vector3(y(3), y(4), y(5)))
    val f = fm ΣF (epoch plusSeconds t, state)
    Array(y(3), y(4), y(5), f.x, f.y, f.z)
  }

  //Create a numerical integrator
  val solver = RK_4.createSolver(dfdx, FixedStepSizeController)

  //Solve the problem
  val states = solver((0.0, initialState.toArray : IndexedSeq[Double]), 86400, 10) map { case (dt, posvel) => (dt, posvel) }

  //Dump the values
  val statesStr =
  "dt (sec), x (m), y (m), z (m), dx (m/s), dy (m/s), dz (m/s)\n" +
    (states map { case (dt, state) => dt + "," + state.mkString(",") }).reduce(_ + "\n" + _)

  Clojure.`var`("clojure.core", "spit").invoke("states.csv", statesStr)

  //compute the deltas
  val rmsDeltas = states map { case (dt, state) =>
    val t = epoch plusSeconds dt
    val tleState = propagator.unsafe_state(t)
    val rms = sqrt(state zip tleState.toArray map { case (a, b) => (a - b) * (a - b) } sum)
    (dt, rms)
  }

  //Dump the RMS errors
  val rmsStr =
    "dt (sec), RMS (m)\n" +
      (rmsDeltas map { case (dt, rms) => dt + "," + rms }).reduce(_ + "\n" + _)

  Clojure.`var`("clojure.core", "spit").invoke("rms.csv", rmsStr)

  //Just print out all of the gravity model outputs for a point on the surface of the Earth. Should be somewhere in the
  //9.8 m/s/s range.
  def realityCheck = {
    val initialState = ECIStateVector(Vector3(WGS84.R_POLAR_M, 0, 0), Vector3(0, 0, 0))
    val epoch = TimeBuilder(2015, 1, 1)

    //Pick one
    val earthGravities = Array(Earth2BodyGravity, DefaultEGM96GravityForce)

    //This is just a reality check.
    println("Gravitational acceleration from the Earth for multiple models:")
    earthGravities foreach { gravityModel =>
      println(gravityModel.acceleration(epoch, initialState).mag)
    }
  }
}
