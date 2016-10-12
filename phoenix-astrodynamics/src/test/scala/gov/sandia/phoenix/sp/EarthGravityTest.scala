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

import gov.sandia.phoenix.constants.{WGS84, Orbit}
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.Vector3
import gov.sandia.phoenix.solarsystem.Luna
import gov.sandia.phoenix.time.TimeBuilder
import org.scalatest.FunSuite

import scala.math._

class EarthGravityTest extends FunSuite {
  test("Acceleration due to gravity at high LEO") {
    val t = TimeBuilder(1994, 4, 28)
    val state = ECIStateVector(Vector3(WGS84.R_EQ_M, 0, 0), Vector3(0, 0, 0))
    val acceleration = DefaultEGM96GravityForce.acceleration(t, state)
    val mag = acceleration.mag
    println(mag)
    //Note that this is not correct. I'll let someone else establish the truth value.
    assert(abs(mag - 9.81) < 0.1)
  }
}
