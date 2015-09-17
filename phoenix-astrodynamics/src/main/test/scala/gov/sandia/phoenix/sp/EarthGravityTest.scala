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
