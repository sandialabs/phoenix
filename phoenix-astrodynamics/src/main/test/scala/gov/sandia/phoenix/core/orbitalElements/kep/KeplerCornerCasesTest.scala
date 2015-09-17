package gov.sandia.phoenix.core.orbitalElements.kep

import gov.sandia.phoenix.constants._
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry._
import org.scalatest.{FunSuite, Matchers}

class KeplerCornerCasesTest extends FunSuite with Matchers
{
  test("Elliptical Equatorial")
  {
    val pos = Vector3(Orbit.GEO_RADIUS_M, 0, 0)
    val vel = Vector3(0, 1.1 * Orbit.GEO_VELOCITY_M_PER_SEC, 0)
    val pv = ECIStateVector(pos, vel)
    val keps = pv.toKeplerElements
    keps.e should not equal 0.0
    keps.inclination.degrees should equal (0.0)
  }

  test("Circular Inclined")
  {
    val pos = Vector3(Orbit.GEO_RADIUS_M, 0, 0)
    val vel = Vector3(0, 1, 1).normalized * Orbit.GEO_VELOCITY_M_PER_SEC
    val pv = ECIStateVector(pos, vel)
    val keps = pv.toKeplerElements
    keps.e should be < 1E-3
    keps.inclination.degrees should not equal 0.0
  }

  test("Circular Equatorial")
  {
    val pos = Vector3(Orbit.GEO_RADIUS_M, 0, 0)
    val vel = Vector3(0, Orbit.GEO_VELOCITY_M_PER_SEC, 0)
    val pv = ECIStateVector(pos, vel)
    val keps = pv.toKeplerElements
    keps.e should be < 1E-3
    keps.inclination.degrees should equal (0.0)
  }
}