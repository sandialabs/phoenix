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