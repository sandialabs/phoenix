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

package gov.sandia.phoenix.launch


import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.orbits.OrbitPlane
import gov.sandia.phoenix.propagators.GeodeticPropagator
import org.scalatest.FunSuite
import scala.math._
import gov.sandia.phoenix.numerics.Numerics
import gov.sandia.phoenix.geometry._
import gov.sandia.phoenix.constants.Orbit
import gov.sandia.phoenix.time.{TimeBuilder, JD}


class LaunchTest extends FunSuite {
  test("45 Degree Inclination 0 Degree RAAN Launch Southern Hemisphere") {
    val start = TimeBuilder(2014, 1, 1, 1)
    val location = Geodetic(90, -45, 0.0)
    val desiredInclinationDegreees = Degrees(45.0)
    val desiredRightAscensionDegrees = Degrees(0.0)
    val plane = new OrbitPlane(desiredRightAscensionDegrees, desiredInclinationDegreees)
    createSolution(start, location, plane)
  }

  def createSolution(windowStart : JD, launchLocation : Geodetic, plane : OrbitPlane) = {
    val site = new LaunchSite(launchLocation)
    val launchWindows = site.launchWindows(plane, windowStart)
    assert(launchWindows.size > 0, "There were no solutions!")
    launchWindows foreach { launchWindow =>
      val launchState = launchWindow.perigeeLaunchState(Orbit.LEO_RADIUS_MIN_M)
      assert(abs((launchState.inclination - plane.inclination).degrees) < 1.0E-10, "Inclinations not similar [ desired: " +
        plane.inclination.degrees + " desired: " + launchState.inclination.degrees + "].")
      assert(abs((launchState.OMEGA - plane.rightAscension).degrees) < 1.0E-10, "Right ascensions not similar [ desired: " +
        plane.rightAscension.degrees + " desired: " + launchState.OMEGA.degrees + "].")

    }
  }

  def testPolar(location : Geodetic, start : JD) = {
    val prop = GeodeticPropagator(location)
    val dt = Numerics.secant(0, 100){ x => asin(prop.unsafe_state(start plusSeconds x).position.normalized * Y_AXIS) }
    val raanTime = start plusSeconds dt
    val isRaan = prop.unsafe_state(raanTime).position
    val s = ECIStateVector(isRaan, Vector3(0, 0, Orbit.LEO_VELOCITY_MAX_M_PER_SEC))
    val keps = s.keplers
    assert(abs(90.0 - keps.inclination.degrees) <= 1.0E-10, "Orbit is not polar")
    assert(abs((keps.OMEGA % Angle.TwoPi).degrees) <= 1.0E-10, "RAAN is not 0 degrees")
    raanTime
  }

  test("Check many planes and launch sites using perigeeLaunchState.") {
    val t = TimeBuilder(2014, 1, 1, 3)

    val lat = 60.0
    val locations = Array(Geodetic(45, lat, 0.0),
      Geodetic(45, -lat, 0.0),
      Geodetic(135, lat, 0.0),
      Geodetic(135, -lat, 0.0),
      Geodetic(225, lat, 0.0),
      Geodetic(225, -lat, 0.0),
      Geodetic(315, lat, 0.0),
      Geodetic(315, -lat, 0.0))

    val i = Degrees(75.0)
    val Ω = Degrees(125.15)
    val planes = Array(new OrbitPlane(Ω, i),
      new OrbitPlane(Ω, i.supplement),
      new OrbitPlane(Ω.explement, i),
      new OrbitPlane(Ω.explement, i.supplement))

    locations foreach { location =>
      planes foreach { plane =>
        new LaunchSite(location).launchWindows(plane, t) foreach { window =>
          val launchState = window.perigeeLaunchState(Orbit.LEO_VELOCITY_MAX_M_PER_SEC)
          assert(launchState.inclination.degrees == plane.inclination.degrees)
          assert(launchState.OMEGA.degrees == plane.rightAscension.degrees)
          assert(window.launchTime >= t)
        }
      }
    }
  }
}
