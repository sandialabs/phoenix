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

package gov.sandia.phoenix.geometry

import gov.sandia.phoenix.constants.WGS84
import org.scalatest.FunSuite

import scala.math._

class GeodeticVsEllipsoidComparisonTest extends FunSuite {
  test("Equator") {
    val geo = Geodetic(0, 0, 0)
    val ecef = geo.toECEF
    val outer = ecef * 2
    val ray = new Ray(outer, (ecef - outer).normalized)
    val p = WGS84.ellipsoid.closestIntersection(ray)
    val geo2 = p.toGeodetic
    assert(abs(geo2.elevation) <= 1E-9)
  }
  
  test("Pole") {
    val geo = Geodetic(0, 90, 0)
    val ecef = geo.toECEF
    val outer = ecef * 2
    val ray = new Ray(outer, (ecef - outer).normalized)
    val p = WGS84.ellipsoid.closestIntersection(ray)
    val geo2 = p.toGeodetic
    assert(abs(geo2.elevation) <= 1E-9)
  }
  
  test("Mid-Latitude") {
    val geo = Geodetic(0, 45, 0)
    val ecef = geo.toECEF
    val outer = ecef * 2
    val ray = new Ray(outer, (ecef - outer).normalized)
    val p = WGS84.ellipsoid.closestIntersection(ray)
    val geo2 = p.toGeodetic
    assert(abs(geo2.elevation) <= 1E-9)
  }
}
