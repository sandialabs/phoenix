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

package gov.sandia.phoenix.eclipses

import gov.sandia.phoenix.constants.{Celestial, WGS84}
import gov.sandia.phoenix.geometry.{Sphere, Vector3}
import gov.sandia.phoenix.solarsystem.Sol
import gov.sandia.phoenix.time.JD

object EclipseFunctions {
  def umbra(t : JD) = Umbra(Sol.sphere(t), WGS84.sphere)
  def umbra(sunPos : Vector3) = Umbra(Sphere(sunPos, Celestial.SOLAR_RADIUS), WGS84.sphere)
  def penumbra(t : JD) = Penumbra(Sol.sphere(t), WGS84.sphere)
  def penumbra(sunPos : Vector3) = Penumbra(Sphere(sunPos, Celestial.SOLAR_RADIUS), WGS84.sphere)
}
