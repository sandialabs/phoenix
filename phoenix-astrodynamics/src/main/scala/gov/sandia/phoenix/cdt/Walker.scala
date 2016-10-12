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

package gov.sandia.phoenix.cdt

import gov.sandia.phoenix.geometry.{Degrees, Angle}
import gov.sandia.phoenix.elements.kepler.CircularInclinedKeplerElements
import gov.sandia.phoenix.orbits.OrbitPlane

/**
 * http://en.wikipedia.org/wiki/Satellite_constellation#Walker_Constellation
 * http://arc.aiaa.org/doi/pdf/10.2514/6.IAC-04-A.5.01
 * http://ccar.colorado.edu/asen5050/projects/projects_2004/firestone/
 */
object Walker {
  /**
   * Create a Walker Delta constellation
   */
  def delta(range : Double, initialPlane : OrbitPlane, t : Int, p : Int, f : Int, u0 : Angle) =
    walker(360.0, range, initialPlane, t, p, f, u0)

  def star(range : Double, initialPlane : OrbitPlane, t : Int, p : Int, f : Int, u0 : Angle) =
    walker(180.0, range, initialPlane, t, p, f, u0)

  private def walker(theta : Double, range : Double, initialPlane : OrbitPlane, t : Int, p : Int, f : Int, u0 : Angle) = {
    val dnu = f * theta / t
    val planes = (0 until p) map { i => initialPlane.phase(Degrees(i * theta / p)) }
    (0 until t) map { i =>
      val plane = planes(i % p)
      new CircularInclinedKeplerElements(range, plane.inclination, plane.rightAscension, (u0 + Degrees(i * dnu)).constrainUnsigned)
    }
  }
}
