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

import gov.sandia.phoenix.orbits.OrbitPlane

import scala.math._
import gov.sandia.phoenix.math._
import gov.sandia.phoenix.time.JD
import gov.sandia.phoenix.geometry._

object LaunchWindowFunctions {
  def launchAzimuths(inc : Angle, lat : Angle) : List[Angle] = oasin(inc.cos / lat.cos) match {
    case Some(β) => β.constrainUnsigned :: β.supplement.constrainUnsigned :: Nil
    case _ => Nil
  }

  def geometries(plane : OrbitPlane, loc : Geodetic) = launchAzimuths(plane.inclination, Degrees(loc.latitude)) map { β =>
      val λinitial = Radians(acos(β.cos / plane.inclination.sin))
      val λ = if(plane.direct == (loc.latitude >= 0.0)) λinitial else λinitial.explement
      LaunchGeometry(loc, plane, β, λ)
    }
  
  def windows(plane : OrbitPlane, loc : Geodetic, t : JD) = geometries(plane, loc) map { _.window(t) }
}
