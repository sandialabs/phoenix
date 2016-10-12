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

package gov.sandia.phoenix.orbits

import gov.sandia.phoenix.elements.kepler.KeplerElements
import gov.sandia.phoenix.geometry.{Degrees, _}
import gov.sandia.phoenix.solarsystem.Sol
import gov.sandia.phoenix.time.JD

trait OrbitPlaneFunctions extends IPlane {
  def inclination : Angle
  def rightAscension : Angle

  def direct = inclination.degrees >= 0.0 && inclination.degrees < 90.0
  def prograde = direct
  def retrograde = !direct

  def perigeePoint(omega : Angle, range : Double) = KeplerElements.q(rightAscension, inclination, omega) * (X_AXIS * range)

  def apogeePoint(omega : Angle, range : Double) = perigeePoint(omega + Degrees(180.0), range)

  def phase(angle : Angle) = new OrbitPlane((rightAscension + angle).constrainUnsigned, inclination)

  def beta(t : JD) = Angle.acos(Sol.direction(t) * n).complement
}

case class OrbitPlane(rightAscension : Angle, inclination : Angle) extends OrbitPlaneFunctions {
  val d = 0.0
  val n = rightAscension.rz * inclination.rx * Z_AXIS
}
