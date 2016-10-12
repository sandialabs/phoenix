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

import gov.sandia.phoenix.geometry.{Geodetic, Degrees, Angle}
import gov.sandia.phoenix.constants.WGS84
import gov.sandia.phoenix.orbits.OrbitPlane
import gov.sandia.phoenix.time.JD

case class LaunchGeometry(location : Geodetic, plane : OrbitPlane, β : Angle, λ : Angle) {
  val θgmst = plane.rightAscension + λ - Degrees(location.longitude)
  def UT(θ0hr : Angle) = Angle.rads0To2Pi((θgmst - θ0hr).radians) / WGS84.omega_Earth
  def launchTime(ti : JD) = {
    def inner(tf : JD) : JD = {
      val lt = tf plusSeconds UT(Degrees(tf.toGreenwichMeanSiderealTime0Hour))
      if(lt < ti) inner(tf plusDays 1) else lt
    }
    inner(ti.floor)
  }

  def window(t : JD) = LaunchWindow(this, launchTime(t))

  def descending = β.degrees > 90.0 && β.degrees <= 270.0
}