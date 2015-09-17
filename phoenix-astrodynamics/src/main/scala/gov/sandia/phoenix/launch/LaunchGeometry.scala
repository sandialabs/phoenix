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