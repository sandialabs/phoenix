package gov.sandia.phoenix.launch

import gov.sandia.phoenix.constants.WGS84
import gov.sandia.phoenix.elements.kepler.EllipticalInclinedKeplerElements
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry._
import gov.sandia.phoenix.iod.IOD
import gov.sandia.phoenix.propagators.GeodeticPropagator
import gov.sandia.phoenix.time.JD

import scala.math._

case class LaunchWindow(geometry : LaunchGeometry, launchTime : JD) {
  val launchSiteState = GeodeticPropagator(geometry.location).unsafe_state(launchTime)
  def deltaV(launchState : ECIStateVector) = (launchState - launchSiteState).velocity

  /**
   * Compute the spherical triangle associated with this window. Vertices are at the launch site, the zero latitude
   * point corresponding to the launch site, and the ascending or descending node (Depending on whether the launch is
   * at ascend or descent.).
   */
  def sphericalTriangle = {
    val offset = if(geometry.descending) Pi else 0.0
    val v0 = Radians(geometry.plane.rightAscension.radians - offset).rz * X_AXIS * WGS84.R_EQ_M
    val v1 = Radians(geometry.plane.rightAscension.radians + geometry.λ.radians).rz * X_AXIS * WGS84.R_EQ_M
    val v2 = GeodeticPropagator(geometry.location).unsafe_state(launchTime).position
    new SphericalTriangle(v0, v1, v2)
  }

  def launchAzEl(launchState : ECIStateVector) =
    (!Axes.ZX(launchState.position, -Z_AXIS) * deltaV(launchState)).toAzEl

  /**
   * Compute the idealized Kepler elements as if a launch were to occur using this location as perigee and launching to
   * a given apogee range.
   * @param apogee Desired apogee for launch.
   * @return Kepler elements corresponding to these launch conditions.
   */
  def perigeeLaunchState(apogee : Double) = {
    val omega_plus_nu = Angle.asin(sin(geometry.location.latitude.toRadians) / geometry.plane.inclination.sin)

    //Note the the flip here only occurs in this function.
    val omega = if(geometry.descending) omega_plus_nu.supplement else omega_plus_nu

    val ra = apogee
    val rp = GeodeticPropagator(geometry.location).unsafe_state(launchTime).position.mag
    val a = 0.5 * (ra + rp)
    val p = rp * ra / a
    val e = sqrt(1.0 - p / a)
    new EllipticalInclinedKeplerElements(p, e, geometry.plane.inclination, geometry.plane.rightAscension, omega, Angle.ZERO)
  }

  def apogeeLaunchState(omega : Angle, range : Double) = {
    val omega_plus_nu = Angle.asin(sin(geometry.location.latitude.toRadians) / geometry.plane.inclination.sin)

    //Note the the flip here only occurs in this function.
    //val omega = if(geometry.descending) omega_plus_nu.supplement else omega_plus_nu
    val nu = if(geometry.descending) omega_plus_nu.supplement - omega else omega_plus_nu - omega

    val launch = GeodeticPropagator(geometry.location).unsafe_state(launchTime).position
    val apogee = geometry.plane.apogeePoint(omega, range)
    val sympt = AxisAngle(apogee, Angle.Pi).toQuaternion * launch
    val (p, e) = IOD.p(sympt, launch, apogee).get
    new EllipticalInclinedKeplerElements(p, e, geometry.plane.inclination, geometry.plane.rightAscension, omega, nu)
  }

  def perigeePropagator(apogee : Double) = perigeeLaunchState(apogee).toPropagator(launchTime)

  def elevatedLaunchState(elevation : Angle, apogee : Double) = {
    val nonElevatedState = perigeeLaunchState(apogee).state
    val rot = AxisAngle(nonElevatedState.h, -elevation).toQuaternion
    ECIStateVector(nonElevatedState.position, rot * nonElevatedState.velocity).keplers
  }
}