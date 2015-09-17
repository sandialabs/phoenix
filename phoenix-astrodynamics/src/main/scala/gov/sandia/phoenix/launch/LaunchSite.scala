package gov.sandia.phoenix.launch

import gov.sandia.phoenix.orbits.OrbitPlane
import gov.sandia.phoenix.time.JD
import gov.sandia.phoenix.geometry.Geodetic

/**
 * See section 6.4 of Vallado
 */
class LaunchSite(val location : Geodetic) {
  def launchWindows(orbitPlane : OrbitPlane, t : JD) = LaunchWindowFunctions.windows(orbitPlane, location, t)
}