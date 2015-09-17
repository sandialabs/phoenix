package gov.sandia.phoenix.launch

import gov.sandia.phoenix.orbits.OrbitPlane
import gov.sandia.phoenix.time.TimeBuilder
import gov.sandia.phoenix.constants.Orbit
import gov.sandia.phoenix.geometry.{Geodetic, Degrees}

object LWFApp extends App {
  println(getClass.getName)

  val t = TimeBuilder(2014, 1, 1, 3)
  val f = t plusDays 1
  val interval = t until f
  val i = Degrees(75.0)
  val Ω = Degrees(125.15)

  val lat = 45.0
  val locations = Array(Geodetic(45, lat, 0.0),
    Geodetic(45, -lat, 0.0),
    Geodetic(135, lat, 0.0),
    Geodetic(135, -lat, 0.0),
    Geodetic(225, lat, 0.0),
    Geodetic(225, -lat, 0.0),
    Geodetic(315, lat, 0.0),
    Geodetic(315, -lat, 0.0))

  val planes = Array(new OrbitPlane(Ω, i),
    new OrbitPlane(Ω, i.supplement),
    new OrbitPlane(Ω.explement, i),
    new OrbitPlane(Ω.explement, i.supplement))

  val windows = new LaunchSite(locations(0)).launchWindows(planes(0), t)
  windows.tail foreach { window =>
    println("Perigee Launch")
    val ps = window.perigeeLaunchState(Orbit.GEO_RADIUS_M)
    println(ps)
    println("Direct (Omega + Nu) Method")
    val fs = window.apogeeLaunchState(Degrees(45.0), Orbit.GEO_RADIUS_M)
    println(fs)
    println(ps.state.position dist fs.state.position)
    println("Axis distance: " + (ps.a - fs.a))
  }
}
