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
