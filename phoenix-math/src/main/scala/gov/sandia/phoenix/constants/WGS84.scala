package gov.sandia.phoenix.constants

import gov.sandia.phoenix.geometry._

import scala.math._

/**
 * Constants defined as part of the WGS84 standard. To read more, take a look at
 * http://earth-info.nima.mil/GandG/wgs84/
 * http://earth-info.nima.mil/GandG/publications/tr8350.2/wgs84fin.pdf
 * <p>
 * @author [[mailto: markbastian@gmail.com Mark Bastian]]
 */
object WGS84 {
  //The big 4 constants from which others may be derived
  val R_EQ_KM = 6378.137
  val GM = 398600.4418E9 //m**3/(solar sec)**2
  val mu = GM / 1E9 //km**3/(solar sec)**2
  val omega_Earth = 0.0000729211585530 //rad/solar sec
  val C2 = -484.1653717E-6 //dimensionless

  val J2 = 0.0010826267 //dimensionless
  val J3 = -0.0000025327 //dimensionless
  val J4 = -0.0000016196 //dimensionless

  val e = 0.081819190842622  //Earth eccentricity
  val e_squared = e * e

  val R_POLAR_KM = R_EQ_KM * sqrt(1 - e_squared)
  val R_POLAR_M = R_POLAR_KM * 1000.0
  val R_EQ_M = R_EQ_KM * 1000.0
  val mu_M = mu * 1E9
  val TU = sqrt(R_EQ_KM * R_EQ_KM * R_EQ_KM / mu) //(solar sec) Vallado, p. 233

  val Cap_Omega_e_dot = 7.2921151467e-5
  //WGS-84 earth rotation rate, rad/s
  val FLATTENING = (R_EQ_M - R_POLAR_M) / R_EQ_M
  val INV_FLATTENING = 1.0 / FLATTENING

  def toDU(d: Double) = d / R_EQ_M

  val sphere = new Sphere(ORIGIN, R_EQ_M)
  val ellipsoid = new Ellipsoid(ORIGIN, new Vector3(R_EQ_M, R_EQ_M, R_POLAR_M))
  val ball = Ball(ORIGIN, R_EQ_M)
}
