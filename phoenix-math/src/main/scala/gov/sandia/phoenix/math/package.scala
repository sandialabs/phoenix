package gov.sandia.phoenix

import gov.sandia.phoenix.constants.WGS84
import gov.sandia.phoenix.geometry._

import scala.math._
  
/**
 * See http://en.wikipedia.org/wiki/Hyperbolic_function for hyperbolic
 * function references. See also: http://www.devx.com/vb2themax/Tip/19024.
 */
package object math {
  val FLT_EPSILON = 1.19209290E-07f

  def cot(theta : Double) = 1.0 / tan(theta)
  def sec(theta : Double) = 1.0 / cos(theta)
  def csc(theta : Double) = 1.0 / sin(theta)

  def acot(theta : Double) = 0.5 * Pi - atan(theta)

  def asinh(x : Double) = log(x + sqrt(x * x + 1.0))
  def acosh(x : Double) = log(x + sqrt(x * x - 1.0))
  def atanh(x : Double) = 0.5 * log((1.0 + x) / (1.0 - x))
  def asech(x : Double) = log((1.0 + sqrt(1.0 - x * x)) / x)
  def acsch(x : Double) = log(1.0 / x + sqrt(1.0 + x * x) / abs(x))
  def acoth(x : Double) = 0.5 * log((x + 1.0) / (x - 1.0))

  def sinc(x : Double) = if(x == 0.0) 1.0 else sin(x) / x

  def r(rand : () => Double) = sphericalPoint(rand(), rand())
    
  val sphericalPoint = (a : Double, b : Double) => {
    val z = 2.0 * a - 1.0
    val t = 2.0 * Pi * b
    val r = sqrt(1.0 - z * z)
    new Vector3(r * cos(t), r * sin(t), z)
  }
    
  def randomSphericalPoint = r { () => random }

  def correlation_coefficient(x : Seq[Double], y : Seq[Double]) = {
    require(x.length == y.length)
    val n = x.length
    val sx = x.sum
    val sy = y.sum
    val num = n * sop(x, y) - sx * sy
    val den = sqrt(n * sop(x, x) - sx * sx) * sqrt(n * sop(y, y) - sy * sy)
    num / den
  }

  /**
   * Capped asin function. Caps argument at [-1, 1] to prevent cases like
   * asin(1.00000000000001) returning NaN. This should only be used if you
   * are confident that out of bound values are the result of double precision
   * issues. In other words, you expect the values to be valid.
   */
  def casin(x : Double) = asin(min(max(x, -1.0), 1.0))

  /**
   * Capped acos function. Caps argument at [-1, 1] to prevent cases like
   * acos(1.00000000000001) returning NaN. This should only be used if you
   * are confident that out of bound values are the result of double precision
   * issues. In other words, you expect the values to be valid.
   */
  def cacos(x : Double) = acos(min(max(x, -1.0), 1.0))

  def oasin(sinx : Double) = if(abs(sinx) <= 1.0) Some(Angle.asin(sinx)) else None
  def oacos(cosx : Double) = if(abs(cosx) <= 1.0) Some(Angle.acos(cosx)) else None

  val sop : (Seq[Double], Seq[Double]) => Double = (x, y) => (x zip y).map(xy => xy._1 * xy._2).sum

  /**
   * Evaluate a polynomial using horner's method. This is non-tail recursive, 
   * but I don't expect the stack depth to be very deep given that most 
   * polynomials aren't enormous.
   */
  def horner(t : Double, a : Seq[Double], i : Int = 0) : Double =
    if (i >= a.length - 1) a(a.length - 1) else a(i) + horner(t, a, i + 1) * t
  
  /**
   * Constrain x to be in the range of [lo, hi)
   */
  final def constrain(x : Double, lo : Double, hi : Double) : Double = x match {
    case _ if x < lo => constrain(x + (hi - lo), lo, hi)
    case _ if x >= hi => constrain(x - (hi - lo), lo, hi)
    case _ => x
  }
  
  /**
   * Constrain a value to be in the range of [0, 360).
   */
  def constrain0to360 = constrain(_ : Double, 0.0, 360.0)
  
    /**
   * Constrain a value to be in the range of [-180, 180).
   */
  def constrainMinus180to180 = constrain(_ : Double, -180.0, 180.0)

  /**
   * Compute the haversine distance between two points on the Earth.  This is
   * better than other methods because it has better precision when the points
   * are close together.<p>
   * http://www.faqs.org/faqs/geography/infosystems-faq/<p>
   * http://www.movable-type.co.uk/scripts/latlong.html<p>
   * http://mathforum.org/library/drmath/view/51879.html<p>
   *   http://en.wikipedia.org/wiki/Haversine_formula<p>
   * @param lat1 point 1 latitude
   * @param lon1 point 1 longitude
   * @param lat2 point 2 latitude
   * @param lon2 point 2 longitude
   * @param R radius of the sphere.
   * @return
   */
  def distHaversine(lat1Degrees : Double,
                    lon1Degrees : Double,
                    lat2Degrees : Double,
                    lon2Degrees : Double,
                    R : Double = 6371000) = {
    val lat1 = lat1Degrees.toRadians
    val lon1 = lon1Degrees.toRadians
    val lat2 = lat2Degrees.toRadians
    val lon2 = lon2Degrees.toRadians

    val dLat = lat2 - lat1
    val dLon = lon2 - lon1
    val sindLat2 = sin(dLat * 0.5)
    val sindLon2 = sin(dLon * 0.5)
    val a = sindLat2 * sindLat2 + cos(lat1) * cos(lat2) * sindLon2 * sindLon2
    val c = 2.0 * atan2(sqrt(a), sqrt(1.0 - a))
    R * c
  }

  /**
   * Calculate geodesic distance (in m) between two points specified by latitude/longitude (in numeric degrees)
   * using Vincenty inverse formula for ellipsoids
   * http://www.movable-type.co.uk/scripts/latlong-vincenty.html
   */
  def distVincenty(lat1Degrees : Double, lon1Degrees : Double, lat2Degrees : Double, lon2Degrees : Double) = {
    val lat1 = toRadians(lat1Degrees)
    val lon1 = toRadians(lon1Degrees)
    val lat2 = toRadians(lat2Degrees)
    val lon2 = toRadians(lon2Degrees)

    val a = WGS84.R_EQ_M
    val b = WGS84.R_POLAR_M
    val f = WGS84.FLATTENING
    val L = lon2 - lon1
    val u1 = atan((1-f) * tan(lat1))
    val u2 = atan((1-f) * tan(lat2))
    val sin_u1 = sin(u1)
    val cos_u1 = cos(u1)
    val sin_u2 = sin(u2)
    val cos_u2 = cos(u2)
    var lambda = L
    var lambda_pi = Pi * 2.0
    var cos_sq_alpha = 0.0
    var sigma = 0.0
    var sinSigma = 0.0
    var cosSigma = 0.0
    var cos2sigma_m = 0.0
    while(abs(lambda-lambda_pi) > 1E-12)
    {
      val sin_lambda = sin(lambda)
      val cos_lambda = cos(lambda)
      sinSigma = sqrt((cos_u2 * sin_lambda) * (cos_u2*sin_lambda) +
        (cos_u1*sin_u2-sin_u1*cos_u2*cos_lambda) * (cos_u1*sin_u2-sin_u1*cos_u2*cos_lambda))
      cosSigma = sin_u1*sin_u2 + cos_u1*cos_u2*cos_lambda
      sigma = atan2(sinSigma, cosSigma)
      val alpha = asin(cos_u1 * cos_u2 * sin_lambda / sinSigma)
      cos_sq_alpha = cos(alpha) * cos(alpha)
      cos2sigma_m = cosSigma - 2*sin_u1*sin_u2/cos_sq_alpha
      val cc = f/16*cos_sq_alpha*(4+f*(4-3*cos_sq_alpha))
      lambda_pi = lambda
      lambda = L + (1-cc) * f * sin(alpha) *
        (sigma + cc*sinSigma*(cos2sigma_m+cc*cosSigma*(-1+2*cos2sigma_m*cos2sigma_m)))
    }
    val usq = cos_sq_alpha*(a*a-b*b)/(b*b)
    val aa = 1 + usq/16384*(4096+usq*(-768+usq*(320-175*usq)))
    val bb = usq/1024 * (256+usq*(-128+usq*(74-47*usq)))
    val deltaSigma = bb*sinSigma*(cos2sigma_m+bb/4*(cosSigma*(-1+2*cos2sigma_m*cos2sigma_m)-
      bb/6*cos2sigma_m*(-3+4*sinSigma*sinSigma)*(-3+4*cos2sigma_m*cos2sigma_m)))
    b*aa*(sigma-deltaSigma)
  }

  def ECEFtoGEO(ecef : Vector3) = {
    val rd = hypot(ecef.x, ecef.y)

    //Technically, if rd == 0, we are at a pole and the longitude is
    //undefined.  For numerical stability, we arbitrarily choose lon = 0.0
    if (rd != 0.0) {
      val sinAlpha = ecef.y / rd
      val cosAlpha = ecef.x / rd
      val alpha = atan2(sinAlpha, cosAlpha)
      //Supposed to add some time to this.  Figure it out later.
      val longitude = alpha + 0.0

      //initial guesses
      var tanPhi = ecef.z / rd
      var phi = atan(tanPhi)
      var C = 0.0
      var num = 0.0
      var den = 0.0
      var latitude = 0.0

      do {
        val sinPhi = sin(phi)
        C = WGS84.R_EQ_M / sqrt(1.0 - WGS84.e_squared * sinPhi * sinPhi)
        tanPhi = (ecef.z + C * WGS84.e_squared * sinPhi) / rd
        phi = atan(tanPhi)

        num = phi - latitude
        den = phi + latitude
        latitude = phi
      } while ((num / den) > 1E-10)

      Geodetic(longitude.toDegrees, latitude.toDegrees, rd / cos(latitude) - C)
    } else {
      val sign = signum(ecef.z)
      Geodetic(0.0, sign * 90.0, sign * ecef.z - WGS84.R_POLAR_M)
    }
  }

  def ECItoRaDec(eci : Vector3) = RaDec(
    RightAscension.fromRadians(atan2(eci.y, eci.x)),
    Declination.fromRadians(atan2(eci.z, hypot(eci.x, eci.y))))

  def ECItoRRadec(eci : Vector3) = RRaDec(eci.mag, atan2(eci.y, eci.x).toDegrees,
    atan2(eci.z, sqrt(eci.x * eci.x + eci.y * eci.y)).toDegrees)
}