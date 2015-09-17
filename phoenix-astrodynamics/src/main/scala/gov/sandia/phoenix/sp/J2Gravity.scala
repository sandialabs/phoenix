package gov.sandia.phoenix.sp

import gov.sandia.phoenix.constants.WGS84
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.Vector3
import gov.sandia.phoenix.time.JD

import scala.math._

/**
 * See Fundamentals of Astrodynamics and Applications, 3rd Ed., p. 590
 */
object J2Gravity extends SPForceProvider {
  override def acceleration(t: JD, state: ECIStateVector): Vector3 = {
    val x = state.position.x
    val y = state.position.y
    val z = state.position.z
    val r = state.position.mag

    val Re = WGS84.R_EQ_M //6378136.3
    val J2 = WGS84.J2 //0.00108262999
    //    val Re = 6378136.3
    //    val J2 = 0.00108262999
    val p = -WGS84.GM / (r * r * r)

    val a = -1.5 * J2 * WGS84.GM * Re * Re / pow(r, 5)
    val b = 5.0 * z * z / (r * r)

    val ax = x * (p + a * (1.0 - b))
    val ay = y * (p + a * (1.0 - b))
    val az = z * (p + a * (3.0 - b))

    Vector3(ax, ay, az)
  }

  def getInstance = this
}
