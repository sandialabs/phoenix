package gov.sandia.phoenix.propagators

import gov.sandia.phoenix.constants.Time
import gov.sandia.phoenix.elements.sv.{ECEFStateVector, ECIStateVector}
import gov.sandia.phoenix.geometry.Vector3
import gov.sandia.phoenix.numerics.LagrangePolynomial3D
import gov.sandia.phoenix.time.JD

/**
 * A propagator that uses a Langrange polynomial to interpolate between states.
 */
class LagrangePropagator(times : Array[JD], locations : Array[Vector3]) extends Propagator {
  require(times.length > 0)
  val epoch = times.head
  val validInterval = times.head until times.last

  def state(t : JD) : Option[ECIStateVector] = if(validInterval contains t) Some {
    val ecef = interpolator(t.doubleValue)
    //Units are m / day
    val decef = interpolator.d(t.doubleValue)
    new ECEFStateVector(Vector3(ecef._1, ecef._2, ecef._3),
      Vector3(decef._1 / Time.SEC_PER_DAY, decef._2 / Time.SEC_PER_DAY, decef._3 / Time.SEC_PER_DAY)).toECI(t)
  } else None

  private val interpolator = new LagrangePolynomial3D(
    for(t <- times) yield t.doubleValue,
    for(l <- locations) yield l.x,
    for(l <- locations) yield l.y,
    for(l <- locations) yield l.z,
    12)
}