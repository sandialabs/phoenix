package gov.sandia.phoenix.attitudes

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.Axes

/**
 * Compute the coordinate frame of the satellite in the NTW coordinate
 * system.  See Vallado, p. 162.  This CS points outward radially (N - note
 * that N is not exactly along the radial vector in non-circular orbits),
 * along the direction of velocity (T), and normal to the orbital plane
 *  (W) as defined by the cross product W = N x T.
 * @param t Time at which to compute the frame.
 * @return NTW frame for this sat.
 */
object NTW extends AttitudeModel {
  def attitude(state : ECIStateVector) = Some(Axes.YX(state.velocity, state.position))
}
