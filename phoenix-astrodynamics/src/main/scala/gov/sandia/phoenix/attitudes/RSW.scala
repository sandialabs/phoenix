package gov.sandia.phoenix.attitudes

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.Axes

/**
 * Compute the frame frame this state in the RSW coordinate system.
 * See Vallado, p. 162.  This CS points outward radially (R), along
 * the direction of velocity (S - note, this is the general direction, not
 * exactly the velocity direction vector), and normal to the orbital plane
 *  (W) as defined by the cross product W = R x S.
 */
object RSW extends AttitudeModel {
  def attitude(state : ECIStateVector) = Some(Axes.XY(state.position, state.velocity))
}
