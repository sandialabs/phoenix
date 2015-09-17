package gov.sandia.phoenix.attitudes

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.{Vector3, Axes}

/**
 * An attitude model in which nadir points in the direction of some target location and the secondary axis is in the
 * orbit plane in the direction of the velocity vector.
 * @param state
 * @param target
 */
case class LookAt(target : () => Vector3) extends AttitudeModel {
  def attitude(state : ECIStateVector) = Some(Axes.ZY(target() - state.position, state.velocity))
}
