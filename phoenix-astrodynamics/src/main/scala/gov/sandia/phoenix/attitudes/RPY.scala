package gov.sandia.phoenix.attitudes

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.Axes

/**
 * Coordinate system with:
 * X - Roll Axis - Points forward out of a vehicle (Principal)
 * Y - Pitch Axis - Points to the right if sitting in the driver/pilot seat (RHR)
 * Z - Yaw Axis - Points down (Secondary)
 */
object RPY extends AttitudeModel {
  def attitude(state : ECIStateVector) = Some(Axes.XZ(state.velocity, -state.position))
}
