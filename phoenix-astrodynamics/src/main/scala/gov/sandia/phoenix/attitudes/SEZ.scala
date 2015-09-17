package gov.sandia.phoenix.attitudes

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.{Axes, Z_AXIS}

/**
 * Coordinate system with:
 * S - South (Secondary)
 * E - East
 * Z - Zenith (Principal)
 */
object SEZ extends AttitudeModel {
  def attitude(state : ECIStateVector) = Some(Axes.ZX(state.position, -Z_AXIS))
}
