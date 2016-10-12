/*
 * Copyright (c) 2016 Sandia Corporation. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by the
 * terms of this license.
 * You must not remove this notice, or any other, from this software.
 *
 * Contributors:
 * - Mark Bastian: Original author.
 * - See Git logs.
 */

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
