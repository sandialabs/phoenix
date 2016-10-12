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
