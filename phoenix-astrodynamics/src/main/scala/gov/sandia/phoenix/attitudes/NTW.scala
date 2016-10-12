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
