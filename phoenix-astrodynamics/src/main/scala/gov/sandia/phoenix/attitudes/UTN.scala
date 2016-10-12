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
 * UTN is defined by these azes:
 * U - Up as defined by T x N
 * T - along track
 * N - Nadir (this is the defining vector)
 *
 * This should be used if following the STK default attitude model of "Nadir alignment with ECI velocity constraint."
 * http://www.agi.com/resources/help/online/stk/index.html?page=source%2Fextfile%2Fgator%2Feq-coordsys.htm
 */
object UTN extends AttitudeModel {
  def attitude(state : ECIStateVector) = Some(Axes.ZY(-state.position, state.velocity))
}
