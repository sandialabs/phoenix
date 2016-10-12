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

package gov.sandia.phoenix.propagators

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.solarsystem.Luna
import gov.sandia.phoenix.time.JD

object MoonPropagator extends Propagator {
  override def state(t: JD): Option[ECIStateVector] = {
    val position = Luna.position(t)
    val delta = 60
    val position2 = Luna.position(t plusSeconds delta)
    val velocity = (position2 - position) / delta
    Some(ECIStateVector(position, velocity))
  }
  def getInstance = this
}

