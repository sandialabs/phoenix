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

import gov.sandia.phoenix.elements.sv.ECEFStateVector
import gov.sandia.phoenix.geometry.{Geodetic, Vector3}
import gov.sandia.phoenix.time.JD

case class GeodeticPropagator(location : Geodetic) extends Propagator {
  val stationaryECEFState = new ECEFStateVector(location.toECEF, Vector3(0, 0, 0))
  def state(t : JD) = Some(stationaryECEFState.toECI(t))
}
