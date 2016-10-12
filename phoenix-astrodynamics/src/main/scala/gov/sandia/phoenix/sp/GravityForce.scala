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

package gov.sandia.phoenix.sp

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.solarsystem._
import gov.sandia.phoenix.time.JD

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class GravityForce(val gravityModel : GravityModel, val degree : Int = 20, val order : Int = 20) extends SPForceProvider {
  def acceleration(t : JD, state : ECIStateVector) =
    t.ECEFtoECI(gravityModel(t.ECItoECEF(state.position), degree, order))
}

class EGM96GravityForce(degree : Int = 20, order : Int = 20) extends GravityForce(EGM96GravityModel, degree, order)
class WGS84_EGM96GravityForce(degree : Int = 20, order : Int = 20) extends GravityForce(WGS84_EGM96GravityModel, degree, order)
class WGS84GravityForce(degree : Int = 20, order : Int = 20) extends GravityForce(WGS84GravityModel, degree, order)
class JGM3GravityForce(degree : Int = 20, order : Int = 20) extends GravityForce(JGM3GravityModel, degree, order)

object DefaultEGM96GravityForce extends EGM96GravityForce(20, 20) {
  def getInstance = this
}
object DefaultWGS84_EGM96GravityForce extends WGS84_EGM96GravityForce(20, 20)
object DefaultWGS84GravityForce extends WGS84GravityForce(20, 20)
object DefaultJGM3GravityForce extends JGM3GravityForce(20, 20)
