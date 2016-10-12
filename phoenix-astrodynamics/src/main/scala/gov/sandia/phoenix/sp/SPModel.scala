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
import gov.sandia.phoenix.solarsystem.{Luna, Sol}
import gov.sandia.phoenix.time.JD

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class SPModel(val forces : Set[SPForceProvider] = Set.empty) {

  def ΣF(t : JD, state : ECIStateVector) = forces map { _.acceleration(t, state) } reduceLeft { (a, b) => a+b }

  def sigmaF (t : JD, state : ECIStateVector) =  ΣF (t, state)
  
  def +(force : SPForceProvider) = new SPModel(forces+force)
  
  override def toString = ("Forces:" /: forces){ (s, f) => s + "\n\t" + f }
}

/**
 * Sun, Moon, Earth Gravity Model. The following forces are present:
 * Sun: Low precision Sun location.
 * Moon: JPL405 Moon ephemeris.
 * Earth Gravity: EGM96 model.
 * 
 * This is a good model to start with. If SRP or Drag are desired they can be
 * added with their respective parameters for whatever vehicle is to be modeled.
 */
object SMGModel extends SPModel(Set(Sol, Luna, DefaultEGM96GravityForce))