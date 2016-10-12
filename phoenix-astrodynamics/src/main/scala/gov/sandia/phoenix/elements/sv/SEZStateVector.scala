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

/**
 *
 */
package gov.sandia.phoenix.elements.sv

import gov.sandia.phoenix.geometry.{AzElR, Vector3}

import scala.math._


/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 *
 */
case class SEZStateVector(sez : Vector3, dsez : Vector3) {
  def toECEF(ecef : Vector3) = {
    val frame = ecef.toGeodetic.toSEZFrame
    new ECEFStateVector(frame * sez, frame.rotation * dsez)
  }
  
  def toAzElR = {
    val pAzElR = sez.toAzElR
    val dR = sez * dsez / sez.mag
    val den = sez.x * sez.x + sez.y * sez.y
    val dAz = (dsez.x * sez.y - dsez.y * sez.x) / den
    val dEl = (dsez.z - dR * sin(pAzElR.elevation.toRadians)) / sqrt(den)

    new AzElRStateVector(pAzElR, new AzElR(dAz.toDegrees, dEl.toDegrees, dR))
  }

  def pretty = sez + ", " + dsez
}