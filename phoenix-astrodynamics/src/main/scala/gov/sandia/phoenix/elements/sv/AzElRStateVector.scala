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

package gov.sandia.phoenix.elements.sv

import gov.sandia.phoenix.geometry.{AzElR, Vector3}

import scala.math._


/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class AzElRStateVector(val azelr : AzElR, val dazelr : AzElR) {
  def toSEZ = {
    val el = toRadians(azelr.elevation)
    val B = toRadians(azelr.azimuth)
    val p = azelr.range
    val del = toRadians(dazelr.elevation)
    val dB = toRadians(dazelr.azimuth)
    val dp = dazelr.range
    val se = sin(el)
    val ce = cos(el)
    val sB = sin(B)
    val cB = cos(B)
    val dx = -dp * ce * cB + p * se * cB * del + p * ce * sB * dB
    val dy = dp * ce * sB - p * se * sB * del + p * ce * cB * dB
    val dz = dp * se + p * ce * del
    new SEZStateVector(azelr.toSEZ, Vector3(dx, dy, dz))
  }
  
  override def toString = azelr + "," + dazelr
}
