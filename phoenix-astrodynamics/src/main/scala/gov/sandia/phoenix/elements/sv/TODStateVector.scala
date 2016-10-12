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

import gov.sandia.phoenix.geometry.Vector3
import gov.sandia.phoenix.time.JD

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */

case class TODStateVector(position : Vector3, velocity : Vector3)
  extends CartesianStateVector {
  def +(that : TODStateVector) = new TODStateVector(this.position + that.position, this.velocity + that.velocity)
  def -(that : TODStateVector) = new TODStateVector(this.position - that.position, this.velocity - that.velocity)
  def *(s : Double) = new TODStateVector(this.position * s, this.velocity * s)
  def /(s : Double) = this * (1.0 / s)
  def toECI(t : JD) = t.fk5.TODtoJ2000(this)
}