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