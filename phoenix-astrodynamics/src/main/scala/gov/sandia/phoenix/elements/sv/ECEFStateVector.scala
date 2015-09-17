package gov.sandia.phoenix.elements.sv

import gov.sandia.phoenix.geometry.Vector3
import gov.sandia.phoenix.time._

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class ECEFStateVector(position : Vector3, velocity : Vector3)
  extends CartesianStateVector {
  def +(that : ECEFStateVector) = new ECEFStateVector(this.position + that.position, this.velocity + that.velocity)
  def -(that : ECEFStateVector) = new ECEFStateVector(this.position - that.position, this.velocity - that.velocity)
  def *(s : Double) = new ECEFStateVector(this.position * s, this.velocity * s)
  def /(s : Double) = this * (1.0 / s)

  def toECI(epoch : JD) = epoch.fk5.ITRFtoJ2000(this)
}