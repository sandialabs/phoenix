package gov.sandia.phoenix.elements.sv

import gov.sandia.phoenix.geometry.{Angle, Vector3}

import scala.math._

trait CartesianStateVector {
  def position : Vector3
  def velocity : Vector3
  
  def toArray = Array(position.x, position.y, position.z, velocity.x, velocity.y, velocity.z)
  
  override def toString = position + ", " + velocity

  def inclination = {
    val W = (position ⨯ velocity).normalized
    val inclination = Angle.atan2(sqrt(W.x * W.x + W.y * W.y), W.z)
    if(inclination.radians < 0) inclination + Angle.TwoPi else inclination
  }

  def toCSV = toArray.mkString(",")

  /**
   * Create a hermite splined point on the interval [0, 1] between this state vector and that one using the velocities as derivatives.
   */
  def hermitianPoint(t : Double, that : CartesianStateVector) = {
    val t1 = t
    val t2 = t1 * t
    val t3 = t2 * t
    this.position * (2*t3 - 3*t2 + 1) + this.velocity * (t3 -2*t2 + t1) + that.position * (-2*t3 + 3*t2) + that.velocity * (t3 - t2)
  }
}
