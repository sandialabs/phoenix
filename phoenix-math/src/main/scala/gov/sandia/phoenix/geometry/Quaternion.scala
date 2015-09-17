package gov.sandia.phoenix.geometry

import scala.collection.immutable.{Vector => ImmutableArray}
import scala.math._
import gov.sandia.phoenix.math._

/**
 * A Quaternion class. This is a very efficient way to handle rotations. It also
 * doesn't suffer from some of the issues (such as drift) that rotation matrices
 * suffer from.
 * <p>
 * @author [[mailto: markbastian@gmail.com Mark Bastian]]
 */
case class Quaternion(s: Double, v: Vector3) extends Rotation {
  //Java compatibility
  def mul(q : Quaternion) = this * q
  def mul(p : Vector3) = this * p
  def inverse = new Quaternion(s, v.negated)

  def * (q : Quaternion) = new Quaternion(s*q.s - v*q.v, q.v*s + v*q.s + v % q.v)
  def * (p : Vector3) : Vector3 = p * (s * s - v * v) + (v % p * 2 * s) + v * (v * p * 2)

  def mag = sqrt(s * s + v.magSquared)

  def unary_! = new Quaternion(s, -v)

  def x = v.x
  def y = v.y
  def z = v.z
  def w = s

  private def xx = 2.0 * v.x * v.x
  private def yy = 2.0 * v.y * v.y
  private def zz = 2.0 * v.z * v.z

  private def xy = 2.0 * v.x * v.y
  private def xz = 2.0 * v.x * v.z
  private def yz = 2.0 * v.y * v.z

  private def wx = 2.0 * s * v.x
  private def wy = 2.0 * s * v.y
  private def wz = 2.0 * s * v.z

  def M00 : Double = 1.0 - yy - zz
  def M01 : Double = xy - wz
  def M02 : Double = xz + wy
  def M10 : Double = xy + wz
  def M11 : Double = 1.0 - xx - zz
  def M12 : Double = yz - wx
  def M20 : Double = xz - wy
  def M21 : Double = yz + wx
  def M22 : Double = 1.0 - xx - yy

  def toMatrix = new RotationMatrix(ImmutableArray(M00, M01, M02, M10, M11, M12, M20, M21, M22))

  def toAxisAngle = AxisAngle(v.normalized, Angle.atan2(v.mag, s) * 2.0)

  def slerp(that : Quaternion, t : Double) = {
    val d = this.s * that.s + this.v * that.v
    val theta = acos(d)
    val invSinTheta = 1.0 / sqrt(1.0 - d * d)
    val a = sin(theta * (1.0 - t)) * invSinTheta
    val b = sin(theta * t) * invSinTheta
    new Quaternion(a * this.s + b * that.s, this.v * a + that.v * b)
  }

  def close (that : Quaternion) = {
    //To be equal, this * q will give an identity Quaternion.  The first element
    //must be 1, so we can discard the vector portion of the Quaternion multiply
    //since we are only dealing with unit IQuaternions.  In other words, if
    //s == 1, v = <0, 0, 0>.
    abs(1 - abs(this.s*that.s + this.v*that.v)) < FLT_EPSILON
  }

  def pretty = "<" + s + ", " + v + ">"
}

object IDENTITY_QUATERNION extends Quaternion(1, ORIGIN) {
  def getInstance = this
}