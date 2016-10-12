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

package gov.sandia.phoenix.geometry

import gov.sandia.phoenix.math._

import scala.math._

/**
 * A representation of Vector.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class Vector3(x : Double, y : Double, z : Double) {
  
  //Java compatibility layer
  def dot(v : Vector3) = this * v
  def cross(v : Vector3) = this % v
  def perp(v : Vector3) = ~this
  def plus(v : Vector3) = this + v
  def minus(v : Vector3) = this - v
  def scaled(s : Double) : Vector3 = { this * s }
  def length = mag
  def interpolate(that : Vector3, s : Double) = this * (1.0 - s) + that * s
  
  //Plane comparisons
  def > (p : Plane) = p.dist(this) > 0.0
  def < (p : Plane) = p.dist(this) < 0.0
  def >= (p : Plane) = p.dist(this) >= 0.0
  def <= (p : Plane) = p.dist(this) <= 0.0
  def == (p : Plane) = abs(p.dist(this)) < 1E-10
  
  //Magnitude of this Vector
  lazy val mag = sqrt(magSquared)
  lazy val magSquared = this * this
    
  //Vector dot product
  def * (v : Vector3) = x * v.x + y * v.y + z * v.z

  //Vector cross product
  def ⨯ (v : Vector3) = Vector3(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)
  def % (v : Vector3) = this ⨯ v

  //Normalization
  lazy val normalized = if(mag == 1.0) this else this / mag
    
  //Add-Subtract
  def + (v : Vector3) = Vector3(x + v.x, y + v.y, z + v.z)
  def - (v : Vector3) = Vector3(x - v.x, y - v.y, z - v.z)
    
  //Negate
  def unary_- = this * -1.0
  def negated = -this
    
  //Scale
  def * (s : Double) : Vector3 = Vector3(x * s, y * s, z * s)
    
  //Div-scale
  def / (s : Double) = this * (1.0 / s)
    
  //Perp
  def unary_~ : Vector3 = (if(abs(this.normalized.y) < 0.9999)
    Y_AXIS % this else X_AXIS % this).normalized
    
  def dist(v : Vector3) = distance(v)
  def distance(v : Vector3) = {
    val dx = x - v.x
    val dy = y - v.y
    val dz = z - v.z
    sqrt(dx*dx + dy*dy + dz*dz)
  }
    
  def dist(ls : LineSegment) = {
    val u = ls.start - this
    val v = ls.end - ls.start
    val den = v * v
    if(den == 0.0) u.mag
    else {
      val t = max(min(-(u * v) / den, 1.0), 0.0)
      ((ls.start + v * t) - this).mag
    }
  }

  def angle(that : Vector3) = Radians(cacos(this.normalized * that.normalized))

  def toSpherical = new SphericalCoordinate(mag, atan2(y, x), acos(z / mag))

  def toAzEl = {
    val r = mag
    val el = asin(z / r).toDegrees
    val h = hypot(x, y)
    val den = if(h != 0.0) 1.0 / h else 0.0
    val sinAz = y * den
    val cosAz = -x * den
    val az = atan2(sinAz, cosAz).toDegrees
    AzEl(if(az < 0.0) az + 360.0 else az, el)
  }

  def toAzElR = {
    val r = mag
    val el = asin(z / r).toDegrees
    val h = sqrt(x * x + y * y)
    val den = if(h != 0.0) 1.0 / h else 0.0
    val sinAz = y * den
    val cosAz = -x * den
    val az = atan2(sinAz, cosAz).toDegrees
    new AzElR(if(az < 0.0) az + 360.0 else az, el, r)
  }

  def pretty = "<" + x + ", " + y + ", " + z + ">"
    
  def close(v : Vector3) = abs(x - v.x) <  FLT_EPSILON && abs(y - v.y) <  FLT_EPSILON && abs(z - v.z) < FLT_EPSILON
  
  def sweep(theta : Double, steps : Int) = {
    val rot = AxisAngle(this, Radians(2.0 * Pi / steps)).toQuaternion
    val sweepers = new Array[Vector3](steps)
    sweepers(0) = AxisAngle(~this, Radians(theta)).toQuaternion * this
    for(i <- 1 until steps) sweepers(i) = rot * sweepers(i-1)
    sweepers
  }

  def toArray = Array(x, y, z)
  
  def apply(i : Int) = i match {
    case 0 => x
    case 1 => y
    case 2 => z
  }

  def rotInto(that : Vector3) = {
    val u = this.normalized
    val v = that.normalized
    val c = u * v
    val axis = u % v
    val s = axis.mag
    if (s < 1E-10) IDENTITY_QUATERNION else AxisAngle(axis, Angle.atan2(s, c)).toQuaternion
  }

  //Spherical Linear Interpolation
  def slerp(that : Vector3, t : Double) = {
    val uhat = this.normalized
    val vhat = that.normalized
    val theta = acos(max(-1.0, min(1.0, uhat * vhat)))
    val sinTheta = sin(theta)
    if(abs(sinTheta) <= 1E-10) uhat.lerp(vhat,t) else {
      val sint = sin(t * theta)
      val sin1t = sin((1.0 - t) * theta)
      uhat * (sin1t / sinTheta) + vhat * (sint / sinTheta)
    }
  }

  //Linear Interpolation
  def lerp(that : Vector3, t : Double) = this * (1-t) + that * t

  def toGeodetic = ECEFtoGEO(this)
  def toRaDec = ECItoRaDec(this)
  def toRRaDec = ECItoRRadec(this)
}

