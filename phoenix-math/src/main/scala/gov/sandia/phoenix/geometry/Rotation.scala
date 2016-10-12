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
 * A good way to get an understanding of the toXXX transformations in which XXX
 * is some sort of angle arrangement is to take a look at a symbolic expansion
 * of the said transformation.  One place to do this is http://planetphysics.org
 * (specifically http://planetphysics.org/encyclopedia/D/).  It should be
 * apparent how to extract the angles using asin/acos for one element and atan2
 * for the others.  One critical thing to note is how matrices are stored and 
 * multiplied.  In this code, matrices are multipled right to left.  At
 * planet physics they are multplied left to right (front multiplied).  This
 * results in their solutions being the transpose of mine.  So, if you use their
 * results, be sure to use the transposed elements.  Also, if you use another
 * web site or book to figure this out, you'll need to consider the same issues.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
trait Rotation {
  //Transforms vectors
  def * (p : Vector3) : Vector3
    
  //Invertible
  def unary_! : Rotation
    
  def M00 : Double
  def M01 : Double
  def M02 : Double
  def M10 : Double
  def M11 : Double
  def M12 : Double
  def M20 : Double
  def M21 : Double
  def M22 : Double
    
  /**
   * http://planning.cs.uiuc.edu/node103.html
   * http://planetphysics.org/encyclopedia/Euler123Sequence.html - transpose
   */
  def toYPR = {
    val yaw = Angle.atan2(-M01, M00)
    //Take care of odd cases in which you are just outside of the range of 
    //asin due to numerical error.
    val m = if(M02 < -1 && abs(M02 + 1) < FLT_EPSILON) -1 else
      if(M02 > 1 && abs(M02 - 1) < FLT_EPSILON) 1
    else M02

    val pitch = Angle.asin(m)
    val roll = Angle.atan2(-M12, M22)
    YPR(yaw, pitch, roll)
  }
    
  /**
   * http://planning.cs.uiuc.edu/node103.html
   * http://planetphysics.org/encyclopedia/Euler313Sequence.html - transpose
   */
  def toEulerZXZ = {
    ////Take care of odd cases in which you are just outside of the range of 
    //acos due to numerical error.
    val m = if(M22 < -1 && abs(M22 + 1) < FLT_EPSILON) -1 else
    if(M22 > 1 && abs(M22 - 1) < FLT_EPSILON) 1 else M22

    val theta = Angle.acos(m)
    val psi = Angle.atan2(M20, M21)
    val phi = Angle.atan2(M02, -M12)
    EulerZXZ(phi, theta, psi)
  }

  def toEulerXYZ = {
    ////Take care of odd cases in which you are just outside of the range of
    //asin due to numerical error.
    val m = if(M02 < -1 && abs(M02 + 1) < FLT_EPSILON) -1 else
    if(M02 > 1 && abs(M02 - 1) < FLT_EPSILON) 1 else M02

    val theta = Angle.atan2(-M12, M22)
    val psi = Angle.asin(m)
    val phi = Angle.atan2(-M01, M00)
    EulerXYZ(theta, psi, phi)
  }
    
  def toAzEl = {
    val azimuth = constrain0to360(-toDegrees(atan2(-M01, M11)))
    //Take care of odd cases in which you are just outside of the range of 
    //acos due to numerical error.
    //TODO: Can I just use cacos here?
    val m = M22 match {
      case n if n < -1 && abs(n + 1) < FLT_EPSILON => -1
      case p if p > 1 && abs(p - 1) < FLT_EPSILON => 1
      case _ => M22
    }
    val elevation = 90 - toDegrees(acos(m))
    AzEl(azimuth, elevation)
  }

  def directionCosines(v : Vector3) = {
    val vhat = v.normalized
    Array(vhat * (this * X_AXIS), vhat * (this * Y_AXIS), vhat * (this * Z_AXIS))
  }
}
