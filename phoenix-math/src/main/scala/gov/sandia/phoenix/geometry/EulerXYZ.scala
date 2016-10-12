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

/**
 * Euler rotations represented by rotations in order of XYZ.
 *
 * http://mathworld.wolfram.com/EulerAngles.html
 * http://en.wikipedia.org/wiki/Euler_angles.
 *
 * One thing to consider is recasting the definition from Angles to Degrees. This prevents potential failures
 * on equality testing and euler angles are almost always specified in degrees.
 *
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class EulerXYZ(phi : Angle, theta : Angle, psi : Angle) {
  def toMatrix : RotationMatrix = toQuaternion.toMatrix

  def toQuaternion = phi.rx * theta.ry * psi.rz
}