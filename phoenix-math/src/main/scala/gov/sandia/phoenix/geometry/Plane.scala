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
 * Representation of a plane. Any plane can be represented as a normal vector
 * pointing away from the global origin and a distance along that vector from
 * the origin.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class Plane(d : Double, direction : Vector3) extends IPlane {
  val n = direction.normalized
  def this(o : Vector3, n : Vector3) = this(o * n.normalized, n.normalized)

  def + (d : Double) = new Plane(this.d + d, n)
  def - (d : Double) = new Plane(this.d - d, n)
  def flip = new Plane(-d, -n)
}

object X_PLANE extends Plane(0, X_AXIS){
  def getInstance = this
}
object Y_PLANE extends Plane(0, Y_AXIS){
  def getInstance = this
}
object Z_PLANE extends Plane(0, Z_AXIS){
  def getInstance = this
}
object NEGATIVE_X_PLANE extends Plane(0, -X_AXIS){
  def getInstance = this
}
object NEGATIVE_Y_PLANE extends Plane(0, -Y_AXIS){
  def getInstance = this
}
object NEGATIVE_Z_PLANE extends Plane(0, -Z_AXIS){
  def getInstance = this
}
