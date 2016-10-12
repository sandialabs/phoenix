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

import scala.math._



/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class SphericalTriangle(val v0 : Vector3,
                        val v1 : Vector3,
                        val v2 : Vector3) {

  private final val vertices = Array(v0, v1, v2)
  lazy val radius = v0.mag
  lazy val area = SphericalTriangle.area(vertices)
  lazy val center = SphericalTriangle.center(vertices)
  def getVertex(index : Int) = vertices(index)
}

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object SphericalTriangle {
  def center(vertices : Array[Vector3]) = ((ORIGIN : Vector3) /: vertices)(_+_) * (vertices(0).mag / 3.0)

  /**
   * Compute the spherical triangular area given 3 vertices on sphere.
   * @param vertices 3 vertices on the sphere.
   * @return Area of the small spherical triangle on the sphere.
   */
  def area(vertices : Array[Vector3]) = {
    val cosa = vertices(0) * vertices(1)
    val cosb = vertices(1) * vertices(2)
    val cosc = vertices(2) * vertices(0)
    val sina = sin(acos(cosa))
    val sinb = sin(acos(cosb))
    val sinc = sin(acos(cosc))
    val A = acos((cosa - cosb * cosc) / (sinb * sinc))
    val B = acos((cosb - cosa * cosc) / (sina * sinc))
    val C = acos((cosc - cosb * cosa) / (sinb * sina))

    A + B + C - Pi
  }
}
