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
 * Circle class.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class Circle(center : Vector3, xAxis :  Vector3, yAxis : Vector3) extends IInterpolable {
  def radius = xAxis.mag

  @deprecated("Use sample instead", "6.4")
  def asPoints(numPoints : Int) = scala.collection.immutable.Vector.tabulate[Vector3](numPoints) { i =>
    val theta = Pi * 2.0 * i / (numPoints - 1)
    center + xAxis * cos(theta) + yAxis * sin(theta)
  }

  def interpolate(x : Double) = center + xAxis * cos(x * Pi * 2.0) + yAxis * sin(x * Pi * 2.0)

  def plane = new Plane(center, xAxis ⨯ yAxis)
}
