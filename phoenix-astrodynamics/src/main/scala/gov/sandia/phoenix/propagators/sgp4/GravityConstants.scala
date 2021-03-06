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

package gov.sandia.phoenix.propagators.sgp4

import scala.math._

sealed abstract class GravityConstants(val radiusearthkm : Double, //km
    val mu : Double, //km3 / s2
    val xke : Double,
    val j2 : Double, //j2 = k2 / a1 / a1 * 2?
    val j3 : Double,
    val j4 : Double)
{
  def this(radiusearthkm : Double, mu : Double, j2 : Double, j3 : Double, j4 : Double) =
    this(radiusearthkm, mu, 60.0 / sqrt(radiusearthkm * radiusearthkm * radiusearthkm / mu), j2, j3, j4)

  val tumin = 1.0 / xke
  val j3oj2 = j3 / j2

  def getInstance = this
}

case object WGS72 extends GravityConstants(6378.135, 398600.8, 0.001082616, -0.00000253881, -0.00000165597)
case object WGS72OLD extends GravityConstants(6378.135, 398600.79964, 0.0743669161, 0.001082616, -0.00000253881, -0.00000165597)
case object WGS84 extends GravityConstants(6378.137, 398600.5, 0.00108262998905, -0.00000253215306, -0.00000161098761)