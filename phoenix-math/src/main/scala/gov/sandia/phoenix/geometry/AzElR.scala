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
 * Class for storing azimuth, elevation, and range values.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class AzElR(azimuth : Double, elevation : Double, range : Double) {
  require (elevation >= -90 && elevation <= 90)

  def toSEZ = Degrees(-azimuth).rz * Degrees(elevation).ry * Vector3(-range, 0, 0)

  def csv = azimuth + ", " + elevation + ", " + range
}