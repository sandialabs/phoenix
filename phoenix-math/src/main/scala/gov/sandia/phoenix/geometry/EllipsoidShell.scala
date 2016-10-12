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
 * An ellipsoid class. See http://www.cl.cam.ac.uk/teaching/1999/AGraphHCI/SMAG/node2.html for more.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class EllipsoidShell(center : Vector3, axes : Vector3) extends EllipsoidLike with Shell {
  override def area = throw new Exception("Unsupported Operation")
}