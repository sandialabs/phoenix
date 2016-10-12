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

import scala.collection.immutable.{Vector => Vec}
import scala.math._

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object Icosahedron {
  val t = (1.0 + sqrt(5.0)) * 0.5
  val den = 1.0 / sqrt(1.0 + t * t)

  val vertices = Vec(new Vector3(t, 1.0, 0.0) * den,
                     new Vector3(-t, 1.0, 0.0) * den,
                     new Vector3(t, -1.0, 0.0) * den,
                     new Vector3(-t, -1.0, 0.0) * den,
                     new Vector3(1.0, 0.0, t) * den,
                     new Vector3(1.0, 0.0, -t) * den,
                     new Vector3(-1.0, 0.0, t) * den,
                     new Vector3(-1.0, 0.0, -t) * den,
                     new Vector3(0.0, t, 1.0) * den,
                     new Vector3(0.0, -t, 1.0) * den,
                     new Vector3(0.0, t, -1.0) * den,
                     new Vector3(0.0, -t, -1.0) * den)

  val connectivity = Vec(
    Vec(0,8,4),Vec(0,5,10),Vec(2,4,9),Vec(2,11,5),Vec(1,6,8),
    Vec(1,10,7),Vec(3,9,6),Vec(3,7,11),Vec(0,10,8),Vec(1,8,10),
    Vec(2,9,11),Vec(3,11,9),Vec(4,2,0),Vec(5,0,2),Vec(6,1,3),
    Vec(7,3,1),Vec(8,6,4),Vec(9,4,6),Vec(10,5,7),Vec(11,7,5)
  )
}
