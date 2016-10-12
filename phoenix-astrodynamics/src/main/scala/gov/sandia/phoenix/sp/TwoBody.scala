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

package gov.sandia.phoenix.sp

import gov.sandia.phoenix.constants._
import gov.sandia.phoenix.geometry._

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object TwoBody {
  def acceleration(r : Vector3, s : Vector3, GM : Double) = {
    val rm = r.mag
    val sm = s.mag
    val smr = s - r
    val smrm = smr.mag
    val t2 = if(sm != 0.0) s / (sm * sm * sm) else s
    (smr / (smrm * smrm * smrm) - t2) * GM
  }
  
  def earthGravity(r : Vector3) = acceleration(r, new Vector3(0, 0, 0), WGS84.GM)
}
