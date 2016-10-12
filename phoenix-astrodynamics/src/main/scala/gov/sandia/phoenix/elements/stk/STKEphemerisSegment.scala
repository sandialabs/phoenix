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

package gov.sandia.phoenix.elements.stk

import gov.sandia.phoenix.elements.sv.ECIStateVector

import collection.immutable.SortedMap

class STKEphemerisSegment(val start : Double, val end : Double,
                          val states : SortedMap[Double, ECIStateVector] = SortedMap.empty) {
  def +(entry : (Double, ECIStateVector)) = new STKEphemerisSegment(start, end, states+entry)
  override def toString = "STKEphemerisSegment start = " + start + ", end = " + end + ", state count = " + states.size + "."
}