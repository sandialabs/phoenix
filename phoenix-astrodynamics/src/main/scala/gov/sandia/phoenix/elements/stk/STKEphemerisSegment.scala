package gov.sandia.phoenix.elements.stk

import gov.sandia.phoenix.elements.sv.ECIStateVector

import collection.immutable.SortedMap

class STKEphemerisSegment(val start : Double, val end : Double,
                          val states : SortedMap[Double, ECIStateVector] = SortedMap.empty) {
  def +(entry : (Double, ECIStateVector)) = new STKEphemerisSegment(start, end, states+entry)
  override def toString = "STKEphemerisSegment start = " + start + ", end = " + end + ", state count = " + states.size + "."
}