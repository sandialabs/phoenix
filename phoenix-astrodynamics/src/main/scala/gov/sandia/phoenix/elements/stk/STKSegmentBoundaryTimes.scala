package gov.sandia.phoenix.elements.stk

import collection.immutable.SortedSet

object STKSegmentBoundaryTimes {
  def unapply(l : List[String]) = l match {
    case BEGINSegmentBoundaryTimes() :: tail => Some(extractBoundaries(tail))
    case _ => None
  }

  private def extractBoundaries(l : List[String], boundaries : SortedSet[Double] = SortedSet.empty) :
  (List[String], SortedSet[Double]) = l match {
    case ENDSegmentBoundaryTimes() :: tail => (tail, boundaries)
    case SegmentBoundaryExtractor(b) :: tail => extractBoundaries(tail, boundaries+b)
    case _ => extractBoundaries(l.tail, boundaries)
  }
}