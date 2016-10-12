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