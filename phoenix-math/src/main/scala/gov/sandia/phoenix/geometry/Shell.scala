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
import scala.language.postfixOps

trait Shell extends LineSegmentIntersectable {
  def volume : Double
  def area : Double
  
  /**
   * A shell is hollow, so only penetrations (line segment parameters [0, 1]) count.
   */
  abstract override def intersect(ls : LineSegment) = super.intersect(ls) match {
    case Array(i, f) => Array(if (i >= 0.0 && i <= 1.0) Some(i) else None,
      if (f >= 0.0 && f <= 1.0) Some(f) else None).flatten
    case x => x 
  }  
  
}