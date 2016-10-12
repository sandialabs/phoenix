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

package gov.sandia.phoenix.elements

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry._
import gov.sandia.phoenix.elements.kepler.KeplerElements


trait ElementFunctions {
  def state : ECIStateVector = this match {
    case eci : ECIStateVector => eci
    case _ => keplers.state
  }

  def state(dt : Double) : ECIStateVector

  def keplers : KeplerElements = this match {
    case keps : KeplerElements => keps
    case _ => state.keplers
  }
  
  def apogee : ECIStateVector = keplers.apogee
  def perigee : ECIStateVector = keplers.perigee
  def ascendingNode : ECIStateVector = keplers.ascendingNode
  def descendingNode : ECIStateVector = keplers.descendingNode
  def period : Double = keplers.period

  def nextApogeeDt = state timeOfFlight apogee
  def nextPerigeeDt = state timeOfFlight perigee
  def nextAscendingNodeDt = state timeOfFlight ascendingNode
  def nextDescendingNodeDt = state timeOfFlight descendingNode

  def apogeeDts(n : Int) = for(i <- 0 until n) yield nextApogeeDt + i * period
  def perigeeDts(n : Int) = for(i <- 0 until n) yield nextPerigeeDt + i * period
  def ascendingNodeDts(n : Int) = for(i <- 0 until n) yield nextAscendingNodeDt + i * period
  def descendingNodeDts(n : Int) = for(i <- 0 until n) yield nextDescendingNodeDt + i * period

  def apogeeDts(p : Int, n : Int) = for(i <- p until n) yield nextApogeeDt + i * period
  def perigeeDts(p : Int, n : Int) = for(i <- p until n) yield nextPerigeeDt + i * period
  def ascendingNodeDts(p : Int, n : Int) = for(i <- p until n) yield nextAscendingNodeDt + i * period
  def descendingNodeDts(p : Int, n : Int) = for(i <- p until n) yield nextDescendingNodeDt + i * period
}
