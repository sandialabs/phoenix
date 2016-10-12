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

package gov.sandia.phoenix.propagators

import gov.sandia.phoenix.constants._
import gov.sandia.phoenix.time._
import gov.sandia.phoenix.elements.kepler.KeplerElements

class KeplerElementsPropagator(val elements : KeplerElements,
                               val epoch : JD) extends Propagator {
  def state(t : JD) = Some(elements.state((t.doubleValue - epoch.doubleValue) * Time.SEC_PER_DAY))
    
  def getPeriod = elements.period
  def getRAAN = elements.OMEGA
  def getArgP = elements.omega
  def getMeanAnomaly = elements.meanAnomaly.value
  def getSemimajorAxis = elements.a
}