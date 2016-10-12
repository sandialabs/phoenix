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

import gov.sandia.phoenix.time.{JD, TimeSequence}

import scala.collection.SortedMap

object EmptyPropagationSequence extends PropagationSequence(SortedMap.empty){
  def getInstance : PropagationSequence = this
}

class PropagationSequence(epochs : SortedMap[JD, Propagator] = SortedMap.empty) extends TimeSequence(epochs) with Propagator {
  type TT = PropagationSequence
  def state(t : JD) = this(t) flatMap { _.state(t) }

  def build(epochs : SortedMap[JD, Propagator]) = new PropagationSequence(epochs)
}