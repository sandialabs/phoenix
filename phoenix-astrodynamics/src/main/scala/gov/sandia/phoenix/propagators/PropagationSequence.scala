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