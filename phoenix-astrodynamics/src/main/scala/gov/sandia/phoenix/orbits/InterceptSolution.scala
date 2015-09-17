package gov.sandia.phoenix.orbits

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.Vector3

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class InterceptSolution(val interceptorInitialState : ECIStateVector,
                        val targetInitialState : ECIStateVector,
                        val dt : Double,
                        val initialDeltaV : Vector3,
                        val initialTransferState : ECIStateVector,
                        val interceptorFinalState : ECIStateVector,
                        val targetFinalState : ECIStateVector) {
  val hitsEarth = initialTransferState.hitEarth(dt)
  val deltaV = initialDeltaV.mag
}