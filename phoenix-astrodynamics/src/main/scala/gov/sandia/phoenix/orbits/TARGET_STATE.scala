package gov.sandia.phoenix.orbits

import gov.sandia.phoenix.elements.sv.ECIStateVector

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
sealed abstract class TARGET_STATE(val state : ECIStateVector)
case class INITIAL_STATE(initialState : ECIStateVector) extends TARGET_STATE(initialState)
case class FINAL_STATE(finalState : ECIStateVector) extends TARGET_STATE(finalState)

