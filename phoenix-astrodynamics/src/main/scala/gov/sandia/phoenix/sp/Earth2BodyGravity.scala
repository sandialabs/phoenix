package gov.sandia.phoenix.sp

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.time.JD

object Earth2BodyGravity extends SPForceProvider {
  def acceleration(t : JD, state : ECIStateVector) = TwoBody.earthGravity(state.position)
  def getInstance = this
}
