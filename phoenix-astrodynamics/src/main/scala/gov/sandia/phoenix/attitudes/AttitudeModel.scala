package gov.sandia.phoenix.attitudes

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.{IDENTITY_QUATERNION, Quaternion}

trait AttitudeModel {
  def attitude(state : ECIStateVector) : Option[Quaternion]
  def apply(state : ECIStateVector) = attitude(state).getOrElse(IDENTITY_QUATERNION)
}
