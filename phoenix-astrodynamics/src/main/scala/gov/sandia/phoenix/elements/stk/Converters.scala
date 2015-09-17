package gov.sandia.phoenix.elements.stk

import gov.sandia.phoenix.elements.sv.CartesianStateVector

object Converters {
  def toSTK(o : CartesianStateVector) = { o.toArray map { STKDoubleFormat.format } mkString " " }
}
