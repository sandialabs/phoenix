package gov.sandia.phoenix.geometry

case class AxisAngle(axis : Vector3, theta : Angle) {
  def toQuaternion = new Quaternion(theta.half.cos, axis.normalized * theta.half.sin)

  def * (p : Vector3) = toQuaternion * p
}
