package gov.sandia.phoenix.geometry

/**
 * A rotation defined by a yaw, pitch, and roll. This is equivalent to EulerXYZ/123
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class YPR(yaw : Angle, pitch : Angle, roll : Angle) {
  def toMatrix : RotationMatrix = toQuaternion.toMatrix
  
  def toQuaternion = roll.rx * pitch.ry * yaw.rz
}
