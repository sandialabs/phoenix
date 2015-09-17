package gov.sandia.phoenix.geometry

/**
 * Euler rotations represented by rotations in order of ZYZ.
 *
 * http://mathworld.wolfram.com/EulerAngles.html
 * http://en.wikipedia.org/wiki/Euler_angles.
 *
 * One thing to consider is recasting the definition from Angles to Degrees. This prevents potential failures
 * on equality testing and euler angles are almost always specified in degrees.
 *
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class EulerZXZ(phi : Angle, theta : Angle, psi : Angle) {
  def toMatrix : RotationMatrix = toQuaternion.toMatrix
  
  def toQuaternion = phi.rz * theta.rx * psi.rz
}
