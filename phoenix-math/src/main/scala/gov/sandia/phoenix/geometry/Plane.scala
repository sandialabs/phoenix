package gov.sandia.phoenix.geometry

/**
 * Representation of a plane. Any plane can be represented as a normal vector
 * pointing away from the global origin and a distance along that vector from
 * the origin.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class Plane(d : Double, direction : Vector3) extends IPlane {
  val n = direction.normalized
  def this(o : Vector3, n : Vector3) = this(o * n.normalized, n.normalized)

  def + (d : Double) = new Plane(this.d + d, n)
  def - (d : Double) = new Plane(this.d - d, n)
  def flip = new Plane(-d, -n)
}

object X_PLANE extends Plane(0, X_AXIS){
  def getInstance = this
}
object Y_PLANE extends Plane(0, Y_AXIS){
  def getInstance = this
}
object Z_PLANE extends Plane(0, Z_AXIS){
  def getInstance = this
}
object NEGATIVE_X_PLANE extends Plane(0, -X_AXIS){
  def getInstance = this
}
object NEGATIVE_Y_PLANE extends Plane(0, -Y_AXIS){
  def getInstance = this
}
object NEGATIVE_Z_PLANE extends Plane(0, -Z_AXIS){
  def getInstance = this
}
