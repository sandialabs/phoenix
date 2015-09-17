package gov.sandia.phoenix.geometry


/**
 * A representation of a Sphere.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class Sphere(center : Vector3, radius : Double) extends SphereLike with Shell {
  def this() = this(ORIGIN, 1.0)
  def this(r : Double) = this(ORIGIN, r)
    
  def intersect(cone : InfiniteCone) = cone.intersect(this)
  override def toString = "r = " + radius + ", c = " + center
}

object UNIT_SPHERE extends Sphere(ORIGIN, 1.0) {
  def getInstance = this
}
