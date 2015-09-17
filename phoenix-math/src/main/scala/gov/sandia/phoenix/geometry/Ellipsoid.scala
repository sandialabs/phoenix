package gov.sandia.phoenix.geometry

/**
 * An ellipsoid class. See http://www.cl.cam.ac.uk/teaching/1999/AGraphHCI/SMAG/node2.html for more.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class Ellipsoid(center : Vector3, axes : Vector3) extends EllipsoidLike with Solid {
  override def area = throw new Exception("Unsupported Operation")
}