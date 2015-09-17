package gov.sandia.phoenix.geometry

/**
 * A trait that can be used for intersection of a ray with an object. This is a
 * common geometric function.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
trait RayIntersection {
  def intersect(ray : Ray) : Array[Double]
  def intersects(r : Ray) = intersect(r).length != 0

    /**
     * Return the closest positive intersection point.  Intersections made by
     * projecting the ray in reverse are not considered.
     * @param ray ray to intersect with.
     * @return Closest forward intersection point or null if no intersection.
     */
    def closestIntersection(ray : Ray) = {
        val intersections = intersect(ray)
        if(intersections.size > 0)
        {
          if(intersections(0) >= 0.0)
            ray.parametricPoint(intersections(0))
          else if(intersections(1) >= 0.0)
            ray.parametricPoint(intersections(1))
          else null
        }
        else
          null;
    }
}
