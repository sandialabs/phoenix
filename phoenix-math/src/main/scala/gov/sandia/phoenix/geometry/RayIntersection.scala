/*
 * Copyright (c) 2016 Sandia Corporation. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by the
 * terms of this license.
 * You must not remove this notice, or any other, from this software.
 *
 * Contributors:
 * - Mark Bastian: Original author.
 * - See Git logs.
 */

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
