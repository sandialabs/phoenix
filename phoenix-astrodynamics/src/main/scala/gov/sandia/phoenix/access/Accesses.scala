package gov.sandia.phoenix.access

import gov.sandia.phoenix.time.{JD, Interval}
import scala.collection.SortedSet

/**
 * Provides utility methods to create a range of time.
 * @todo Move this into a phoenix that makes more sense, maybe gov.sandia.phoenix.time
 */
object Accesses {
  def sample(interval : Interval, dt : Double)(f : JD => Boolean) = interval.stepBy(dt)(f)

  def continuous(interval : Interval, dt : Double, tol : Double)(f : JD => Boolean) =
    samplesToContinuous(sample(interval, dt)(f), tol)(f)

  def samplesToContinuous(samples : Traversable[(JD, Boolean)], tol : Double)(f : JD => Boolean) = {
    def reduce(l : List[(JD, Boolean)], os : Option[JD], res : SortedSet[Interval]) : SortedSet[Interval] = l match {
      case Nil => res
      case (t, true) :: Nil => os match {
        case Some(start) => res + (start until t) //End condition when in view
        case _ => res //This should only happen if there's a single entry. One point doesn't make an access.
      }
      case (pi @ (ti, true)) :: (pf @ (tf, false)) :: tail =>
        val dt = (ti until tf).getDurationSeconds.doubleValue()
        os match { //end
        case Some(start) => reduce(l.tail, None, res + (start until bisect(pi, pf, dt, tol, f)))
        case None => reduce(l.tail, None, res + (ti until bisect(pi, pf, dt, tol, f)))
      }
      case (pi @ (ti, false)) :: (pf @ (tf, true)) :: tail =>
        val dt = (ti until tf).getDurationSeconds.doubleValue()
        reduce(l.tail, Some(bisect(pi, pf, dt, tol, f)), res) //start
      case (t, true) :: tail => reduce(tail, Some(os.getOrElse(t)), res)
      case (t, false) :: tail => reduce(tail, None, res) //Just keep going
    }

    reduce(samples.toList, None, SortedSet.empty[Interval])
  }
}