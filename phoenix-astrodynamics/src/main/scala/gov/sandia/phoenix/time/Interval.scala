package gov.sandia.phoenix.time

import gov.sandia.phoenix.constants._
import java.text.DecimalFormat
import scala.collection.immutable.SortedMap
import scala.collection.SortedSet
import JD._

/**
 * An interval is a span of time defined over [start, end).  It is critical to
 * realize that the end point is not contained in the interval for the purposes
 * of time set operations.  Many very handy functions for computing over 
 * intervals are also included.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class Interval(start : JD, end : JD) extends Ordered[Interval] {
  require(start <= end)

  def remainingHMS = {
    val dt = getDurationSeconds.doubleValue
    val hrs = (dt / 3600).toInt
    val hrem = dt - hrs * 3600
    val min = (hrem / 60).toInt
    val mrem = hrem - min * 60
    val sec = mrem.toInt
    val df2 = new DecimalFormat("00")
    hrs + "h " + df2.format(min) + "m " + df2.format(sec) + "s"
  }
  
  /**
   * Get the length of the interval in days.
   * Warning: This function loses precision and should be fixed for sub ms accuracy.
   * @return duration of the interval in days.
   */
  def getDurationDays = end.value - start.value
  def getDurationSeconds = getDurationDays * Time.SEC_PER_DAY

  def slide(dt : Double) = (start plusSeconds dt) until (end plusSeconds dt)
  def shiftUp = end until (end plusDays getDurationDays.doubleValue)
  def shiftDown = (start minusDays getDurationDays.doubleValue) until start

  /**
   * Iterate over numInterval evenly spaced intervals, with the end time
   * included.  This means that for N intervals, there will be N+1 items
   * iterated over.
   * @param numIntervals number of evenly spaced intervals to iterate over.
   * @return the iterator.
   */
  def getIntervalIterator(numIntervals : Int) : java.lang.Iterable[JD] = getIntervalIterator(numIntervals, false)

  /**
   * Iterate over numInterval evenly spaced intervals.  An open ended interval
   * (having numIntervals points) would not include the final time and a
   * closed ended interval (having numIntervals + 1 points) would.
   * @param numIntervals number of evenly spaced intervals to iterate over.
   * @param openEnded true excludes the end instant.  false includes it.
   * @return the iterator.
   */
  def getIntervalIterator(numIntervals : Int, openEnded : Boolean) : java.lang.Iterable[JD] = new java.lang.Iterable[JD]{
    var currentStep = 0
    val step = getDurationDays / numIntervals
    def iterator = new java.util.Iterator[JD] {
      def hasNext = if(openEnded) currentStep < numIntervals else currentStep <= numIntervals
      def next = {
        val n = JD(start.value + step * currentStep)
        currentStep = currentStep + 1
        n
      }
      def remove = {}
    }
  }

  /**
   * Iterate over this interval, starting at start, ending at end, and taking
   * time steps of duration (units are days).  Start and end are always
   * included.
   * @param duration step size in days.
   * @return the interator.
   */
  def getDurationIterator(duration : Double) : java.lang.Iterable[JD] = new java.lang.Iterable[JD]{
    var currentStep = 0
    var done = false
    def iterator = new java.util.Iterator[JD] {
      def hasNext = !done
      def next = {
        val t = start plusDays (duration * currentStep)
        currentStep = currentStep + 1
        if(t > end) {
          done = true
          end
        } else t
      }
      def remove = {}
    }
  }

  /**
   * Iterate from start to finish (inclusive) in one second steps.  Start and
   * end are always included, regardless of interval length.
   * @return the iterator.
   */
  def getSecondIterator : java.lang.Iterable[JD] = new java.lang.Iterable[JD]{
    var currentStep = 0
    var done = false
    def iterator = new java.util.Iterator[JD] {
      def hasNext = !done
      def next = {
        val t = start plusSeconds currentStep
        currentStep = currentStep + 1
        if(t > end) {
          done = true
          end
        } else t
      }
      def remove = {}
    }
  }
  
  /**
   * Compute parametric instant on this interval.
   * @param t Parametric location on the interval [0, 1].  If t is outside of
   * this range it will be considered periodic and modded with 1.
   * @return JulianDate mapped to t.
   */
  def interpolate(t : Double) = start.plusDays(getDurationDays.doubleValue * (t % 1.0))

  /**
   * Compute the unbounded parametric instant using this interval
   */
  def extrapolate(t : Double) = start.plusDays(getDurationDays.doubleValue * t)

  def parameterize(t : JD) = (t.value - start.value) / getDurationDays

  /**
   * Do these intervals abut each other?
   * @param interval interval to check against.
   * @return true if abuts, false if not.
   */
  def abuts(that : Interval) = that.end == this.start || that.start == this.end

  /**
   * Determine if an instant is completely within this interval.  If the
   * instant is at the end point, the instant is not in the interval.
   * Remember, this interval is defined on [start, end).
   * @param instant instant to compare with.
   * @return true if completely contained, false if not.
   */
  def contains(that : Interval) = if(getDurationDays == 0) false else {
    if(that.start < this.start) false
    else if(that.end > this.end || this.end == that.start) false
    else true
  }
  
  /**
   * Determine if an instant is completely within this interval.  If the
   * instant is at the end point, the instant is not in the interval.
   * Remember, this interval is defined on [start, end).
   * @param instant instant to compare with.
   * @return true if completely contained, false if not.
   */
  def contains(time : JD) = if(time < start) false else if(time < end) true else false
    
  /**
   * Determine if an interval overlaps this interval.
   * @param interval Interval to compare with.
   * @return true if overlap, false if not.
   */
  def overlaps(that : Interval) = !((this abuts that) || (this.start > that.end) || (this.end < that.start))

  def touches(that : Interval) = (this overlaps that) || (this abuts that)

  /**
   * Compute the overlapping area between these intervals.
   * @param interval Interval to compute with.
   * @return Some overlapping interval, or None if no overlap.
   */
  def overlap(that : Interval) = if(!overlaps(that)) None else
  if(this contains that) Some(that) else
  if(that contains this) Some(this) else
    Some(Interval(this.start max that.start, this.end min that.end))

  def join(that : Interval) = if(this touches that) Some(this.start.min(that.start) until this.end.max(that.end)) else None

  /**
   * Are these intervals equal?  Equality is defined as having the same begin
   * and end instants.
   * @param interval
   * @return
   */
  def isEqual(that : Interval) = (this.start isEqual that.start) && (this.end isEqual that.end)

  /**
   * Is this interval completely before this instant?
   * @param instant
   * @return true if before, false if not.
   */
  def isBefore(instant : JD) = this.end < instant
  
  /**
   * Is this interval completely after this instant?
   * @param instant
   * @return true if after, false if not.
   */
  def isAfter(instant : JD) = this.start > instant

  /**
   * Compare two intervals.  The logic is as follows:
   * An interval is less than another if its start time is earlier.  If the
   * start times are equal, the shorter interval is the lesser one.
   */
  override def compare(that : Interval) = if(this.start < that.start) -1 else {
    if(this.start > that.start) 1 else {
      if(this.end < that.end) -1 else {
        if(this.end > that.end) 1 else 0
      }
    }
  }

  /**
   * Intersect a bunch of intervals with this one.
   * @param intervals Intervals to intersect with this one.
   * @return The intersected intervals.
   */
  def intersect(intervals : Traversable[Interval]) = (SortedSet.empty[Interval] /: intervals){ (res, i) =>
    i.overlap(this) match {
      case None => res
      case Some(o) => res + o
    }
  }

  /**
   * Clip an interval by another interval.  The result will be a set of either
   * 0, 1, or 2 intervals.  If the clipper is completely outside the range of
   * the clipee, the original interval is returned.  If the clipper
   * completely overlaps the clipee, an empty set is returned.  If one end
   * of the interval is overlapped by the other interval, a single shortened
   * interval is returned.  Finally, if the clipper is inside of the clipee,
   * two intervals are returned (one for each side of this interval that
   * didn't get clipped.
   *
   * @param that
   * @return
   */
  def clip(that : Interval) : List[Interval] = that match {
    case _ if that contains this => Nil
    case _ if !(this touches that) => this :: Nil
    case _ if this.start < that.start && this.end > that.end =>
      (this.start until that.start) :: (that.end until this.end) :: Nil
    case _ if that.end < this.end => (that.end until this.end) :: Nil
    case _ => (this.start until that.start) :: Nil
  }

  /**
   * Clip many intervals against this one.
   * @param clipIntervals intervals to clip against this one.
   * @return Set of clipped intervals.
   */
  def clip(clipIntervals : Traversable[Interval]) : SortedSet[Interval] = {
    def m(current : Interval, clips : List[Interval], res : SortedSet[Interval]) : SortedSet[Interval] = clips match {
      case Nil => res + current
      case head :: tail => current clip head match {
        case pre :: post :: Nil => m(post, tail, res + pre)
        case i :: Nil => m(i, tail, res)
        case _ => res
      }
    }
    m(this, IntervalUtil.merge(clipIntervals filter { _ overlaps this }).toList, SortedSet.empty[Interval])
  }

  /**
   * Subdivide this interval into N evenly spaced intervals.  If N = 0, the
   * returned intervals are empty.
   * @param numIntervals Number of evenly spaced intervals to return.
   * @return Collection of evenly spaced intervals.
   */
  def subdivide(numIntervals : Int) = numIntervals match {
    case _ if numIntervals < 1 => Nil
    case _ =>
      val t = this.start.value
      val delta = this.getDurationDays / numIntervals
      (0 until numIntervals) map { i => JD(t + i * delta) until JD(t + (i + 1) * delta) }
  }
  
  def divvyto(n : Int) = (0 to n) map { x => extrapolate(x.doubleValue / n) }
  def divvyuntil(n : Int) = (0 until n) map { x => extrapolate(x.doubleValue / n) }
  def divvytoPar(n : Int) = (0 to n).par map { x => extrapolate(x.doubleValue / n) }
  def divvyuntilPar(n : Int) = (0 until n).par map { x => extrapolate(x.doubleValue / n) }

  final def dtDivide(seconds : Double) : SortedSet[JD] = {
    def inner(dt : Double, t : JD, res : SortedSet[JD]) : SortedSet[JD] = if(t <= end) {
      inner(dt, t plusSeconds dt, res + t)
    } else res
    inner(seconds, start, SortedSet.empty[JD])
  }

  final def dtDivideL(seconds : Double) : List[JD] = {
    def inner(dt : Double, t : JD, res : List[JD]) : List[JD] = if(t <= end) {
      inner(dt, t plusSeconds dt, t :: res)
    } else res
    inner(seconds, start, Nil)
  }

  final def stepBy[T](seconds : Double)(f : JD => T) : SortedMap[JD, T] = {
    def inner(dt : Double, t : JD, res : SortedMap[JD, T]) : SortedMap[JD, T] = if(t <= end) {
     inner(dt, t plusSeconds dt, res + (t -> f(t)))
    } else res
    inner(seconds, start, SortedMap.empty[JD, T])
  }
}
