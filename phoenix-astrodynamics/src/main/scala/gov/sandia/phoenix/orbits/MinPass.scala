package gov.sandia.phoenix.orbits

import gov.sandia.phoenix.elements.tle.TLE
import gov.sandia.phoenix.propagators.Propagator
import gov.sandia.phoenix.propagators.sgp4.SGP4
import gov.sandia.phoenix.time.Interval
import gov.sandia.phoenix.uom.{Meters, Seconds, Time}

import scala.collection.SortedMap

object MinPass {
  def findMin(a : Propagator, b : Propagator, interval : Interval, tol : Time) = {
    val dTol = tol / Seconds(interval.getDurationSeconds.doubleValue())
    val (minT, minDist) = gov.sandia.phoenix.numerics.optimization.goldenMin(0.0, 1.0, dTol){ x =>
      val t = interval.interpolate(x)
      (a.position(t) flatMap { apos =>
        b.position(t) map { bpos =>
          apos dist bpos
        }
      }).getOrElse(Double.MaxValue)
    }
    (interval.interpolate(minT), minDist)
  }
  
  def computeRendevous(a : Propagator, b : Propagator, interval : Interval, steps : Int) = {
    val times = interval.divvyto(steps)
    val solutions = for(startIndex <- 1 until times.length - 1;
        endIndex <- startIndex until times.length;
        interval = times(startIndex) until times(endIndex);
        astate = a.unsafe_state(times(startIndex));
        tInitialState = b.unsafe_state(interval.start);
        tFinalState = b.unsafe_state(interval.end);
        solution = astate.rendezvous(tInitialState, tFinalState, interval.getDurationSeconds.doubleValue(), true))
        yield (interval, solution)
    (SortedMap.empty[Interval, RendezvousSolution] /: solutions){ case (m, s) => m + s }
    }
}

object MinPassTestApp extends App {
  val a = TLE(Some("0 ISS (ZARYA)"),
    "1 25544U 98067A   14222.19124509  .00007044  00000-0  12834-3 0  8661",
    "2 25544 051.6454 191.8885 0007020 353.2075 140.6079 15.50643715899708")

  val b = TLE(Some("0 ISS (ZARYA)"),
    "1 25544U 98067A   14223.22448236  .00006356  00000-0  11655-3 0  8713",
    "2 25544 051.6454 186.7643 0006993 357.2138 148.2812 15.50654986899863")

  val pa = SGP4(a)
  val pb = SGP4(b)

  val interval = a.epoch until b.epoch

  val themin = MinPass.findMin(pa, pb, interval, Seconds(1))
  println(themin._1.toGregorianDate)
  println(Meters(themin._2))

  val solutions = MinPass.computeRendevous(pa, pb, interval, 100)
  val bestSolution = solutions.minBy(_._2.deltaV)
  println(bestSolution._1.start.toGregorianDate + ", " +
    bestSolution._1.end.toGregorianDate + ": " + bestSolution._2.deltaV)
}
