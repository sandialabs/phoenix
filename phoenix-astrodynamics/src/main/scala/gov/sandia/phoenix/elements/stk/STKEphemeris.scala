package gov.sandia.phoenix.elements.stk

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.time._

import scala.io._
import scala.collection.immutable._
import gov.sandia.phoenix.propagators.{ECIStateVectorPropagator, Propagator}

/**
 * STK Ephemeris File
 * There are a few outstanding issues regarding how to parse this file type,
 * mostly centering around how boundaries are defined.
 * @see http://stk.com/resources/help/online/stk/source/stk/importfiles-02.htm
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object STKEphemeris {
  def apply(fileName : String) = parse(Source.fromFile(fileName).getLines().toList)

  def parse(l : List[String],
            epoch : Option[JD] = None,
            boundaries : SortedSet[Double] = SortedSet.empty,
            states : List[(Double, ECIStateVector)] = Nil) :
  STKEphemeris = l match {
    case Nil => new STKEphemeris(epoch, boundaries, reconcile(boundaries.toList, states))
    case BEGINSegmentBoundaryTimes() :: tail =>
      val b = extractBoundaries(tail, boundaries)
      parse(b._1, epoch, b._2, states)
    case STKScenarioEpoch(t) :: tail => parse(tail, Some(t), boundaries, states)
    case EphemerisTimePosVel() :: tail =>
      val (rest, newStates) = extractStates(tail)
      parse(rest, epoch, boundaries, newStates)
    case head :: tail => parse(tail, epoch, boundaries, states)
  }

  private def reconcile(boundaries : List[Double], states : List[(Double, ECIStateVector)],
                        res : SortedMap[Double, STKEphemerisSegment] = SortedMap.empty) :
  SortedMap[Double, STKEphemerisSegment] = boundaries match {
    case min :: max :: tail =>
      val (rem, respart) = partition(min, max, states)
      if(respart.isEmpty) reconcile(tail, rem, res) else {
        reconcile(boundaries.tail, rem, res+(min->new STKEphemerisSegment(min, max, respart)))
      }
    case min :: tail =>
      val (rem, respart) = partition(min, Double.PositiveInfinity, states)
      if(respart.isEmpty) reconcile(tail, rem, res) else {
        reconcile(boundaries.tail, rem, res+(min->new STKEphemerisSegment(min, respart.lastKey, respart)))
      }
    case _ => res
  }

  private def partition(min : Double, max : Double, states : List[(Double, ECIStateVector)],
                        res : SortedMap[Double, ECIStateVector] = SortedMap.empty) :
  (List[(Double, ECIStateVector)], SortedMap[Double, ECIStateVector])  = states match {
    case Nil => (Nil, res)
    case head :: tail => if(head._1 <= max && !res.contains(head._1))
      partition(min, max, tail, res+head) else (states, res)
  }

  private def extractStates(l : List[String], res : List[(Double, ECIStateVector)] = Nil) :
  (List[String], List[(Double, ECIStateVector)]) = l match {
    case Nil => throw new Exception("EOF reached before END Ephemeris")
    case ENDEphemeris() :: tail => (tail, res.reverse)
    case StateExtractor(dt, state) :: tail => extractStates(tail, (dt, state) :: res)
    case head :: tail => extractStates(tail, res)
  }

  private def extractBoundaries(l : List[String], boundaries : SortedSet[Double]) :
  (List[String], SortedSet[Double]) = l match {
    case ENDSegmentBoundaryTimes() :: tail => (tail, boundaries)
    case SegmentBoundaryExtractor(b) :: tail => extractBoundaries(tail, boundaries+b)
    case _ => extractBoundaries(l.tail, boundaries)
  }
}

/**
 * STK Ephemeris File
 * @see http://stk.com/resources/help/online/stk/source/stk/importfiles-02.htm
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class STKEphemeris(val scenarioEpoch : Option[JD],
                   val boundaries : SortedSet[Double],
                   val segments : SortedMap[Double, STKEphemerisSegment]) extends Propagator {
  def state(t : JD) = scenarioEpoch match {
    case Some(e) => Some {
        val interval = e until t
        val dt = interval.getDurationSeconds.doubleValue
        val key = segments.to(dt).lastKey
        val segment = segments(key)
        val stateKey = segment.states.to(dt).lastKey
        val nearestState = segment.states(stateKey)
        val stateEpoch = e plusSeconds stateKey
        if(key == segments.firstKey) {
          val tv = segment.states.keySet.toArray
          val sv = segment.states.values.toArray
          val index = java.util.Arrays.binarySearch(tv, dt)
          if(index >= 0) sv(index) else {
            val ilo = -index - 2
            val ihi = ilo + 1
            val a = sv(ilo)
            val b = sv(ihi)

            val i = (dt - tv(ilo)) / (tv(ihi) - tv(ilo))
            require(i > 0.0 && i < 1.0)
            val p = a.position.lerp(b.position, i)
            val v = a.velocity.lerp(b.velocity, i)
            ECIStateVector(p, v)
          }
        } else ECIStateVectorPropagator(stateEpoch, nearestState).unsafe_state(t)
      }
    case None => None
  }

  val start = scenarioEpoch match {
    case Some(t) => t
    case None => throw new Exception("This STK Ephemeris has no epoch.")
  }
  val end = scenarioEpoch match {
    case Some(t) => boundaries.lastOption match {
        case Some(dt) => t plusSeconds segments(dt).end
        case None => throw new Exception("This STK Ephemeris has no boundaries.")
      }
    case None => throw new Exception("This STK Ephemeris has no epoch.")
  }

  def boundariesArray = boundaries.toArray
  def boundaryTimes = boundaries.map(start plusSeconds)
  def boundaryTimesArray = boundaryTimes.toArray
  def segment(t : Double) : STKEphemerisSegment = segments(t)
  def segment(index : Int) : STKEphemerisSegment = segment(boundariesArray(index))

  def epoch = scenarioEpoch match { 
    case Some(e) => e
    case None => throw new Exception("Epoch not constant for STK Ephemeris")
  }

  def getPeriod = throw new Exception("Period not constant for STK Ephemeris")
  def getRAAN = throw new Exception("Right Ascension not constant for STK Ephemeris")
  def getArgP = throw new Exception("Argument of Perigee not constant for STK Ephemeris")
  def getMeanAnomaly = throw new Exception("Mean Anomaly not constant for STK Ephemeris")
  def getSemimajorAxis = throw new Exception("Axis not constant for STK Ephemeris")

  override def toString = {
    "stk.v.4.0\n\n#WrittenBy PHOENIX\n\nBEGIN Ephemeris\n\n" +
    "NumberOfEphemerisPoints " + (0 /: segments.values)(_+_.states.size) + "\n\n" +
    {scenarioEpoch match {
        case Some(t) => "ScenarioEpoch    " + DDMMMYYYYFormat(t) + "\n\n"
        case None => ""
      }}+
    "CentralBody    Earth\n\n" +
    "CoordinateSystem     J2000\n\n" +
    "BEGIN SegmentBoundaryTimes\n" +
    ("" /: boundaries)(_+"\n\t"+STKDoubleFormat.format(_)) +
    "\n\nEND SegmentBoundaryTimes\n\n" +
    "EphemerisTimePosVel\n\n" + ("" /: segments){(s, segment)=>
      s + ("" /: segment._2.states){(ss, state)=>
        ss +  STKDoubleFormat.format(state._1) + " " +
          STKDoubleFormat.format(state._2.position.x) + " " + STKDoubleFormat.format(state._2.position.y) + " " +
          STKDoubleFormat.format(state._2.position.z) + " " + STKDoubleFormat.format(state._2.velocity.x) + " " +
          STKDoubleFormat.format(state._2.velocity.y) + " " + STKDoubleFormat.format(state._2.velocity.z) + "\n"
      } + "\n"
    } + "\n\nEND Ephemeris"
  }
}

private object SegmentBoundaryExtractor {
  def unapply(s : String) = if(s.trim.length > 0) Some(s.trim.toDouble) else None
}

private object BEGINSegmentBoundaryTimes {
  def unapply(s : String) = (s contains "BEGIN") && (s contains "SegmentBoundaryTimes")
}

private object ENDSegmentBoundaryTimes {
  def unapply(s : String) = (s contains "END") && (s contains "SegmentBoundaryTimes")
}

private object BEGINEphemeris {
  def unapply(s : String) = (s contains "BEGIN") && (s contains "Ephemeris")
}

private object ENDEphemeris {
  def unapply(s : String) = (s contains "END") && (s contains "Ephemeris")
}

private object EphemerisTimePosVel {
  def unapply(s : String) = s contains "EphemerisTimePosVel"
}

private object EphemerisECFTimePosVel {
  def unapply(s : String) = s contains "EphemerisECFTimePosVel"
}

private object StateExtractor {
  def unapply(s : String) = {
    val fields = s.replaceAll("\\s+", ",").split(",")
    if(fields.length == 7) Some {
      val dt = fields(0).toDouble
      val px = fields(1).toDouble
      val py = fields(2).toDouble
      val pz = fields(3).toDouble
      val vx = fields(4).toDouble
      val vy = fields(5).toDouble
      val vz = fields(6).toDouble
      (dt, ECIStateVector(gov.sandia.phoenix.geometry.Vector3(px, py, pz), gov.sandia.phoenix.geometry.Vector3(vx, vy, vz)))
    } else None
  }
}