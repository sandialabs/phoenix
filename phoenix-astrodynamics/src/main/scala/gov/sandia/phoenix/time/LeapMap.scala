package gov.sandia.phoenix.time

import scala.collection.SortedMap

trait LeapMap {
  def leapseconds : SortedMap[Double, Double]
  def apply(jd_utc : Double) = (leapseconds to jd_utc).lastOption.getOrElse(0.0 -> 0.0)._2
  def apply(t_utc : JD) = (leapseconds to t_utc.doubleValue).lastOption.getOrElse(0.0 -> 0.0)._2
}