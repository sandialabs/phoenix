package gov.sandia.phoenix.time

import java.util.Comparator

object JulianDateComparator extends Comparator[JD] {
  override def compare(a : JD, b : JD) = a.compareTo(b)
}
