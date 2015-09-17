package gov.sandia.phoenix.time

import scala.collection._

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object GPS extends LeapMap {
  //http://en.wikipedia.org/wiki/Leap_second
  //TODO - sync to the tai leapsec.dat file since it's just a 19 sec difference
  lazy val leapseconds = SortedMap(
    TimeBuilder(2015, 7, 1).doubleValue->17.0,
    TimeBuilder(2012, 7, 1).doubleValue->16.0,
    TimeBuilder(2009, 1, 1).doubleValue->15.0,
    TimeBuilder(2006, 1, 1).doubleValue->14.0,
    TimeBuilder(1999, 1, 1).doubleValue->13.0,
    TimeBuilder(1997, 7, 1).doubleValue->12.0,
    TimeBuilder(1996, 1, 1).doubleValue->11.0,
    TimeBuilder(1994, 7, 1).doubleValue->10.0,
    TimeBuilder(1993, 7, 1).doubleValue->9.0,
    TimeBuilder(1992, 7, 1).doubleValue->8.0,
    TimeBuilder(1991, 1, 1).doubleValue->7.0,
    TimeBuilder(1990, 1, 1).doubleValue->6.0,
    TimeBuilder(1988, 1, 1).doubleValue->5.0,
    TimeBuilder(1985, 7, 1).doubleValue->4.0,
    TimeBuilder(1983, 7, 1).doubleValue->3.0,
    TimeBuilder(1982, 7, 1).doubleValue->2.0,
    TimeBuilder(1981, 7, 1).doubleValue->1.0)
}