/**
 *
 */
package gov.sandia.phoenix.time



import org.scalatest.FunSuite

import scala.math.BigDecimal

/**
 * @author <A HREF="mailto:markbastian@gmail.com">Mark Bastian</A>
 */

class JDFrameConversionsTest extends FunSuite {
  test("2000 Jan 1.5") {
    assert(gregorianDateToJD( 2000, 1, 1, 12, 0, 0, 0) === 2451545.0)
  }
  test("1987 Jan 27.0") {
    assert(gregorianDateToJD( 1987, 1, 27, 0, 0, 0, 0) === 2446822.5)
  }
  test("1987 Jun 19.5") {
    assert(gregorianDateToJD( 1987, 6, 19, 12, 0, 0, 0) === 2446966.0)
  }
  test("1988 Jan 27.0") {
    assert(gregorianDateToJD( 1988, 1, 27, 0, 0, 0, 0) === 2447187.5)
  }
  test("1988 Jun 19.5") {
    assert(gregorianDateToJD( 1988, 6, 19, 12, 0, 0, 0) === 2447332.0)
  }
  test("1900 Jan 1.0") {
    assert(gregorianDateToJD( 1900, 1, 1, 0, 0, 0, 0) === 2415020.5)
  }
  test("1600 Jan 1.0") {
    assert(gregorianDateToJD( 1600, 1, 1, 0, 0, 0, 0) === 2305447.5)
  }
  test("1600 Dec 31.0") {
    assert(gregorianDateToJD( 1600, 12, 31, 0, 0, 0, 0) === 2305812.5)
  }
  // Must cast BigDecimal to Double
  test("837 Apr 10.3") {
    assert(gregorianDateToJD(  837, 4, 10, 7, 12, 0, 0) === BigDecimal(2026871.8))
  }
  test("-1000 Jul 12.5") {
    assert(gregorianDateToJD(-1000, 7, 12, 12, 0, 0, 0) === 1356001.0)
  }
  test("-1000 Feb 29.0") {
    assert(gregorianDateToJD(-1000, 2, 29, 0, 0, 0, 0) === 1355866.5)
  }
  test("-1001 Aug 17.9") {
    assert(gregorianDateToJD(-1001, 8, 17, 21, 36, 0, 0) === BigDecimal(1355671.4))
  }
  test("-4712 Jan 1.5") {
    assert(gregorianDateToJD(-4712, 1, 1, 12, 0, 0, 0) === 0.0)
  }
}
