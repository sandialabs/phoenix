package gov.sandia.phoenix.time

import scala.math.BigDecimal

/**
 * A class that conveniently wraps a Modified Julian Date
 */
class MJD(val value : BigDecimal) {
  def toJulianDate = JD(value + 2400000.5)
  def doubleValue = this.value.doubleValue
}