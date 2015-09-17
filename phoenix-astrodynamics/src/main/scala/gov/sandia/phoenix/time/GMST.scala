package gov.sandia.phoenix.time

case class GMST(value : BigDecimal) {
  def doubleValue = this.value.doubleValue
}
