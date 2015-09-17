package gov.sandia.phoenix

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object BigMath {
  def sqrt(fx : BigDecimal) = {
    def sqrt(x : BigDecimal, i : Int) : BigDecimal = if(i == 0) x
      else sqrt(x - (x * x - fx)(java.math.MathContext.DECIMAL128) / (x * 2.0), i - 1)
    sqrt(fx, 10)
  }
}
