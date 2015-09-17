package gov.sandia.phoenix.propagators.sgp4

/**
 * Created by mbastia on 4/30/14.
 */
trait ResonanceTerms {
  def xlamo : Double
  def xfact : Double
  def frequency : Int
  def update(atime : Double, xli : Double, xni : Double) : (Double, Double, Double)
}
