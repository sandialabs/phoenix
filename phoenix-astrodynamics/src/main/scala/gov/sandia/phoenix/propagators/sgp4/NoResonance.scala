package gov.sandia.phoenix.propagators.sgp4

object NoResonance extends ResonanceTerms {
  val xlamo = 0.0
  val xfact = 0.0
  val frequency = 0
  def update(atime : Double, xli : Double, xni : Double) = (0.0, 0.0, 0.0)
}
