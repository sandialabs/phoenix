package gov.sandia.phoenix.numerics

object finitedifference {
  def dx(x : Double, Δ : Double)(f : Double => Double) = 0.5 * (f(x + Δ) - f(x - Δ)) / Δ
  def dxdx(x : Double, Δ : Double)(f : Double => Double) = (f(x + Δ) - 2.0 * f(x) - f(x - Δ)) / (Δ * Δ)
}
