package gov.sandia.phoenix.numerics {
  import scala.math._

  object integration {
    def trap(n : Int, a : Double, b : Double)(f : Double => Double) = {
      val h = (b - a) / n
      h * 0.5 * ((f(a) + f(b)) /: (1 until n)){ (s, i)=> s + 2.0 * f(a + i * h) }
    }

    def romberg(a : Double, b : Double, maxit : Int, es : Double)(f : Double => Double) = {
      val I = Array.tabulate[Array[Double]](maxit){ i => Array.ofDim[Double](maxit - i) }
      I(0)(0) = trap(1, a, b)(f)
      def step(I : Array[Array[Double]], iter : Int = 1) : Double = 
        if(iter >= maxit) I(0)(iter - 1) else {
          I(iter)(0) = trap(1 << iter, a, b)(f)
          def extrap(k : Int, I : Array[Array[Double]]) : Unit = if(k < iter + 1) {
            val j = iter - k
            val c = 1 << (2 * k)
            I(j)(k) = (c * I(j + 1)(k - 1) - I(j)(k - 1)) / (c - 1)
            extrap(k + 1, I)
          }
          extrap(1, I)
          
          val err = abs((I(0)(iter) - I(0)(iter - 1)) / I(0)(iter)) * 100
          if(err < es) I(0)(iter) else step(I, iter + 1)
        }
      step(I)
    }
  }
}