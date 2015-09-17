package gov.sandia.phoenix.elements.util

import gov.sandia.phoenix.elements.sgp4.{ECIStateVectorMeanElements, MeanElements}
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.elements.tle.TLE
import gov.sandia.phoenix.geometry._
import gov.sandia.phoenix.linalg.{ColumnVector, Matrix}
import gov.sandia.phoenix.propagators.sgp4.{SGP4, WGS72}
import gov.sandia.phoenix.time.JD

import scala.annotation.tailrec

class OsculatingToMeanElements(val osculatingElements : ECIStateVector, val epoch : JD, val bstarDrag : Double) {
  final def fx(x : ECIStateVector) = SGP4(ECIStateVectorMeanElements(x, epoch, bstarDrag, WGS72)).unsafe_state(epoch) - osculatingElements

  //x+ = x - Jinv*f(x)
  @tailrec private final def iter(x : ECIStateVector,
                                  bstar : Double,
                                  epoch : JD,
                                  tol : Double,
                                  maxIter : Int,
                                  numIter : Int = 0) : Option[ECIStateVector] = {
    val delta = 100.0
    val p = x.perturb(delta)
    val a = p map { s => SGP4(ECIStateVectorMeanElements(s, epoch, bstar, WGS72)) } map { _.unsafe_state(epoch).toArray }
    val jacobian = Array.tabulate[Array[Double]](6) { i =>
      (a(2 * i) zip a(2 * i + 1)) map { pr => (pr._1 - pr._2) / (2.0 * delta) }
    }

    val jtx = Array.tabulate(jacobian(0).length, jacobian.length){ (i, j) => jacobian(j)(i) }

    val J = new Matrix(jtx.length, jtx.length, jtx.flatten)
    val oc = J.invert flatMap { inverse => inverse * new ColumnVector(fx(x).toArray) }

    oc match {
      case Some(correction) =>
        val pos = Vector3(x.position.x - correction(0, 0), x.position.y - correction(0, 1), x.position.z - correction(0, 2))
        val vel = Vector3(x.velocity.x - correction(0, 3), x.velocity.y - correction(0, 4), x.velocity.z - correction(0, 5))
        val corrected = ECIStateVector(pos, vel)
        val err = correction.norm

        if(numIter >= maxIter) None else
        if(err < tol) Some(corrected) else iter(corrected, bstar, epoch, tol, maxIter, numIter + 1)
      case None => None
    }
  }

  def solve(tol : Double, maxIter : Int) = iter(osculatingElements, bstarDrag, epoch, tol, maxIter)
}
