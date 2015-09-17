package gov.sandia.phoenix.numerics

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class GeneralizedRungeKuttaFormulation(val bt : ButcherTableau) {
  def createSolver(df : (Double, IndexedSeq[Double]) => IndexedSeq[Double], controller : StepSizeController) = new RKSolver(df, bt, controller)
}

object RK_4 extends GeneralizedRungeKuttaFormulation(BT_RK_4)
object RKF_4_5 extends GeneralizedRungeKuttaFormulation(BT_RKF_4_5)
object RKCK_5_4 extends GeneralizedRungeKuttaFormulation(BT_RKCK_5_4)
object RKDP_8_7 extends GeneralizedRungeKuttaFormulation(BT_RKDP_8_7)