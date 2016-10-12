/*
 * Copyright (c) 2016 Sandia Corporation. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by the
 * terms of this license.
 * You must not remove this notice, or any other, from this software.
 *
 * Contributors:
 * - Mark Bastian: Original author.
 * - See Git logs.
 */

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