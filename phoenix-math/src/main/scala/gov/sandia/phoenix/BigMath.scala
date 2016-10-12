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
