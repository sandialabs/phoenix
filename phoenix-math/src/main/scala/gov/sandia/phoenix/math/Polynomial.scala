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

package gov.sandia.phoenix.math

import scala.collection.immutable.{IndexedSeq, Vector => IArray}
import scala.math._

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class Polynomial(coefficients : IndexedSeq[Double]) {
  def this(coefficients : Array[Double]) = this(IArray.empty++coefficients)
  
  def apply(t : Double) = horner(t, coefficients)

  def R(ti : Seq[Double], fti : IndexedSeq[Double]) = correlation_coefficient(for(t <- ti) yield this(t), fti)

  override def toString = {
    val terms = for(i <- coefficients.indices;
    a = abs(coefficients(i)); if a != 0.0;
    sign = if(signum(coefficients(i)) == 1.0) if(i == 0) "" else "+" else "-";
    term = sign + a) yield (term /: (for(t <- 0 until i) yield "*t"))(_+_)
    ("f(t)=" /: terms)(_+_)
  }
}