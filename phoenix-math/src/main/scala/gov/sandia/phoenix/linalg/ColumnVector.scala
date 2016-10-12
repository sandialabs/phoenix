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

package gov.sandia.phoenix.linalg

class ColumnVector(a : IndexedSeq[Double]) extends Matrix(a.length, 1, a){
  def this(d : Double*) = this(d.toIndexedSeq)
  override def unary_~ = new RowVector(data)
}

object ColumnVector {
  def apply(a : IndexedSeq[Double]) = new ColumnVector(a)
  def apply(a : Double*) = new ColumnVector(a.toIndexedSeq)
}