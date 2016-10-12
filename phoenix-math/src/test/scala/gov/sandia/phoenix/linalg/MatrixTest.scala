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

import org.scalatest.FunSuite
import scala.math._


class MatrixTest extends FunSuite {
  val M = new Matrix(3, 2, Array(3.0, 1.0, 8.0, 6.0, 0.0, 4.0))
  val N = new Matrix(2, 2, Array(5.0, 9.0, 7.0, 2.0))
  val R = new Matrix(3, 2, Array(22.0, 29.0, 82.0, 84.0, 28.0, 8.0))

  test("M * N") {
    M * N match {
      case Some(x) => assert(x.closeTo(R), "M * N != R")
      case None => assert(false, "Matrices should multiply.")
    }
  }

  test("inverse") {
    val A = new Matrix(3, 3, Array(3.0, -0.1, -0.2, 0.1, 7.0, -0.3, 0.3, -0.2, 10.0))
    !A match {
      case Some(inverse) =>
        A * inverse match {
          case Some(x) => (0 until x.rows) foreach { row =>
            (0 until x.columns) foreach { column =>
              if(row == column)
                assert(abs(1.0 - x(row, column)) <= 1.0E-14, "Should be 1")
              else assert(abs(x(row, column)) <= 1.0E-14, "Should be 1")
            }
          }
          case None => assert(false, "A * A.inverse was not I.")
        }
      case None => assert(false, A + " should be invertible.")
    }
  }

  test("pivoting 1A") {
    val A = new Matrix(2, 2, Array(2.0, 100000.0, 1.0, 1.0))
    val B = ColumnVector(100000.0, 2.0)
    val x = A.LUdecomp(B)
    assert(A * x forall { _.closeTo(B) }, "A * x != B")
  }

  test("pivoting 1B") {
    val A = new Matrix(2, 2, Array(1.0, 1.0, 2.0, 100000.0))
    val B = ColumnVector(2.0, 100000.0)
    val x = A.LUdecomp(B)
    assert(A * x forall { _.closeTo(B) }, "A * x != B")
  }

  test("pivoting http://en.wikipedia.org/wiki/Pivot_element") {
    val A = new Matrix(3, 3, Array(1.0, -1.0, 2.0, 0.0, 0.0, -1.0, 0.0, 2.0, -1.0))
    val B = ColumnVector(8.0, -11.0, -3.0)
    A.LUdecomp(B) match {
      case Some(x) => assert(A * x forall { _.closeTo(B) }, "A * x != B")
      case _ => assert(false, "Unabled to form LU decomposition. Pivoting may be failing.")
    }
  }

  test("noninvertable") {
    val A = new Matrix(3, 3, Array(3.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0.3, -0.2, 10.0))
    A.invert match {
      case Some(inverse) => assert(false, A + " should not be invertible. Inverse is " + inverse + ".")
      case None =>
    }
  }
}