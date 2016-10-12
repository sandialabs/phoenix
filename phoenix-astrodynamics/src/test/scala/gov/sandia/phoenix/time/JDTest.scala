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

package gov.sandia.phoenix.time

import org.scalatest.FunSuite
import java.math.MathContext._
import scala.math.BigDecimal

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 *
 */
class JDTest extends FunSuite {
  test("BigDecimal Cache Test 1") {
    //Create two similar BDs
    val a = BigDecimal(12)
    val b = BigDecimal(12, java.math.MathContext.DECIMAL128)
    assert(b.mc === java.math.MathContext.DECIMAL128)
    assert(a.mc === b.mc)
    //If an exception is thrown, the math context is off.
    println(b / 7)
  }

  test("BigDecimal Cache Test 2") {
    //Create two similar BDs
    val a = BigDecimal(12)
    val b = BigDecimal(12, java.math.MathContext.DECIMAL32)
    assert(b.mc === java.math.MathContext.DECIMAL32)
    assert(a.mc != b.mc, a.mc + " should not be equal to "+ b.mc)
    //If an exception is thrown, the math context is off.
    println(b / 7)
  }

  test("Test for non-terminating decimal expansion") {
    val zip = BigDecimal(0, DECIMAL128)
    gregorianDateToJD(2010,3,23,18,59,43, zip)
    gregorianDateToJD(2010,3,23,18,59,43, BigDecimal(0, DECIMAL128))
    assert(true)
  }
  
  test("2010,3,23,19,15,25,999") {
    val millis = BigDecimal(999, DECIMAL128)
    gregorianDateToJD(2010,3,23,19,15,25,millis)
  }
  
  test("2010,3,23,19,15,26,0") {
    gregorianDateToJD(2010,3,23,19,15,26,0)
  }
  
  test("2010,3,23,19,23,21,12") {
//    val millis = BigDecimal(12, DECIMAL128)
//    PreciseTime.toJulianDate(2010,3,23,19,23,21,millis)
    val millis = new BigDecimal(BigDecimal(12).bigDecimal, DECIMAL128)
    gregorianDateToJD(2010,3,23,19,23,21,millis)
  }
  
  test("Constructor") {
    for(i <- 0 until 10000) gov.sandia.phoenix.time.now
    assert(true)
  }
}
