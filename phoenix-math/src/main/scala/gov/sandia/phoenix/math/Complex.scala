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

import scala.math._

/**
 * Complex numbers class.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class Complex(re : Double, im : Double) {
  def mul(that : Complex) = this * that
  def div(that : Complex) = this / that
  def mul(s : Double) = this * s
  def div(s : Double) = this / s
  def scala(s : Double) = this * s
  def plus(that : Complex) = this + that
  def minus(that : Complex) = this - that
  def inverse = !this
  def conjugate = ~this
  val mag = hypot(re, im)
  val phase = atan2(im, re)
  
  def * (that : Complex) = new Complex(this.re * that.re - this.im * that.im, this.im * that.re + this.re * that.im)
  def / (that : Complex) = {
    val den = that.re * that.re + that.im * that.im
    val rnum = this.re * that.re + this.im * that.im
    val inum = this.im * that.re - this.re * that.im
    new Complex(rnum / den, inum / den)
  }
  def * (s : Double) = new Complex(this.re * s, this.im * s)
  def / (s : Double) = this * (1.0 / s)
  def + (that : Complex) = new Complex(this.re + that.re, this.im + that.im)
  def + (re : Int) = new Complex(this.re + re, this.im)
  def - (that : Complex) = new Complex(this.re - that.re, this.im - that.im)
  def - (re : Int) = new Complex(this.re - re, this.im)
  def unary_! = { val den = re*re + im*im; new Complex(re / den, -im / den) }
  def unary_~ = new Complex(re, -im)

  override def toString = re + " + " + im + "i"
}