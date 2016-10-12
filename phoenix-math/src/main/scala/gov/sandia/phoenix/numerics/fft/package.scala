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

import gov.sandia.phoenix.math.Complex

import scala.math._

/**
 * http://stackoverflow.com/questions/4364823/how-to-get-frequency-from-fft-result
 * http://introcs.cs.princeton.edu/java/97data/FFT.java.html
 * http://www.cv.nrao.edu/course/astr534/FourierTransforms.html
 * http://www.mathworks.com/support/tech-notes/1700/1702.html
 */
package object fft {
  /**
   * Compute the power spectrum response to a function.
   * @param freqHz The function sample frequency, in Hertz.
   * @param suggestedSamples The suggested number of samples. If it is not a 
   * power of 2 it will be raised to the next power of 2.
   * @param f Function to evaluate the power spectrum response for.
   * @return (Frequency, Power) mapping.
   */
  def power(freqHz : Double, suggestedSamples : Int)(f : Double => Double) : Array[(Double, Double)] = withFreq(power)(freqHz, suggestedSamples)(f)
  
  /**
   * Compute the FFT of a function.
   * @param freqHz The function sample frequency, in Hertz.
   * @param suggestedSamples The suggested number of samples. If it is not a 
   * power of 2 it will be raised to the next power of 2.
   * @param f Function to evaluate the power spectrum response for.
   * @return (Frequency, FFT) mapping.
   */
  def fft(freqHz : Double, suggestedSamples : Int)(f : Double => Double) : Array[(Double, Complex)] = withFreq(fft)(freqHz, suggestedSamples)(f)
  
  /**
   * Curryable function that computes frequency to T mapping.
   */
  private final def withFreq[T](b : Array[Double] => Array[T])(freqHz : Double, suggestedSamples : Int)(f : Double => Double) : Array[(Double, T)] = {
    val actualSamples = pow(2, ceil(log(suggestedSamples) / log(2))).toInt
    b(Array.tabulate[Double](actualSamples)(i => f(i / freqHz))).zipWithIndex map { p =>
      (p._2 * freqHz / actualSamples, p._1)
    }
  }
  
  /**
   * Compute the power spectrum transform of an array of points.
   * 
   */
  def power(x : Array[Double]) = {
    val f = fft(x) map { _.mag / x.length } map { x => x * x }
    Array.tabulate[Double](f.length){ i => if(i == 0) f(i) else 2.0 * f(i) }
  }
  
  /**
   * Compute the power spectrum transform of an array of real values.
   */
  def fft(x : Array[Double]) : Array[Complex] = fft(x map { new Complex(_, 0) }) slice (0, x.length / 2)
  
  /**
   * Compute the power spectrum transform of an array of complex values.
   */
  def fft(x : Array[Complex]) : Array[Complex] = if(x.length == 1) x else {
    val N = x.length
    //FFTs of even and odd terms
    val q = fft(Array.tabulate[Complex](N / 2){ k => x(2 * k) })
    val r = fft(Array.tabulate[Complex](N / 2){ k => x(2 * k + 1) })
      
    //Combine results
    val y = Array.ofDim[Complex](N)
    for(k <- 0 until N / 2) {
      val kth = -2 * k * Pi / N
      val wk = new Complex(cos(kth), sin(kth))
      val wkrk = wk * r(k)
      y(k) = q(k) + wkrk
      y(k + N / 2) = q(k) - wkrk
    }
    y
  }

  /**
   * http://paulbourke.net/miscellaneous/dft/
   */
  def fft2(x : Array[Array[Complex]]) = {
    val res = x.clone()
    //Transform rows
    for(j <- x(0).indices){
      val f = fft((for(i <- x.indices) yield res(i)(j)).toArray)
      for(i <- x.indices) res(i)(j) = f(i)
    }
    //Transform cols
    for(i <- x.indices){
      val f = fft((for(j <- x(0).indices) yield res(i)(j)).toArray)
      for(j<- x(0).indices) res(i)(j) = f(j)
    }
    res
  }

  def ifft2(x : Array[Array[Complex]]) = {
    val res = x.clone()
    //Transform rows
    for(j <- x(0).indices){
      val f = ifft((for(i <- x.indices) yield res(i)(j)).toArray)
      for(i <- x.indices) res(i)(j) = f(i)
    }
    //Transform cols
    for(i <- x.indices){
      val f = ifft((for(j <- x(0).indices) yield res(i)(j)).toArray)
      for(j<- x(0).indices) res(i)(j) = f(j)
    }
    res
  }

  /**
   * Inverse FFT
   */
  def ifft(x : Array[Complex]) = fft(x map { _.conjugate }) map { _.conjugate / x.length }
}