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

package gov.sandia.phoenix.propagators.sgp4

/**
 * Created by mbastia on 4/30/14.
 */
trait ResonanceTerms {
  def xlamo : Double
  def xfact : Double
  def frequency : Int
  def update(atime : Double, xli : Double, xni : Double) : (Double, Double, Double)
}
