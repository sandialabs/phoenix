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



/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
abstract class ArrayOfCyclicalEvents[T](val values : Vector[T]) {
  val count = values.length
  val first = this(0)
  val last = this(count - 1)
  def apply(index : Int) = values(index % count)
  def next(index : Int) = this((index + 1) % count)
  def previous(index : Int) = if(index == 0) last else this(index - 1)
}