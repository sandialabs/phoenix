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

import scala.collection.SortedMap
import scala.annotation.tailrec

abstract class TimeSequence[T](val epochs : SortedMap[JD, T] = SortedMap.empty[JD, T]) {
  type TT
  def build(epochs : SortedMap[JD, T]) : TT

  lazy val arrayOfEpochs = epochs.keySet.toIndexedSeq

  def +(entry : (JD, T)) = build(epochs+entry)
  def -(instant : JD) = build(epochs-instant)
  def add(entry : (JD, T)) = this + entry
  def add(epoch : JD, value : T) = this + (epoch->value)
  def remove(instant : JD) = this - instant

  private final def index(instant : JD) = if(epochs.isEmpty || instant < epochs.firstKey) None else Some {
    @tailrec def find(i0 : Int, i1 : Int) : Int = (i0 + i1) / 2 match {
      case i if i == i0 => i
      case mid => if(instant < arrayOfEpochs(mid)) find(i0, mid) else find(mid, i1)
    }
    find(0, arrayOfEpochs.length)
  }

  def interval(instant : JD) = index(instant) map { start => Interval(arrayOfEpochs(start), arrayOfEpochs(start + 1)) }

  def apply(instant : JD) = if(epochs contains instant)
    epochs.get(instant) else
    index(instant) map { start => epochs(arrayOfEpochs(start)) }

  def interval = if(arrayOfEpochs.length > 1) Some(arrayOfEpochs(0) until arrayOfEpochs(arrayOfEpochs.length - 1)) else None
}
