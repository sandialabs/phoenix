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

package gov.sandia.phoenix.collections

import scala.collection.mutable

/**
 * This is currently only used by DTED, but it prevents an entire library dependency sinc java.util.collections doesn't
 * have an LRU implemented and we don't want all of Apache Commons Collections just for this.
 */
class LRUMap[K, V](val capacity : Int) {
  private var cache = new mutable.LinkedHashMap[K, V]
  def put(key : K, value : V) = cache.synchronized {
    cache+=key->value
    cache.size - capacity match {
      case overflow if overflow > 0 => cache = cache.dropRight(overflow)
      case _ =>
    }
  }

  def size = cache.size
  def get(key : K) = cache.get(key)
  def apply(key : K) = cache(key)
  def containsKey(key : K) = cache.contains(key)
}