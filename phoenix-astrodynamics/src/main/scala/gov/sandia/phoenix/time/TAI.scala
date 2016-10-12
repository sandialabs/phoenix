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

import java.util.logging.Logger

import scala.collection.immutable._
import scala.io._

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object TAI extends LeapMap {
  val logger = Logger.getLogger(getClass.getName)

  val r = """.+\s+=JD\s+([\.\d]+).+\s+TAI-UTC=\s+([\.\d]+).+""".r
  def parse(lines : List[String], res : SortedMap[Double, Double] = SortedMap.empty) : SortedMap[Double, Double] =
    lines match {
      case Nil => res
      case r(key, value) :: tail => parse(tail, res+(key.toDouble->value.toDouble))
      case head :: tail => parse(tail, res)
    }

  //Add documentation to link to the NOAA site.
  lazy val leapseconds = parse(Source.fromInputStream(getClass.getResourceAsStream("leapsec.dat")).getLines.toList)
}
