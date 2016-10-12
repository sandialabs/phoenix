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

package gov.sandia.phoenix.elements.stk

import gov.sandia.phoenix.time.TimeBuilder

object STKScenarioEpoch {
  def unapply(s : String) = if(s.startsWith("ScenarioEpoch")) Some {
    val fields : Array[String] = s.replaceAll("\\s+", ",").split(",")
    val date = fields(1).toInt
    val month = fields(2) match {
      case "Jan" => 1
      case "Feb" => 2
      case "Mar" => 3
      case "Apr" => 4
      case "May" => 5
      case "Jun" => 6
      case "Jul" => 7
      case "Aug" => 8
      case "Sep" => 9
      case "Oct" => 10
      case "Nov" => 11
      case "Dec" => 12
      case _ => -1
    }
    val year = fields(3).toInt
    val hms = fields(4).split(":")
    val h = hms(0).toInt
    val m = hms(1).toInt
    val sec = hms(2).toDouble
    if(sec != 0.0) TimeBuilder(year, month, date, h, m) else
      TimeBuilder(year, month, date, h, m) plusSeconds sec
  } else None
}