package gov.sandia.phoenix.elements.igs

import java.io.{File, FileInputStream, InputStream}
import java.util.regex.MatchResult
import java.util.{Map, Scanner, Set}

import gov.sandia.phoenix.geometry.Vector3
import gov.sandia.phoenix.time.{JD, TimeBuilder}

class IGSPreciseOrbitsAndClocks(is : InputStream) {
  def this(fileName : String) = this(new FileInputStream(fileName))
  private final val igsValues: Map[JD, Map[Integer, Vector3]] = new java.util.TreeMap[JD, Map[Integer, Vector3]]
  private var date: JD = null
  private final val accuracyMap: Map[Integer, Integer] = new java.util.HashMap[Integer, Integer]
  private final val prnSet: Set[Integer] = new java.util.TreeSet[Integer]
  read(is)

  def getUTCTimes: Set[JD] = igsValues.keySet
  def position(date: JD, id: Integer): Vector3 = igsValues.get(date).get(id)

  /**
   * @return
   */
  def getPRNs: Set[Integer] = {
    return prnSet
  }

  private def read(is: InputStream) {
    val scanner: Scanner = new Scanner(is)

    // This parsing format is based on the description of SP3 files here:
    // https://igscb.jpl.nasa.gov/igscb/data/format/sp3_docu.txt

    //Line 1
    scanner.findInLine(".{2}[PV](\\d{4}) ([\\s\\d]{2}) ([\\s\\d]{2}) ([\\s\\d]{2}) ([\\s\\d]{2}) ([\\s\\d\\.]{11}) [\\s\\d]{7} .{5} .{5} .{3} .{4}")
    var result: MatchResult = scanner.`match`
    date = IGSPreciseOrbitsAndClocks.parseDate(result)
    scanner.nextLine

    //Line 2
    scanner.findInLine("## \\d{4} [\\.\\d]{15} [\\s\\.\\d]{14} \\d{5} [\\.\\d]{15}")
    scanner.nextLine

    //Lines 3-7
    scanner.findInLine("\\+.{3}(\\d{2})")
    result = scanner.`match`
    val numSats: Int = result.group(1).trim.toInt
    val satsArray: Array[Int] = new Array[Int](numSats)

    // 85 is the maximum number of satellites that can be specified
    // 17 satellites per line
    for(i <- 1 to 85){
      if (i % 17 == 0)
        scanner.nextLine
      if(i < numSats){
        scanner.findInLine("G([\\s\\d]{2})")
        result = scanner.`match`
        satsArray(i) = result.group(1).trim.toInt
      } else scanner.findInLine("[\\+\\s]*0")
    }

    // Lines 8-12
    for(i <- 1 to 85){
      if (i % 17 == 0)
        scanner.nextLine
      if(i < numSats){
        scanner.findInLine("[\\s\\+]*([\\d]+)")
        result = scanner.`match`
        accuracyMap.put(satsArray(i), result.group(1).trim.toInt)
      } else scanner.findInLine("[\\+\\s]*0")
    }

    // Line 13
    scanner.findInLine("%.{8}(.{3})")
    result = scanner.`match`
    assert(result.group(1) == "GPS", "If this isn't GPS, this code needs enhancement.")
    scanner.nextLine

    // Lines 14-18
    while (scanner.hasNext("[#+%/]+.*")) scanner.nextLine

    val dateRegex: String = "\\*\\s+([\\d]+)\\s+([\\d]+)\\s+([\\d]+)\\s+([\\d]+)\\s+([\\d]+)\\s+([\\d\\.]*)"
    val ecefRegex: String = "[GP\\s]+(\\d+)\\s+([\\d\\.+-]+)\\s+([\\d\\.+-]+)\\s+([\\d\\.+-]+)\\s+.+"

    // Lines 19-end: satellite data starts at line 23 (ignore lines 19-22)
    while (scanner.hasNext("\\*")) {
      scanner.findInLine(dateRegex)
      result = scanner.`match`
      var timeStamp: JD = IGSPreciseOrbitsAndClocks.parseDate(result)

      //Moving from GPS time to UTC
      timeStamp = timeStamp.minusSeconds(14)

      val prnMap: Map[Integer, Vector3] = new java.util.TreeMap[Integer, Vector3]
      igsValues.put(timeStamp, prnMap)

      scanner.nextLine
      // Lines 24-end -- this is the satellite data
      for(i <- 0 until numSats){
        scanner.findInLine(ecefRegex)
        result = scanner.`match`
        val prn: Int = result.group(1).toInt
        //Convert to meters
        val x = result.group(2).toDouble * 1000
        val y = result.group(3).toDouble * 1000
        val z = result.group(4).toDouble * 1000
        val vec = new Vector3(x, y, z)

        if(0 != accuracyMap.get(prn)){
          prnMap.put(prn, vec)
          prnSet.add(prn)
        }
        scanner.nextLine
      }
    }
    scanner.close
  }
}


object IGSPreciseOrbitsAndClocks {
  private def parseDate(result: MatchResult): JD = {
    val year = result.group(1).toInt
    val month = result.group(2).trim.toInt
    val day = result.group(3).trim.toInt
    val hour = result.group(4).trim.toInt
    val min = result.group(5).trim.toInt
    val seconds = result.group(6).toDouble
    TimeBuilder(year, month, day, hour, min, seconds.asInstanceOf[Int], 0.0)
  }
}