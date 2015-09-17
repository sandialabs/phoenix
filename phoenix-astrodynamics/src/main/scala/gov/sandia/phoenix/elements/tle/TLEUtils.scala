package gov.sandia.phoenix.elements.tle

import java.io._
import java.net._
import java.util.logging.Logger
import scala.collection.immutable._
import scala.io._
import scala.math._
import gov.sandia.phoenix.propagators.sgp4.SGP4
import scala.language.postfixOps


/**
 */
object TLEUtils {
  val logger = Logger.getLogger(getClass.getName)

  //1440 minutes / 1 revolution
  val MINUTES_PER_REVOLUTION = 1440.0 / (2.0 * Pi) // 229.1831180523293 Actually, this should be minutes / radian.
  /**
   * Attempt to convert 2 character string launch year to 4 digit year.
   */
  def fix2DigitYear(launchYear : String) : Option[Int] = { val ly = launchYear.trim; if (ly.length > 0) Some(fix2DigitYear(ly.toInt)) else None }
  /**
   * Convert 2 digit launch year to 4 digit year (year > 56 is 1900s, else 2000s).
   * This will be an issue in 2056, but I will be retired by then and it isn't my
   * problem in the first place. 
   */
  def fix2DigitYear(launchYear : Int) : Int = if (launchYear > 56) launchYear + 1900 else launchYear + 2000

  /** A regular expression pattern corresponding to: 1 NNNNNC NNNNNAAA NNNNN.NNNNNNNN +.NNNNNNNN +NNNNN-N +NNNNN-N N NNNNN */
  //Relax requirement on position 8 to anything
  //Relax name requirement to non-numeric
  //  val line1Pattern = """(\d .{5}+.{1}+ [\d\s]{2}+[\d\s]{3}+[\w\s]{3}+ \d{2}+[\s\d\.]{12}+ [\s+-0]\.[\d]{8}+ [\s+-][\d\.\s+-]{5}+[+-]\d [\s+-][\d\.\s+-]{5}+[+-]\d [\d\s]{1}+ [\d\s]{4}+\d?)\s*""".r
  /**A regular expression pattern corresponding to: 2 NNNNN NNN.NNNN NNN.NNNN NNNNNNN NNN.NNNN NNN.NNNN NN.NNNNNNNNNNNNNN*/
  //  val line2Pattern = """(\d .{5}+ [\d\.\s+-]{8}+ [\d\.\s+-]{8}+ [\d\s+-]{7}+ [\d\.\s+-]{8}+ [\d\.\s+-]{8}+ [\s\d]{2}+\.\d{8}+[\s\d]{5}+\d?)\s*""".r
  /**A regular expression with extraction corresponding to: 1 NNNNNC NNNNNAAA NNNNN.NNNNNNNN +.NNNNNNNN +NNNNN-N +NNNNN-N N NNNNN*/
  //Relax requirement on position 8 to anything
  //Relax name requirement to non-numeric
  val line1Extractor = """((\d) (.{5}+)(.{1}+) ([\d\s]{2}+)([\d\s]{3}+)([\w\s]{3}+) (\d{2}+)([\s\d\.]{12}+) ([\s+-0]\.[\d]{8}+) ([\s+-][\d\.\s+-]{5}+)([+-]\d) ([\s+-][\d\.\s+-]{5}+)([+-]\d) ([\d\s]{1}+) ([\d\s]{4}+)(\d?))\s*""".r
  /**A regular expression with extraction corresponding to: 2 NNNNN NNN.NNNN NNN.NNNN NNNNNNN NNN.NNNN NNN.NNNN NN.NNNNNNNNNNNNNN*/
  val line2Extractor = """((\d) (.{5}+) ([\d\.\s+-]{8}+) ([\d\.\s+-]{8}+) ([\d\s+-]{7}+) ([\d\.\s+-]{8}+) ([\d\.\s+-]{8}+) ([\s\d]{2}+\.\d{8}+)([\s\d]{5}+)(\d?))\s*""".r
  // 2 NNNNN NNN.NNNN NNN.NNNN NNNNNNN NNN.NNNN NNN.NNNN NN.NNNNNNNNNNNNNN
  //2 00005  34.2682 348.7242 1859667 331.7664  19.3264 10.82419157413667     0.00      4320.0        360.00
  //  val L2 = """(\d [\d]{5}+ [\d\.\s+-]{8}+ [\d\.\s+-]{8}+ [\d\s+-]{7}+ [\d\.\s+-]{8}+ [\d\.\s+-]{8}+ [\s\d]{2}+\.\d{8}+[\s\d]{5}+\d?)"""
  //  val dbl = """([+-]?(?:(?:\d+\.*\d*)|(?:\d*\.*\d+)))"""
  //  val line2TestExtractor = (L2 + "\\s*" + dbl + "\\s*" + dbl + "\\s*" + dbl).r
  val comment = """#.*""".r
  val line1Regex = line1Extractor.pattern
  val line2Regex = line2Extractor.pattern

  /** Difference between the actual checksum (last character) and the computed checksum. */
  def checksumError(line : String) = (-checkSumDigit(line.last) /: line.substring(0, line.length - 1))(_ + checkSumDigit(_)) % 10

  /** Checksum for the first n-1 characters of the TLE line. Should be the same as character n. */
  def checksum(line : String) = if (line.length < 2) 0 else {
    (line.substring(0, line.length - 1) map { checkSumDigit }).sum
  } % 10

  /** Pad line to LINE_LENGTH characters with 0s. Also truncates anything longer than LINE_LENGTH. */
  def pad(line : String) = if (line.length == TLEConstants.LINE_LENGTH) line else
    new String(Array.tabulate[Char](TLEConstants.LINE_LENGTH) { i => if (i < line.length) line.charAt(i) else '0' })

  /** Modifies line to have correct line length and checksum. */
  def fixline(line : String) : String = if (line.length != TLEConstants.LINE_LENGTH)
    fixline(pad(line)) else line.substring(0, TLEConstants.LINE_LENGTH - 1) + checksum(line)

  /** Compute the TLE checksum value for a given character. */
  def checkSumDigit(c : Char) = if (c.isDigit) c - '0' else if (c == '-') 1 else 0

  def apply(name : String, line1 : String, line2 : String) = TLE(Some(name), line1, line2)

  final def apply(is : InputStream) : Map[String, List[SGP4]] = parse(Source.fromInputStream(is).getLines().toList)
  final def apply(url : URL) : Map[String, List[SGP4]] = this(url.openStream)
  final def apply(uri : URI) : Map[String, List[SGP4]] = this(uri.toURL)
  final def apply(file : File) : Map[String, List[SGP4]] = this(file.toURI)

  /** Loads a list of TLEs from a raw string. */
  final def extract(string : String) : List[TLE] = {
    extract(string.replaceAll("\r\n$", "\n").replaceAll("\\^M$", "\n").split("\n").toList)
  }
  /** Loads a list of TLEs from a given InputStream. */
  final def extract(is : InputStream) : List[TLE] = {
    extract(Source.fromInputStream(is).getLines().toList.map(string => string.replaceAll("\r\n$", "\n")).map(string => string.replaceAll("\\^M$", "")))
  }
  /** Loads a list of TLEs from a given URL. */
  final def extract(url : URL) : List[TLE] = extract(url.openStream)
  /** Loads a list of TLEs from a given URI. */
  final def extract(uri : URI) : List[TLE] = extract(uri.toURL)
  /** Loads a list of TLEs from a given File. */
  final def extract(file : File) : List[TLE] = extract(file.toURI)

  final def toArray(list : List[TLE]) = list.toArray

  final def extract(lines : List[String],
                    name : Option[String] = None,
                    line1 : Option[String] = None,
                    line2 : Option[String] = None,
                    res : List[TLE] = Nil) : List[TLE] = lines match {
    case Nil => res.reverse
    case "" :: tail => extract(tail, None, None, None, res)
    case LINE1(line) :: tail => extract(tail, name, Some(line), None, res)
    case LINE2(line) :: tail => line1 match {
      case Some(l1) => {
        val tle = TLE(name, line1.get, line)
        extract(tail, None, None, None, tle :: res)
      }
      case None => {
        logger.severe("TLE line: " + line + " does not have a corresponding line 1.")
        extract(tail, None, None, None, res)
      }
    }
    case head :: tail => extract(tail, Some(head.trim), None, None, res)
  }

  //  final def parse(lines : List[String]) = extract(lines) map { tle => SGP4(tle) } groupBy { _.getNoradNumber }
  final def parse(lines : List[String]) = extract(lines) groupBy { _.noradID } mapValues { tles => tles map { SGP4(_) }}
}
