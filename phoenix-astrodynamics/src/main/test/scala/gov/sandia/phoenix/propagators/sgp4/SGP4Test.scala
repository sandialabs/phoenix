package gov.sandia.phoenix.propagators.sgp4

import org.scalatest.FunSuite
import gov.sandia.phoenix.elements.tle.TLE
import java.util.Scanner
import scala.math._

/**
 * These test cases correspond to the ones from "Revisiting Space Track Report #3."
 */
class SGP4Test extends FunSuite {
  test("GEO Period") {
    //LES 9 - This is from a set of geostationary sats provided by celestrak.
    val line1 = "1 08747U 76023B   07133.46587318 -.00000026  00000-0  10000-3 0  7845"
    val line2 = "2 08747  10.4298 170.9168 0025173 288.7966 191.3002  1.00270377 59746"
    val sgp4 = SGP4(TLE(None, line1, line2))
    val periodSec = 60.0 * 2.0 * Pi / sgp4.elements.no
    val periodHr = periodSec / (60 * 60)
    val siderealDayMS = 86164091
    val siderealDayHr = siderealDayMS / (60.0 * 60.0 * 1000.0)
    //Geo Orbits are about 24 hours.
    assert(abs(siderealDayHr - periodHr) < 0.0025)
  }

  test("GPS Period") {
    //GPS BIIR-07 (PRN 18) - TLE selected from celestrak samples.
    val line1 = "1 26690U 01004A   07170.00487060 -.00000085  00000-0  10000-3 0  5431"
    val line2 = "2 26690  54.5972 309.7362 0084028 212.4219 147.0520  2.00572269 46800"
    val sgp4 = SGP4(TLE(None, line1, line2))
    val periodSec = 60.0 * 2.0 * Pi / sgp4.elements.no
    val periodHr = periodSec / (60 * 60)
    val siderealDayMS = 86164091
    val siderealDayHr = siderealDayMS / (60.0 * 60.0 * 1000.0)
    val expectedAnswer = siderealDayHr * 0.5
    //GPS Orbits are about 12 hours.
    assert(abs(expectedAnswer - periodHr) < 0.0025)
  }

  test("00005") {
    val line1 = "1 00005U 58002B   00179.78495062  .00000023  00000-0  28098-4 0  4753"
    val line2 = "2 00005  34.2682 348.7242 1859667 331.7664  19.3264 10.82419157413667"
    performSGP4Tests(line1, line2)
  }


  test("04632") {
    val line1 = "1 04632U 70093B   04031.91070959 -.00000084  00000-0  10000-3 0  9955"
    val line2 = "2 04632  11.4628 273.1101 1450506 207.6000 143.9350  1.20231981 44145"

    performSGP4Tests(line1, line2)
  }

  test("06251") {
    val line1 = "1 06251U 62025E   06176.82412014  .00008885  00000-0  12808-3 0  3985"
    val line2 = "2 06251  58.0579  54.0425 0030035 139.1568 221.1854 15.56387291  6774"

    performSGP4Tests(line1, line2)
  }

  test("08195") {
    val line1 = "1 08195U 75081A   06176.33215444  .00000099  00000-0  11873-3 0   813"
    val line2 = "2 08195  64.1586 279.0717 6877146 264.7651  20.2257  2.00491383225656"

    performSGP4Tests(line1, line2)
  }

  test("09880") {
    val line1 = "1 09880U 77021A   06176.56157475  .00000421  00000-0  10000-3 0  9814"
    val line2 = "2 09880  64.5968 349.3786 7069051 270.0229  16.3320  2.00813614112380"

    performSGP4Tests(line1, line2)
  }

  test("09998") {
    val line1 = "1 09998U 74033F   05148.79417928 -.00000112  00000-0  00000+0 0  4480"
    val line2 = "2 09998   9.4958 313.1750 0270971 327.5225  30.8097  1.16186785 45878"

    performSGP4Tests(line1, line2)
  }

  test("11801") {
    val line1 = "1 11801U          80230.29629788  .01431103  00000-0  14311-1      13"
    val line2 = "2 11801  46.7916 230.4354 7318036  47.4722  10.4117  2.28537848    13"

    performSGP4Tests(line1, line2)
  }

  test("14128") {
    val line1 = "1 14128U 83058A   06176.02844893 -.00000158  00000-0  10000-3 0  9627"
    val line2 = "2 14128  11.4384  35.2134 0011562  26.4582 333.5652  0.98870114 46093"

    performSGP4Tests(line1, line2)
  }

  test("16925") {
    val line1 = "1 16925U 86065D   06151.67415771  .02550794 -30915-6  18784-3 0  4486"
    val line2 = "2 16925  62.0906 295.0239 5596327 245.1593  47.9690  4.88511875148616"

    performSGP4Tests(line1, line2)
  }

  test("20413") {
    val line1 = "1 20413U 83020D   05363.79166667  .00000000  00000-0  00000+0 0  7041"
    val line2 = "2 20413  12.3514 187.4253 7864447 196.3027 356.5478  0.24690082  7978"

    performSGP4Tests(line1, line2)
  }

  test("21897") {
    val line1 = "1 21897U 92011A   06176.02341244 -.00001273  00000-0 -13525-3 0  3044"
    val line2 = "2 21897  62.1749 198.0096 7421690 253.0462  20.1561  2.01269994104880"

    performSGP4Tests(line1, line2)
  }

  test("22312") {
    val line1 = "1 22312U 93002D   06094.46235912  .99999999  81888-5  49949-3 0  3953"
    val line2 = "2 22312  62.1486  77.4698 0308723 267.9229  88.7392 15.95744531 98783"

    performSGP4Tests(line1, line2)
  }

  test("22674") {
    val line1 = "1 22674U 93035D   06176.55909107  .00002121  00000-0  29868-3 0  6569"
    val line2 = "2 22674  63.5035 354.4452 7541712 253.3264  18.7754  1.96679808 93877"

    performSGP4Tests(line1, line2)
  }


  test("23177") {
    val line1 = "1 23177U 94040C   06175.45752052  .00000386  00000-0  76590-3 0    95"
    val line2 = "2 23177   7.0496 179.8238 7258491 296.0482   8.3061  2.25906668 97438"

    performSGP4Tests(line1, line2)
  }

  test("23333") {
    val line1 = "1 23333U 94071A   94305.49999999 -.00172956  26967-3  10000-3 0    15"
    val line2 = "2 23333  28.7490   2.3720 9728298  30.4360   1.3500  0.07309491    70"

    performSGP4Tests(line1, line2)
  }

  test("23599") {
    val line1 = "1 23599U 95029B   06171.76535463  .00085586  12891-6  12956-2 0  2905"
    val line2 = "2 23599   6.9327   0.2849 5782022 274.4436  25.2425  4.47796565123555"

    performSGP4Tests(line1, line2)
  }

  test("24208") {
    val line1 = "1 24208U 96044A   06177.04061740 -.00000094  00000-0  10000-3 0  1600"
    val line2 = "2 24208   3.8536  80.0121 0026640 311.0977  48.3000  1.00778054 36119"

    performSGP4Tests(line1, line2)
  }

  test("25954") {
    val line1 = "1 25954U 99060A   04039.68057285 -.00000108  00000-0  00000-0 0  6847"
    val line2 = "2 25954   0.0004 243.8136 0001765  15.5294  22.7134  1.00271289 15615"

    performSGP4Tests(line1, line2)
  }

  test("26900") {
    val line1 = "1 26900U 01039A   06106.74503247  .00000045  00000-0  10000-3 0  8290"
    val line2 = "2 26900   0.0164 266.5378 0003319  86.1794 182.2590  1.00273847 16981"

    performSGP4Tests(line1, line2)
  }

  test("26975") {
    val line1 = "1 26975U 78066F   06174.85818871  .00000620  00000-0  10000-3 0  6809"
    val line2 = "2 26975  68.4714 236.1303 5602877 123.7484 302.5767  2.05657553 67521"

    performSGP4Tests(line1, line2)
  }

  test("28057") {
    val line1 = "1 28057U 03049A   06177.78615833  .00000060  00000-0  35940-4 0  1836"
    val line2 = "2 28057  98.4283 247.6961 0000884  88.1964 271.9322 14.35478080140550"

    performSGP4Tests(line1, line2)
  }

  test("28129") {
    val line1 = "1 28129U 03058A   06175.57071136 -.00000104  00000-0  10000-3 0   459"
    val line2 = "2 28129  54.7298 324.8098 0048506 266.2640  93.1663  2.00562768 18443"

    performSGP4Tests(line1, line2)
  }

  test("28350") {
    val line1 = "1 28350U 04020A   06167.21788666  .16154492  76267-5  18678-3 0  8894"
    val line2 = "2 28350  64.9977 345.6130 0024870 260.7578  99.9590 16.47856722116490"

    performSGP4Tests(line1, line2)
  }

  test("28623") {
    val line1 = "1 28623U 05006B   06177.81079184  .00637644  69054-6  96390-3 0  6000"
    val line2 = "2 28623  28.5200 114.9834 6249053 170.2550 212.8965  3.79477162 12753"

    performSGP4Tests(line1, line2)
  }

  test("28626") {
    val line1 = "1 28626U 05008A   06176.46683397 -.00000205  00000-0  10000-3 0  2190"
    val line2 = "2 28626   0.0019 286.9433 0000335  13.7918  55.6504  1.00270176  4891"

    performSGP4Tests(line1, line2)
  }

  test("28872") {
    val line1 = "1 28872U 05037B   05333.02012661  .25992681  00000-0  24476-3 0  1534"
    val line2 = "2 28872  96.4736 157.9986 0303955 244.0492 110.6523 16.46015938 10708"

    performSGP4Tests(line1, line2)
  }

  test("29141") {
    val line1 = "1 29141U 85108AA  06170.26783845  .99999999  00000-0  13519-0 0   718"
    val line2 = "2 29141  82.4288 273.4882 0015848 277.2124  83.9133 15.93343074  6828"

    performSGP4Tests(line1, line2)
  }

  test("29238") {
    val line1 = "1 29238U 06022G   06177.28732010  .00766286  10823-4  13334-2 0   101"
    val line2 = "2 29238  51.5595 213.7903 0202579  95.2503 267.9010 15.73823839  1061"

    performSGP4Tests(line1, line2)
  }

  test("88888") {
    val line1 = "1 88888U          80275.98708465  .00073094  13844-3  66816-4 0    87"
    val line2 = "2 88888  72.8435 115.9689 0086731  52.6988 110.5714 16.05824518  1058"

    performSGP4Tests(line1, line2)
  }

  def performSGP4Tests(line1: String, line2: String) = {
    val tle = TLE(None, line1, line2)
    val sgp4 = SGP4(tle)

    //NumberFormat format = new DecimalFormat("00000")
    //String name = format.format(sgp4.getSatnum()) + ".e"
    val name = tle.noradID + ".e"

    logger.info("Running tests for " + name)
    val is = classOf[SGP4Test].getResourceAsStream("testcases/" + name)
    assert(is.available > 0)
    val scanner = new Scanner(is)
    while (!scanner.nextLine.startsWith("EphemerisTimePosVel")) {}
    scanner.nextLine

    val pattern =
      "\\s*([\\d\\.+-]*)\\s+([\\d\\.+-]*)\\s+([\\d\\.+-]*)\\s+([\\d\\.+-]*)\\s+([\\d\\.+-]*)\\s+([\\d\\.+-]*)\\s+([\\d\\.+-]*)\\s*"
    //1 km * 1E-6 = 1 mm
    val tolerance = 1E-7
    val baseline = new Array[Double](6)
    var i = 0

    while (scanner.hasNextLine) {
      val line = scanner.nextLine
      if (line.matches(pattern)) {
        val lineScanner = new Scanner(line)
        lineScanner.findInLine(pattern)

        val result = lineScanner.`match`
        val since = result.group(1).toDouble
        baseline(0) = result.group(2).toDouble
        baseline(1) = result.group(3).toDouble
        baseline(2) = result.group(4).toDouble
        baseline(3) = result.group(5).toDouble
        baseline(4) = result.group(6).toDouble
        baseline(5) = result.group(7).toDouble

        val pv = new Array[Double](6)
        sgp4.sgp4(since / 60, pv)

        var dp = 0.0
        var dv = 0.0

        for (j <- 0 until 3) {
          val a = pv(j) - baseline(j)
          val b = pv(j + 3) - baseline(j + 3)
          dp = dp + a * a
          dv = dv + b * b
        }

        dp = sqrt(dp)
        dv = sqrt(dv)
        assert(dp < tolerance, "Distances are unequal at t = " + since)
        assert(dv < tolerance, "Velocities are unequal at t = " + since)
        i = i + 1
      }
    }

    logger.info("Ran " + i + " tests for TLE:\n" + line1 + "\n" + line2)
  }
}
