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

package gov.sandia.phoenix.starcat.hipparcos

import java.io.{DataInputStream, FileInputStream}

import gov.sandia.phoenix.conf.SGConf
import gov.sandia.phoenix.geometry._

import scala.math._

/**
 * [[http://tdc-www.harvard.edu/software/catalogs/hipparcos.html Information on the catalog]]
 * [[http://tdc-www.harvard.edu/software/catalogs/catalogsb.html Binary File Format]]
 * [[http://vizier.u-strasbg.fr/viz-bin/VizieR?-source=I/239/hip_main Look up values]]
 *
 * ===Usage (Scala)===
 * {{{
 * import gov.sandia.phoenix.starcat.hipparcos._
 * import gov.sandia.phoenix.time._
 * import gov.sandia.phoenix.core.util.MathEX._
 * import gov.sandia.phoenix.core._
 * import gov.sandia.phoenix.geometry._
 *
 * val cat = HipparcosCatalog.default
 * //http://en.wikipedia.org/wiki/Theta_Persei
 * val star = cat.entries find { _.catalogNumber == 12777 } get
 * val t = TimeBuilder(2028, 11, 13) plusDays 0.19
 * val radec = star.radec(t)
 * val ra = radec.rightAscension
 * val dec = radec.declination
 * DD2DMS(ra)
 * DD2DMS(dec)
 * val fk5 = t.fk5
 *
 * //This reproduces example 20.b of Meeus. There are small differences, but I
 * //use the zeta, theta, z calcs from Vallado. The differences are tiny.
 * val a = (2 + (44 + 11.986 / 60) / 60.0) / 24.0 * 360
 * val d = 49 + (13.0 + 42.48 / 60) / 60
 * val da = 0.03425 / 240
 * val dd = -0.0895 / 3600
 * val dt = t.toJulianCenturyJ2000 * 100
 * val radechc = new RaDec(a, d)
 * val radechca = new RaDec(radechc.rightAscension + da * dt, radechc.declination + dd * dt)
 * fk5.equatorialReduction(radechca)
 * }}}
 */
object HipparcosCatalog {
  //  val defaultLocation1 = SGConf.getProperty("PHOENIX.home") + "/star_catalogs/hipparcos/hipinputra"
  val defaultLocation = SGConf.getProperty("PHOENIX.home") + "/star_catalogs/hipparcos/hipparcos"
  lazy val default = HipparcosCatalogReader(new DataInputStream(new FileInputStream(defaultLocation)))
}

/**
 * [[http://tdc-www.harvard.edu/software/catalogs/hipparcos.html Information on the catalog]]
 * [[http://tdc-www.harvard.edu/software/catalogs/catalogsb.html Binary File Format]]
 *
 * ===Usage (Scala)===
 * {{{
 * import gov.sandia.phoenix.starcat.hipparcos._
 * import gov.sandia.phoenix.time._
 * import gov.sandia.phoenix.core.util.MathEX._
 * import gov.sandia.phoenix.core._
 * import gov.sandia.phoenix.geometry._
 *
 * val cat = HipparcosCatalog.default
 * //http://en.wikipedia.org/wiki/Theta_Persei
 * val star = cat(12777) get
 * val t = TimeBuilder(2028, 11, 13) plusDays 0.19
 * val radec = star.radec(t)
 * val ra = radec.rightAscension
 * val dec = radec.declination
 *
 * val min = new RaDec(RightAscension.fromDegrees(45.0), Declination.fromDegrees(-23.0))
 * val max = new RaDec(RightAscension.fromDegrees(50.0), Declination.fromDegrees(-17.0))
 * val stars = cat.starsIn(min, max)
 * val starsArray = cat.starsArray(min, max)
 * //Iterate over either of these to get radecs
 * for(star <- stars) println(star.radec(t))
 * for(i <- 0 until starsArray.length) println(starsArray(i).radec(t))
 *
 * //This reproduces example 20.b of Meeus. There are small differences, but I
 * //use the zeta, theta, z calcs from Vallado. The differences are tiny.
 * val a = (2 + (44 + 11.986 / 60) / 60.0) / 24.0 * 360
 * val d = 49 + (13.0 + 42.48 / 60) / 60
 * val da = 0.03425 / 240
 * val dd = -0.0895 / 3600
 * val dt = t.toJulianCenturyJ2000 * 100
 * val radechc = new RaDec(a, d)
 * val radechca = new RaDec(radechc.rightAscension + da * dt, radechc.declination + dd * dt)
 * fk5.equatorialReduction(radechca)
 * }}}
 *
 * ===Plot Big Dipper (Scala)===
 * {{{
 * import gov.sandia.phoenix.geometry._
 * import gov.sandia.phoenix.starcat.hipparcos._
 * import gov.sandia.phoenix.time._
 * import java.awt._
 * import java.awt.geom._
 * import java.awt.BorderLayout
 * import javax.swing._
 * //To run this you must add jfreechart to your path!
 * import org.jfree.chart._
 * import org.jfree.chart.plot._
 * import org.jfree.chart.renderer._
 * import org.jfree.data.xy._
 * import scala.math._
 *
 * val cat = HipparcosCatalog.default
 * val minradec = new RaDec(RightAscension.fromDegrees(11.0 / 24.0 * 360), Declination.fromDegrees(47))
 * val maxradec = new RaDec(RightAscension.fromDegrees(14.0 / 24.0 * 360), Declination.fromDegrees(63))
 * val ursamajor = cat.starsIn(minradec, maxradec)
 * val t = TimeBuilder(2028, 11, 13)
 * val bd = ursamajor filter { _.visualMagnitude < 3.4f }
 * val radecs = for(entry <- bd) yield entry.radec(t)
 * val series = new XYSeries("Ursa Major")
 * for(radec <- radecs) series.add(radec.rightAscension.degrees, cos(radec.declination.radians))
 * val seriesCollection = new XYSeriesCollection(series)
 * val chart = ChartFactory.createPolarChart("Ursa Major", seriesCollection, true, true, true)
 *
 * class PointOnlyRenderer extends DefaultPolarItemRenderer {
 * override def drawSeries(g2 : Graphics2D, dataArea : Rectangle2D, info : PlotRenderingInfo, plot : PolarPlot, dataset : XYDataset, seriesIndex : Int) = {
 * val numPoints = dataset.getItemCount(seriesIndex)
 * for (i <- 0 until numPoints) {
 * val theta = dataset.getXValue(seriesIndex, i)
 * val radius = dataset.getYValue(seriesIndex, i)
 * val p = plot.translateValueThetaRadiusToJava2D(theta, radius, dataArea)
 * val el = new Ellipse2D.Double(p.x, p.y, 5, 5)
 * g2.fill(el)
 * g2.draw(el)
 * }
 * }
 * }
 *
 * val plot = chart.getPlot.asInstanceOf[PolarPlot]
 * plot.setRenderer(new PointOnlyRenderer)
 *
 * val panel = new ChartPanel(chart)
 * val frame = new JFrame("Ursa Major")
 * frame.add(panel, BorderLayout.CENTER)
 * frame.setSize(800, 600)
 * frame.setVisible(true)
 * }}}
 *
 * ===Usage (MATLAB)===
 * {{{
 * % You must change this to point to your phoenix jar.
 * javaaddpath(['/Users/mbastia/PHOENIX-dev/phoenix-core/target/phoenix-core-4.0.0-SNAPSHOT-jar-with-dependencies.jar']);
 *
 * import gov.sandia.phoenix.starcat.hipparcos.*;
 * import gov.sandia.phoenix.time.*;
 * import gov.sandia.phoenix.geometry.*;
 *
 * % Time of interest
 * t = PreciseTime(2028, 11, 13)
 * % The catalog
 * cat = HipparcosCatalog.default;
 *
 * % Compute the location of theta per.
 * % http://en.wikipedia.org/wiki/Theta_Persei
 * star = cat.apply(12777).get
 * radec = star.radec(t)
 * ra = radec.rightAscension
 * dec = radec.declination
 *
 * % Compute the stars in a specified range
 * minradec = RaDec(RightAscension.fromDegrees(45.0), Declination.fromDegrees(-23.0))
 * maxradec = RaDec(RightAscension.fromDegrees(50.0), Declination.fromDegrees(-17.0))
 * starsArray = cat.starsArray(minradec, maxradec)
 * % Compute radecs
 * for i = 1:starsArray.length
 * radecs(i) = starsArray(i).radec(t)
 * end
 * }}}
 */
class HipparcosCatalog(val header : HipparcosCatalogHeader, val entries : Set[HipparcosCatalogEntry]) extends Serializable {
  /**
   * Return all stars in the catalog based on the region specified by the min
   * and max values.
   */
  def starsIn(min : RaDec, max : RaDec) : Set[HipparcosCatalogEntry] = entries filter { entry =>
    entry.meanRaDec.rightAscension.degrees <= max.rightAscension.degrees &&
      entry.meanRaDec.rightAscension.degrees >= min.rightAscension.degrees &&
      entry.meanRaDec.declination.degrees <= max.declination.degrees &&
      entry.meanRaDec.declination.degrees >= min.declination.degrees
  }

  /**
   * Return all stars in the region specified by the point +/- the ranges
   * specified in right ascension and declination.
   */
  def starsIn(point : RaDec, raRange : Double, decRange : Double) : Set[HipparcosCatalogEntry] = entries filter { entry =>
    entry.meanRaDec.rightAscension.degrees <= (point.rightAscension.degrees + raRange) &&
      entry.meanRaDec.rightAscension.degrees >= (point.rightAscension.degrees - raRange) &&
      entry.meanRaDec.declination.degrees <= (point.declination.degrees + decRange) &&
      entry.meanRaDec.declination.degrees >= (point.declination.degrees - decRange)
  }

  /**
   * Return all stars in the region specified by the point +/- the ranges
   * specified in right ascension and declination.
   */
  def starsIn(point : RaDec, range : Double) : Set[HipparcosCatalogEntry] = starsIn(point, range, range)

  /**
   * Return all stars in the fov cone defined by this boresight and fov (degrees).
   */
  def starsInFov(boresight : Vector3, fov : Double) : Set[HipparcosCatalogEntry] = {
    val passValue = cos(fov.toRadians * 0.5)
    entries filter { _.meanRaDec.toECI * boresight >= passValue }
  }

  /**
   * Return all stars in the fov cone defined by this boresight and fov (degrees).
   */
  def starsInFov(radec : RaDec, fov : Double) : Set[HipparcosCatalogEntry] = starsInFov(radec.toECI, fov)

  /**
   * Look up a star in the catalog based on its catalog number.
   */
  def apply(id : Int) = entries find { _.catalogNumber == id }

  def starsArray(min : RaDec, max : RaDec) = starsIn(min, max).toArray
  def starsArray(point : RaDec, raRange : Double, decRange : Double) = starsIn(point, raRange, decRange).toArray
  def starsArray(point : RaDec, range : Double) = starsIn(point, range).toArray
  
  def entriesArray = entries.toArray
}