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

/**
 * [[http://tdc-www.harvard.edu/software/catalogs/catalogsb.html Binary File Format]]
 */
case class HipparcosCatalogHeader(STAR0 : Int, STAR1 : Int, STARN : Int, STNUM : Int, MPROP : Int, NMAG : Int, NBENT : Int) {
  def pretty = "STAR0: " + STAR0 + "\n" + "STAR1: " + STAR1 + "\n" + "STARN: " + STARN + "\n" + "STNUM: " + STNUM + "\n" +
    "MPROP: " + MPROP + "\n" + "NMAG: " + NMAG + "\n" + "NBENT: " + NBENT + "\n"
}