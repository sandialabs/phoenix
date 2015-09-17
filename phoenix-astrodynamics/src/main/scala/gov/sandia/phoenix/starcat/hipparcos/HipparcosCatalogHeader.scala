package gov.sandia.phoenix.starcat.hipparcos

/**
 * [[http://tdc-www.harvard.edu/software/catalogs/catalogsb.html Binary File Format]]
 */
case class HipparcosCatalogHeader(STAR0 : Int, STAR1 : Int, STARN : Int, STNUM : Int, MPROP : Int, NMAG : Int, NBENT : Int) {
  def pretty = "STAR0: " + STAR0 + "\n" + "STAR1: " + STAR1 + "\n" + "STARN: " + STARN + "\n" + "STNUM: " + STNUM + "\n" +
    "MPROP: " + MPROP + "\n" + "NMAG: " + NMAG + "\n" + "NBENT: " + NBENT + "\n"
}