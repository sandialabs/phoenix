package gov.sandia.phoenix.starcat.hipparcos

import java.io.DataInputStream

import gov.sandia.phoenix.geometry.{RaDec, Declination, RightAscension}

object HipparcosCatalogReader {
  def apply(is : DataInputStream) = {
    val header = HipparcosCatalogHeader(is.readInt, is.readInt, is.readInt, is.readInt, is.readInt, is.readInt, is.readInt)
    val entries = for (i <- 0 until header.STARN) yield readEntry(is, header)
    new HipparcosCatalog(header, Set.empty ++ entries)
  }

  private def readEntry(is : DataInputStream, header : HipparcosCatalogHeader) = {
    val catalogNumber = header.STNUM match {
      case 1 => is.readFloat.intValue
      case 4 => is.readInt
      case _ => throw new Exception("Unhandled star number type!")
    }

    val ra = RightAscension.fromRadians(is.readDouble)
    val dec = Declination.fromRadians(is.readDouble)
    val spectralType = new String(Array[Byte](is.readByte, is.readByte))
    val mags = (for(i <- 0 until math.abs(header.NMAG)) yield is.readShort / 100.0f).toArray
    val raProp = if(header.MPROP == 1) is.readFloat.toDegrees else 0.0f
    val decProp = if(header.MPROP == 1) is.readFloat.toDegrees else 0.0f
    val rvkm = if(header.MPROP == 2) is.readDouble else 0.0
    val name = if(header.STNUM < 0) new String((for(i <- 0 until -header.STNUM) yield is.readByte).toArray) else ""

    HipparcosCatalogEntry(header, catalogNumber, RaDec(ra, dec), spectralType, mags, raProp, decProp)
  }
}
