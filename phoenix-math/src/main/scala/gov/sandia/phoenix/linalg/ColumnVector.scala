package gov.sandia.phoenix.linalg

class ColumnVector(a : IndexedSeq[Double]) extends Matrix(a.length, 1, a){
  def this(d : Double*) = this(d.toIndexedSeq)
  override def unary_~ = new RowVector(data)
}

object ColumnVector {
  def apply(a : IndexedSeq[Double]) = new ColumnVector(a)
  def apply(a : Double*) = new ColumnVector(a.toIndexedSeq)
}