package gov.sandia.phoenix.linalg

class RowVector(d : IndexedSeq[Double]) extends Matrix(1, d.length, d){
  def this(d : Double*) = this(d.toIndexedSeq)

  override def unary_~ = new ColumnVector(data)
}

object RowVector {
  def apply(a : IndexedSeq[Double]) = new RowVector(a)
  def apply(a : Double*) = new RowVector(a.toIndexedSeq)
}