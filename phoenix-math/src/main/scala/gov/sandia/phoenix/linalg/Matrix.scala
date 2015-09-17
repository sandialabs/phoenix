package gov.sandia.phoenix.linalg

import scala.math._

object Matrix {
  def column(x : Array[Double]) = new ColumnVector(x)
  def row(x : Array[Double]) = new RowVector(x)
  def column(x : Double*) = new ColumnVector(x.toIndexedSeq)
  def row(x : Double*) = new RowVector(x.toIndexedSeq)
  def square(x : Array[Double]) = sqrt(x.length).toInt match {
    case dim if dim * dim == x.length => Some(new Matrix(dim, dim, x))
    case _ => None
  }
  def apply(x : IndexedSeq[IndexedSeq[Double]]) = {
    val rows = x.length
    x(0).length match {
      case columns if columns > 0 => x forall { _.length == columns } match {
        case true => Some(new Matrix(rows, columns, x.flatten))
        case false => None
      }
      case _ => None
    }
    x map { row => row.length }
  }
  def apply(rows : Int, columns : Int, data : IndexedSeq[Double]) =
    if(rows * columns == data.length) Some(new Matrix(rows, columns, data)) else None
}

class Matrix(val rows : Int, val columns : Int, val data : IndexedSeq[Double]){
  def this(a : Array[Array[Double]]) = this(a.length, a(0).length, a.flatten.toIndexedSeq)
  require(rows * columns == data.length, "Incorrect Dimensions for Matrix")
  def apply(row : Int, col : Int) = data(row * columns + col)
  def apply(row : Int) = data.slice(row * columns, (row + 1) * columns)

  override def toString = ((0 until rows) map { row => "[" + this(row).mkString(", ") + "]" }).mkString("\n")

  def invert = !this
  def transpose = ~this

  def * (that : Matrix) = if(this.columns == that.rows) Some {
    new Matrix(this.rows, that.columns, Vector.tabulate(this.rows * that.columns){ i =>
      ((0 until that.rows) map { row => this(i / that.columns, row) * that(row, i % that.columns) }).sum
    })
  } else None

  def norm = sqrt((data map { element => element * element }).sum)

  def closeTo(that : Matrix, tol : Double = 1.0E-10) = (this - that) forall { res => res.norm <= tol }

  def - (that : Matrix) = elementWise(that){ (a, b) => a - b }
  def + (that : Matrix) = elementWise(that){ (a, b) => a + b }

  def elementWise(that : Matrix)(f : (Double, Double) => Double) = if(this.rows == that.rows && this.columns == that.columns)
    Some(new Matrix(rows, columns, (this.data zip that.data) map { case (a, b) => f(a, b) }))
  else None

  def * (scale : Double) : Matrix = new Matrix(rows, columns, data.map(_ * scale))
  def scale(factor : Double) = this * factor
  
  def * (that : Option[Matrix]) : Option[Matrix] = that flatMap { this * _ }
  def - (that : Option[Matrix]) : Option[Matrix] = that flatMap { this - _ }
  def + (that : Option[Matrix]) : Option[Matrix] = that flatMap { this + _ }

  def isSquare = rows == columns
  def unary_~ = new Matrix(columns, rows, Vector.tabulate(columns * rows){ i => this(i % rows, i / rows) })
  def toArray = Array.tabulate(rows, columns){ (i, j) => this(i, j) }

  def LUdecomp(b : ColumnVector, tol : Double = 1.0E-14) = decompose(tol) flatMap { case (a, o) => Some(substitute(a, o, b)) }

  def unary_! = decompose(1.0E-9) map { case (a, o) =>
    new Matrix(rows, columns, Vector.tabulate(rows){ i =>
      substitute(a, o, new ColumnVector(Array.tabulate(rows){ k => if(i == k) 1.0 else 0.0 })).data
    }.flatten).transpose
  }

  private final def decompose(tol : Double) = {
    val a = toArray
    val o = Array.tabulate(rows){ i => i }
    val s = Vector.tabulate(rows){ i => ((0 until rows) map { j => abs(a(i)(j)) }).max }

    val res = (0 until rows - 1) map { k =>
      pivot(a, o, s, k)
      if(abs(a(o(k))(k)) / s(o(k)) < tol) false else {
        for(i <- k + 1 until rows){
          val factor = a(o(i))(k) / a(o(k))(k)
          for(j <- k until rows) { a(o(i))(j) = if(j == k) factor else a(o(i))(j) - factor * a(o(k))(j) }
        }
        true
      }
    }
    val ok = res forall { _ == true }
    if(!ok || abs(a(o(rows - 1))(rows - 1) / s(o(rows - 1))) < tol) None else Some(a, o)
  }

  private final def pivot(a : Array[Array[Double]], o : Array[Int], s : IndexedSeq[Double], k : Int) {
    val p = (k until o.length) maxBy { i => abs(a(o(i))(k) / s(o(i))) }
    val tmp = o(p)
    o(p) = o(k)
    o(k) = tmp
  }

  private final def substitute(a : Array[Array[Double]], o : IndexedSeq[Int], cv : ColumnVector) = {
    val b = cv.data.toArray
    for(i <- 1 until rows){
      b(o(i)) = b(o(i)) - ((0 until i) map { j => a(o(i))(j) * b(o(j)) }).sum
    }

    val x = Array.fill[Double](o.length)(90.9)
    for(i <- rows - 1 to 0 by -1){
      x(i) = (b(o(i)) - ((i + 1 until rows) map { j => a(o(i))(j) * x(j) }).sum) / a(o(i))(i)
    }
    ColumnVector(x)
  }
}


