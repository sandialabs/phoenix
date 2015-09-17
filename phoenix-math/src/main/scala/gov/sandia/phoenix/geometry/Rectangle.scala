package gov.sandia.phoenix.geometry

import scala.math._

case class Rectangle(minX : Double, minY : Double, width : Double, height : Double) {
  val maxX = minX + width
  val maxY = minY + height
  val cx = minX + width * 0.5
  val cy = minY + height * 0.5

  def shift(Δx : Double, Δy : Double) = new Rectangle(minX + Δx, minY + Δy, width, height)
  def rebase(x : Double, y : Double) = new Rectangle(x, y, width, height)
  def centerAt(x : Double, y : Double) = new Rectangle(x - width * 0.5, y - height * 0.5, width, height)
  def scale(Δ : Double) = if(Δ <= 0) this else {
    val sx = width * Δ
    val sy = height * Δ
    new Rectangle(cx - sx * 0.5, cy - sy * 0.5, sx, sy)
  }
  def area = width * height
  def aspectRatio = width / height

  def union(that : Rectangle) = {
    val xmin = min(this.minX, that.minX)
    val ymin = min(this.minY, that.minY)
    val xmax = max(this.maxX, that.maxX)
    val ymax = max(this.maxY, that.maxY)

    new Rectangle(xmin, ymin, xmax - xmin, ymax - ymin)
  }

  def intersection(that : Rectangle) = {
    val xmin = max(this.minX, that.minX)
    val ymin = max(this.minY, that.minY)
    val xmax = min(this.maxX, that.maxX)
    val ymax = min(this.maxY, that.maxY)
    val w = xmax - xmin
    val h = ymax - ymin
    if(w < 0 || h < 0) None else Some(new Rectangle(xmin, ymin, w, h))
  }

  def contains(x : Double, y : Double) : Boolean = x >= minX && x <= maxX && y >= minY && y <= maxY
  def contains(p : (Double, Double)) : Boolean = contains(p._1, p._2)
}
