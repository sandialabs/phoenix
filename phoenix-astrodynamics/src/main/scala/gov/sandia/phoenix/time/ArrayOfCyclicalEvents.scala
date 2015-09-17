package gov.sandia.phoenix.time



/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
abstract class ArrayOfCyclicalEvents[T](val values : Vector[T]) {
  val count = values.length
  val first = this(0)
  val last = this(count - 1)
  def apply(index : Int) = values(index % count)
  def next(index : Int) = this((index + 1) % count)
  def previous(index : Int) = if(index == 0) last else this(index - 1)
}