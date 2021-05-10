package conslist

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new ConsList[U](elem, this)
}

object List {
  def apply[T]() = NilList
  def apply[T](x1: T) = new ConsList[T](x1, NilList)
  def apply[T](x1: T, x2: T) = new ConsList[T](x1, new ConsList[T](x2, NilList))
}
