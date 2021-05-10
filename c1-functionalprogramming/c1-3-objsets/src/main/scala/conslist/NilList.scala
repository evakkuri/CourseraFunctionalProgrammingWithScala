package conslist

object NilList extends List[Nothing]{
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException()

  override def tail: Nothing = throw new NoSuchElementException()
}

object test {
  val x: List[String] = NilList
}