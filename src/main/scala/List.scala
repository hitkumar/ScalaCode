
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
}

object testing {

  def singleton[T](elem: T): List[T] = new Cons[T](elem, new Nil[T])

  def nth[T](n: Int, xs: List[T]): T = {
    if (xs.isEmpty) throw new IndexOutOfBoundsException
    if (n == 0) xs.head
    else nth(n - 1, xs.tail)
  }

  def main(args: Array[String]): Unit = {
    val a = new Cons[Int](1, new Nil[Int])
    val d = new Cons[Int](2, a)
    val e = new Cons[Int](3, d)
    val b = singleton(1)
    val c = singleton(true)
    assert(b.head == 1)
    assert(c.head == true)
    assert(nth(1, e) == 2)

    try {
      nth(5, e)
      assert(false)
    } catch {
      case _: IndexOutOfBoundsException  => println("expected exception")
    }
  }
}
