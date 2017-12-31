trait Stream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: Stream[A]
}

object Stream {
  def cons[T](hd: T, tl: =>  Stream[T]) = new Stream[T] {
    // println("head is " + hd + " and tail is " + tl)
    def isEmpty = false
    def head = hd
    lazy val tail = tl
  }

  val empty = new Stream[Nothing] {
    def isEmpty = true
    def head = throw new NoSuchElementException("empty.head")
    def tail = throw new NoSuchElementException("empty.tail")
  }
}


object Streams {
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def main(args: Array[String]) = {
    println("Streams")
    val intStream = Stream.cons(1, Stream.cons(2, Stream.empty))
    println(intStream.head)
    println(intStream.tail.head)

    println(from(12).tail)
  }
}
