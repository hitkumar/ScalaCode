import scala.util.Random

object ForExpression {
  def main(args: Array[String]) = {

    case class Book(title: String, authors: List[String])

    val books = List(
      Book(title = "Effective Java", authors = List("Josh Bloc", "Alvim")),
      Book(title = "Java puzzles", authors = List("Josh Bloc", "Andre")))

    val bookAuthors = (for {
      b <- books
      a <- b.authors
      if a.indexOf("Josh") >= 0
    } yield b.title).distinct

    val bookAuthorsWithFlatMap = books.flatMap { b =>
      if (b.authors.filter(a => a.indexOf("Josh") >= 0).size > 0)
        Some(b.title)
      else
        None
    }

    trait Generator[+S] {
      def generate: S
    }

    def integers = new Generator[Int] {
      def generate: Int = new Random().nextInt()
    }

    integers.generate

    def test[T, S](g1: Generator[T], g2: Generator[S], numTimes: Int = 100)(f: T => Boolean): Unit = {
      println("test")
    }

    test[Int, Int](integers, integers) { x => true }
  }
}
