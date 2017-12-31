
trait Expressions {
  def eval: Int = this match {
    case Number(n: Int) => n
    case Sum(e1: Expressions, e2: Expressions) => e1.eval + e2.eval
  }

  def show: String = this match {
    case Number(n: Int) => n.toString
    case Sum(e1: Expressions, e2: Expressions) => e1.show + " + " + e2.show
  }
}

case class Number(n: Int) extends Expressions

case class Sum(e1: Expressions, e2: Expressions) extends Expressions

object testingExpressions {
  def main(args: Array[String]) = {
    val num = Number(1)
    assert(num.eval == 1)
    val sum = new Sum(num, new Number(3))
    assert(sum.eval == 4)

    println(num.show)
    println(sum.show)

    val n = 8
    val a = for {
      i <- 1 until n
      j <- 1 until i
      if i + j >= 9
    } yield i + j
    println("a is " + a)

    val b = (1 until n).flatMap { i =>
      (1 until i).map { j =>
        (i + j)
      }
    }

    println("b is " + b.sum)

    val list1 = List(1, 2, 3)
    val list2 = 1 :: 2 :: 3 :: Nil
    assert(list1.head == list2.head)
    assert(isort(List(3,2,1)).equals(List(1,2,3)))
    println(isort(List(-1,-2,-3)))
    println(Map(("a" -> 1)))
  }

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List[Int]()
    case y :: ys => insert(y, isort(ys))
  }

  private def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y::ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }
}