
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

object testing1 {
  def main(args: Array[String]) = {
    val num = Number(1)
    assert(num.eval == 1)
    val sum = new Sum(num, new Number(3))
    assert(sum.eval == 4)

    println(num.show)
    println(sum.show)
  }
}