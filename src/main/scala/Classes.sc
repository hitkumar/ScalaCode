class Rational(x: Int, y: Int) {

  require(y != 0, "denom should be non zero")

  def this(x: Int) = this(x, 1)

  private val g = gcd(x, y)

  def numer = x / g

  def denom = y / g

  def add(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def neg = new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)

  def less(that: Rational) = numer * that.denom < denom * that.numer

  def max(that: Rational) = if (this.less(that)) that else this

  override def toString: String = numer + "/" + denom

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
}

val a = new Rational(1, 2)
val b = new Rational(2, 3)
a.numer
a.denom
a.add(b)
a.sub(b)
a less b

// val strange = new Rational(1, 0)
val rational = new Rational(2)