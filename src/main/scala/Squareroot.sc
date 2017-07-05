1 + 2
val a = 12

def abs(x: Double) = if (x <= 0) -x else x

// square root using nested functions
def sqrt(x: Double) = {
  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def isGoodEnough(guess: Double) =
    abs(guess * guess - x) / x < .001

  def improve(guess: Double) =
    (guess + x / guess) / 2

  sqrtIter(1.0)
}

sqrt(2)

sqrt(4)

// Higher order functions
def sumOfInts(h: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0 else h(a) + sumOfInts(h, a + 1, b)

sumOfInts(x => x, 1, 5)
sumOfInts(x => x * x, 1, 5)

// currying

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)
             (a: Int, b: Int): Int =
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

def sum(h: Int => Int) (a: Int, b: Int): Int =
  if (a > b) 0 else h(a) + sum(h)(a + 1, b)

sum(x => x * x) (3, 4)

def product(h: Int => Int) (a: Int, b: Int): Int =
  mapReduce(x => x, (x, y) => x * y, 1)(a, b)

product(x => x * x) (3, 4)

def factorial(n: Int): Int = product(x => x) (1, n)

factorial(5)