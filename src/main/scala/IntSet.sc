abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  override def toString: String = "."
  def union(other: IntSet) = other
}

class NonEmpty(num: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < num)
      left contains x
    else if (x > num)
      right contains x
    else
      true

  def incl(x: Int): IntSet =
    if (x < num)
      new NonEmpty(num, left incl x, right)
    else if (x > num)
      new NonEmpty(num, left, right incl x)
    else
      this

  def union(other: IntSet): IntSet =
    ((left union right) union other) incl num

  override def toString: String = "{ " + left + num + right + "}"
}

trait Planar {
  def height: Double
  def width: Double
  def area = height * width
}

def error(msg: String) = throw new Error(msg)

val a = new NonEmpty(1, Empty, Empty)
a incl 2 incl -1
a incl 3
a incl 4