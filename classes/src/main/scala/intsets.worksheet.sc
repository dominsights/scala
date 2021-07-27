abstract class IntSet:
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet

object IntSet:
  def apply(): IntSet = Empty
  def apply(x: Int): IntSet = Empty.incl(x)
  def apply(x: Int, y: Int): IntSet = Empty.incl(x).incl(y)

object Empty extends IntSet:
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet:
  def contains(x: Int): Boolean =
    if x < elem then left.contains(x)
    else if x > elem then right.contains(x)
    else true

  def incl(x: Int): IntSet =
    if x < elem then NonEmpty(elem, left.incl(x), right)
    else if x > elem then NonEmpty(elem, left, right.incl(x))
    else this

  def union(other: IntSet): IntSet =
    left.union(right).union(other).incl(elem)

val set = NonEmpty(1, Empty, Empty).incl(2)
set.contains(1)

val newSet = IntSet(1,2)
newSet.contains(1)

