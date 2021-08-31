import java.{util => ju}
abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](item: B): MyList[B]
  def printElements: String
  override def toString: String = "[" + printElements + "]"
}

object Empty extends MyList[Nothing] {
    def head: Nothing = throw new NoSuchElementException
    def tail: MyList[Nothing] = throw new NoSuchElementException
    def isEmpty: Boolean = true
    def add[B >: Nothing](item: B): MyList[B] = Cons(item, Empty)
    override def printElements = ""
}

class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
    def head: A = h
    def tail: MyList[A] = t
    def isEmpty: Boolean = false
    def add[B >: A](item: B): MyList[B] = Cons(item, this)
    override def printElements: String =
        if t.isEmpty then "" + h
        else s"${h} ${t.printElements}"
}

class Vehicle
class Car extends Vehicle

val list1: MyList[Vehicle] = new Cons[Car](new Car, Empty)
val list2: MyList[Vehicle] = new Cons[Vehicle](new Vehicle, Empty)
// var contravariance: MyList[Car] = new Cons[Vehicle](new Vehicle, Empty) // MyList[-A]


