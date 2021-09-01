import java.{util => ju}
abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](item: B): MyList[B]
  def printElements: String
  def map[B](transformer: MyTransformer[A, B]): MyList[B]
  def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B]
  def filter(predicate: MyPredicate[A]): MyList[A]
  def ++[B >: A](list: MyList[B]): MyList[B]
  override def toString: String = "[" + printElements + "]"
}

object Empty extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing](item: B): MyList[B] = Cons(item, Empty)
  def map[B](transformer: MyTransformer[Nothing, B]): MyList[B] = Empty
  def flatMap[B](transformer: MyTransformer[Nothing, MyList[B]]): MyList[B] = Empty
  def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = Empty
  def ++[B >: Nothing](list: MyList[B]): MyList[B] = list
  override def printElements = ""
}

class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  def head: A = h
  def tail: MyList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](item: B): MyList[B] = Cons(item, this)
  def map[B](transformer: MyTransformer[A, B]): MyList[B] = Cons(transformer.transform(h), t.map(transformer))
  def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B] = transformer.transform(h) ++ t.flatMap(transformer)
  def filter(predicate: MyPredicate[A]): MyList[A] =
    if predicate.test(h) then Cons(h, t.filter(predicate))
    else t.filter(predicate)
  override def printElements: String =
    if t.isEmpty then "" + h
    else s"${h} ${t.printElements}"
  def ++[B >: A](list: MyList[B]): MyList[B] = Cons(h, t ++ list)
}

trait MyPredicate[-T] {
  def test(t: T): Boolean
}

trait MyTransformer[-A, B] {
  def transform(a: A): B
}

class Vehicle
class Car extends Vehicle

val list1: MyList[Vehicle] = new Cons[Car](new Car, Empty)
val list2: MyList[Vehicle] = new Cons[Vehicle](new Vehicle, Empty)
// var contravariance: MyList[Car] = new Cons[Vehicle](new Vehicle, Empty) // MyList[-A]
val ints: MyList[Int] = Cons[Int](1, Cons(2, Empty))

ints.map(new MyTransformer[Int, Int] { 
  override def transform(elem: Int): Int = elem * 2
 })

ints.flatMap(new MyTransformer[Int, MyList[Int]] {
  override def transform(elem: Int): MyList[Int] =
    Cons(elem, Cons(elem + 1, Empty))
})