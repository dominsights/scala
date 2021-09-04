import java.{util => ju}
abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](item: B): MyList[B]
  def printElements: String
  // higher-order functions
  def map[B](transformer: Function1[A, B]): MyList[B]
  def flatMap[B](transformer: Function1[A, MyList[B]]): MyList[B]
  def filter(predicate: Function1[A, Boolean]): MyList[A]
  def ++[B >: A](list: MyList[B]): MyList[B]
  override def toString: String = "[" + printElements + "]"
  def foreach(f: A => Unit): Unit
  def sort(f: (A, A) => Int): MyList[A]
  def zipWith[B,C](list: MyList[B], zip: (A, B) => C): MyList[C]
  def fold[B](acc: B)(f: (B, A) => B): B
}

case object Empty extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing](item: B): MyList[B] = Cons(item, Empty)
  def map[B](transformer: Function1[Nothing, B]): MyList[B] = Empty
  def flatMap[B](transformer: Function1[Nothing, MyList[B]]): MyList[B] =
    Empty
  def filter(predicate: Function1[Nothing, Boolean]): MyList[Nothing] = Empty
  def ++[B >: Nothing](list: MyList[B]): MyList[B] = list
  override def printElements = ""
  def foreach(f: Nothing => Unit): Unit = ()
  def sort(f: (Nothing, Nothing) => Int): MyList[Nothing] = this
  def zipWith[B,C](list: MyList[B], zip: (Nothing, B) => C): MyList[C] = 
    if !list.isEmpty then throw RuntimeException("Lists do not have the same length.")
    else Empty
  def fold[B](acc: B)(f: (B, Nothing) => B): B = acc
}

case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  def head: A = h
  def tail: MyList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](item: B): MyList[B] = Cons(item, this)
  def map[B](transformer: Function1[A, B]): MyList[B] =
    Cons(transformer(h), t.map(transformer))
  def flatMap[B](transformer: Function1[A, MyList[B]]): MyList[B] =
    transformer(h) ++ t.flatMap(transformer)
  def filter(predicate: Function1[A, Boolean]): MyList[A] =
    if predicate(h) then Cons(h, t.filter(predicate))
    else t.filter(predicate)
  override def printElements: String =
    if t.isEmpty then "" + h
    else s"${h} ${t.printElements}"
  def ++[B >: A](list: MyList[B]): MyList[B] = Cons(h, t ++ list)
  def foreach(f: A => Unit): Unit = 
    f(h)
    t.foreach(f)
  def sort(compare: (A, A) => Int): MyList[A] =
    def insert(x: A, sortedList: MyList[A]): MyList[A] =
      if sortedList.isEmpty then new Cons(x, Empty)
      else if compare(x, sortedList.head) <= 0 then new Cons(x, sortedList)
      else new Cons(sortedList.head, insert(x, sortedList.tail))
    
    val sortedTail = t.sort(compare)
    insert(h, sortedTail)

  def zipWith[B,C](list: MyList[B], f: (A, B) => C): MyList[C] = 
    if list.isEmpty then throw RuntimeException("Lists do not have the same length.")
    else new Cons(f(h, list.head), t.zipWith(list.tail, f))
  def fold[B](acc: B)(f: (B, A) => B): B = 
    t.fold(f(acc, h))(f)
}

trait MyPredicate[-T] { // replaced with Function1
  def test(t: T): Boolean
}

trait MyTransformer[-A, B] { // replaced with Function1
  def transform(a: A): B
}

class Vehicle
class Car extends Vehicle

val list1: MyList[Vehicle] = new Cons[Car](new Car, Empty)
val list2: MyList[Vehicle] = new Cons[Vehicle](new Vehicle, Empty)
// var contravariance: MyList[Car] = new Cons[Vehicle](new Vehicle, Empty) // MyList[-A]
val ints: MyList[Int] = Cons[Int](1, Cons(2, Cons(3, Empty)))

ints.map(elem => elem * 2)

ints.flatMap(elem => Cons(elem, Cons(elem + 1, Empty)))
ints.fold(0)((x, y) => x + y)
val unsorted: MyList[Int] = Cons[Int](3, Cons(2, Cons(1, Empty)))
unsorted.sort((x, y) => x - y)

val concat: ((String, String) => String) = _ + _

concat("1", "2")
  
def adder(num: Int): Function1[Int, Int] = // higher-order function
  return new Function1[Int, Int] {
    override def apply(otherNum: Int): Int =
      num + otherNum
  }

def nicerAdder(num: Int)(otherNum: Int): Int = num + otherNum

val func = adder(1)
func(2)
adder(1)(2) // curry function

val niceFunc = nicerAdder(1)
niceFunc(2)
nicerAdder(1)(2) // curry function

class Animal
class Cat extends Animal
class Dog extends Animal

class List[A]

val list: List[Animal] = new List[Animal] // it only works for the same type

// val list: List[Animal] = new List[Cat] // it won´t work (must be covariant)
class CovariantList[+A] {
  def method[B](a: B): B = a // +A doesn´t allow usage of A in this case
}
val covariantList: CovariantList[Animal] = new CovariantList[Cat]

// val list: List[Cat] = new List[Animal] // it won´t work (must be contravariant)
class ContravariantList[-A]
val contravariantList: ContravariantList[Cat] = new ContravariantList[Animal]

class SubtypeOnly[A] {
  def method[B >: A](x: B): B = x
}

val animal = new Animal
val cat = new Cat
val subtypeOnly = SubtypeOnly[Animal]
// subtypeOnly.method(animal)
subtypeOnly.method(cat)

val numbers = Cons(1, Cons(2, Cons(3, Empty)))
val chars = Cons('a', Cons('b', Cons('c', Empty)))
val colors = Cons("black", Cons("white", Empty))

for {
  n <- numbers
  c <- chars
  color <- colors
} yield "" + n + c + "-" + color

