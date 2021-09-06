val numbers = List(1, 2, 3, 4)
val chars = List('a', 'b', 'c', 'd')
val colors = List("black", "white")

val combinations = numbers.filter(_ % 2 == 0).flatMap(n => chars.map(c => "" + c + n))

val forCombinations = for
  n <- numbers if n % 2 == 0
  c <- chars
  color <- colors
yield "" + c + n + "-" + color

abstract class Maybe[+T] {
    def map[B](f: T => B): Maybe[B]
    def flatMap[B](f: T => Maybe[B]): Maybe[B]
    def filter(p: T => Boolean): Maybe[T]
}

case object MaybeNot extends Maybe[Nothing] {
    def map[B](f: Nothing => B): Maybe[B] = MaybeNot
    def flatMap[B](f: Nothing => Maybe[B]): Maybe[B] = MaybeNot
    def filter(p: Nothing => Boolean): Maybe[Nothing] = MaybeNot
}

case class Just[+T](value: T) extends Maybe[T] {
    def map[B](f: T => B): Maybe[B] = Just(f(value))
    def flatMap[B](f: T => Maybe[B]): Maybe[B] = f(value)
    def filter(p: T => Boolean): Maybe[T] = 
        if p(value) then this
        else MaybeNot
}

val just3 = Just(3)
just3.map(_ * 2)
just3.flatMap(x => Just(x % 2 == 0))
just3.filter(_ % 2 == 0)


// List replication

val num = 3
val arr = List(1,2,3)

val result = for
    i <- arr
    n <- 1 to num
yield i

arr flatMap(x => 1 to num map(_ => x))

// Map key collision

val phoneNumbers = Map("Jim" -> 555, "Dom" -> 123, "JIM" -> 666)
phoneNumbers.map(pair => pair(0).toLowerCase -> pair(1))

// Overly simplified social network
