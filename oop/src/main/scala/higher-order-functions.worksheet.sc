def nTimes(f: Int => Int, n: Int, x: Int): Int = // this apply the function to x
    if n <= 0 then x
    else nTimes(f, n-1, f(x))

def nTimesBetter(f: Int => Int, n: Int): (Int => Int) = // this return a function which applies the fuction to x only when invoked
    if n <= 0 then (x: Int) => x
    else (x: Int) => nTimesBetter(f, n-1)(f(x))

def curriedFormatter(c: String)(x: Double): String = c.format(x)

val standardFormat = curriedFormatter("%4.2f")
val preciseFormat: (Double => String) = curriedFormatter("%10.8f")

println(standardFormat(Math.PI))
println(preciseFormat(Math.PI))

def toCurry(f: (Int, Int) => Int): Int => Int => Int =
    x => y => f(x, y)

def fromCurry(f: Int => Int => Int): (Int, Int) => Int =
    (x, y) => f(x)(y)

def compose[A,B,T](f: A => B, g: T => A): T => B = 
    x => f(g(x))
def andThen[A,B,C](f: A => B, g: B => C): A => C = 
    x => g(f(x))

def adder: (Int, Int) => Int = (x,y) => x + y
def superAdder = toCurry(adder)
def add4 = superAdder(4)
add4(17)

val simpleAdder = fromCurry(superAdder)
simpleAdder(4, 17)

val add2 = (x: Int) => x + 2
val times3 = (x: Int) => x * 3

val composed = compose(add2, times3)
val ordered = andThen(add2, times3)

composed(4)
ordered(4)
