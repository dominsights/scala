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