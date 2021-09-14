val nums = List(1,2,3,4,5)
val chars = List('a', 'b', 'c', 'd', 'e')

nums.zip(chars)
nums.flatMap(n => chars.map(c => (n, c)))
nums.fold(10)(_+_)
nums.foldLeft(10)(_+_)
nums.foldRight(10)(_+_)
nums.mkString
nums.mkString("+")
nums.toString
nums.mkString("(", "+", ")")
nums.padTo(7, 10)
nums.partition(n => n < 4)
val xs = Iterable(1, "one", 2, "two", 3, "three") partitionMap {
    case i: Int => Left(i)
    case s: String => Right(s)
}
val otherNums = List(6,7,8,9)
nums.patch(3, otherNums, 0)

val list = List(1,2,5)
val missing = List(3,4)
list.patch(2, missing, 0)
list.patch(2, missing, 1)

val perm = "aba".permutations
perm.mkString(", ")
