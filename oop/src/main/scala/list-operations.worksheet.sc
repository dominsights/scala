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
// partitionMap ...