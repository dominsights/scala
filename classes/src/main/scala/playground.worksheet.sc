def contains(nums: Array[Int], x: Int, i: Int): (Boolean, Int) =
  if nums.length == 0 then (false, -1)
  else if nums(0) == x then (true, i)
  else contains(nums.drop(1), x, i + 1)

def twoSum(nums: Array[Int], target: Int): Array[Int] =
  def twoSum_iter(nums: Array[Int], target: Int, i: Int): Array[Int] =
    if nums.length == 0 then Array[Int](-1)
    else
      val left = target - nums(0)
      val tuple = contains(nums, left, i)
      if tuple._1 then Array[Int](i, tuple._2)
      else twoSum_iter(nums.drop(1), target, i + 1)
  twoSum_iter(nums, target, 0)

val nums = Array[Int](1, 2, 3)

contains(nums, 3, 0)
contains(nums, 3, 0)
contains(nums, 4, 0)

val result = twoSum(nums, 4)
result(0)
result(1)

val nums1 = Array[Int](3, 1, 2)
val result1 = twoSum(nums1, 4)
result1(0)
result1(1)

val nums2 = Array[Int](2, 1, 5)
val result2 = twoSum(nums2, 6)
result2(0)
result2(1)

case class Square(width: Double):
  val area = width * width

val square = Square(10)
square.area

def repeatedString(s: String, n: Long): Long = {
    val times = n / s.size
    val left = n % s.size
    val pred = (c: Char) => c == 'a'
    val count = s.filter(pred).size
    val leftCount = s.substring(0, left.intValue).filter(pred).size
    times * count + leftCount
}

repeatedString("aba", 6)
repeatedString("aba", 7)
repeatedString("aba", 9)
repeatedString("aba", 2)
repeatedString("aba", 1)

val s = "aba"
val times = 6 / s.size
val left = 6 % s.size
val leftCount = s.substring(0, left).filter(c => c == 'a').size
val count = s.filter(c => c == 'a').size
times * count + leftCount

s.substring(0, 1L.intValue).filter(c => c == 'a').size
