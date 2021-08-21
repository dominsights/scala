package patmat

class HuffmanSuite extends munit.FunSuite:
  import Huffman.*

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  test("weight of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(weight(t1), 5)
  }

  test("times a char appears in text") {
    val expected: List[(Char, Int)] = List(('a', 2), ('b', 1))
    assertEquals(expected, times(List('a', 'b', 'a')))
  }

  test("ordered leaf list") {
    val frequencies = times(List('a', 'b', 'a'))
    val expected: List[Leaf] = List(Leaf('b', 1), Leaf('a', 2))
    assertEquals(makeOrderedLeafList(frequencies), expected)
  }

  test("singleton") {
    val leaf = List(Leaf('a', 2))
    assert(singleton(leaf))
  }

  test("not a singleton") {
    val leafs = List(Leaf('a', 2), Leaf('b', 1))
    assert(!singleton(leafs))
  }

  test("combine 3 leafs") {
    val leafs = List(Leaf('a', 1), Leaf('b', 2), Leaf('c', 3))
    val left = Leaf('a', 1)
    val right = Leaf('b', 2)
    val fork = Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))
    assertEquals(combine(leafs), List(fork, Leaf('c', 3)))
  }

  test("create code tree") {
    val leftL = Leaf('a', 1)
    val rightL = Leaf('b', 2)
    val fork = Fork(leftL, rightL, chars(leftL) ::: chars(rightL), weight(leftL) + weight(rightL))
    val right = Leaf('c', 3)
    val expected = Fork(fork, right, chars(fork) ::: chars(right), weight(fork) + weight(right))
    assertEquals(createCodeTree(List('a', 'b', 'b', 'c', 'c', 'c')), expected)
  }

  test("decode word") {
    val tree = createCodeTree(List('f', 'a', 'd', 'a'))
    val bits = List[Bit](0,0,1,0,1,1)
    assertEquals(decode(tree, bits), List[Char]('f', 'a', 'd', 'a'))
  }

  test("chars of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(chars(t2), List('a','b','d'))
  }

  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees:
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
